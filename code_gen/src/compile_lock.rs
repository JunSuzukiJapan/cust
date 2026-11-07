//! Process-wide lock serializing every LLVM-touching operation.
//!
//! Each `CodeGen` owns an independent inkwell `Context`, so there is no shared
//! `Context` to collide on. What *is* shared is LLVM's process-global state:
//! `Target::initialize_native()` mutates the global `TargetRegistry` on every
//! `create_jit_execution_engine` call, and MCJIT compilation/execution touches
//! further globals. Driving two of those concurrently from different test
//! threads intermittently crashes (SIGSEGV/SIGABRT) or corrupts the heap, so a
//! whole `CodeGen` holds this from `try_new` until it is dropped: engine
//! creation, IR building, JIT lookup/call, and engine/module teardown.
//!
//! The lock nests on the holding thread: `calc_type_size` spins up a second
//! JIT engine in the middle of a codegen and takes it again, and that inner
//! take is a no-op instead of a deadlock on itself.

use std::sync::{Mutex, MutexGuard};

pub static COMPILE_LOCK: CompileLock = CompileLock(Mutex::new(()));

pub struct CompileLock(Mutex<()>);

thread_local! {
    static HOLDS_COMPILE_LOCK: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

pub struct CompileGuard<'a> {
    _guard: Option<MutexGuard<'a, ()>>,
}

impl CompileLock {
    /// Waits for the lock, unless this thread already holds it — then this
    /// nests rather than waiting for itself. A poisoned mutex still yields
    /// its guard: a test that panicked mid-compile must not fail every test
    /// that runs after it.
    pub fn lock(&self) -> CompileGuard<'_> {
        if self.held_here() {
            return CompileGuard { _guard: None };
        }
        let guard = self.0.lock().unwrap_or_else(|e| e.into_inner());
        HOLDS_COMPILE_LOCK.with(|held| held.set(true));
        CompileGuard { _guard: Some(guard) }
    }

    pub fn held_here(&self) -> bool {
        HOLDS_COMPILE_LOCK.with(|held| held.get())
    }
}

impl Drop for CompileGuard<'_> {
    fn drop(&mut self) {
        if self._guard.is_some() {
            HOLDS_COMPILE_LOCK.with(|held| held.set(false));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::COMPILE_LOCK;

    /// `COMPILE_LOCK` nests on the holding thread and still excludes every
    /// other: a whole codegen holds it across `calc_type_size`'s inner take,
    /// so the inner take must return at once rather than wait for itself —
    /// and dropping it must not release the outer one.
    #[test]
    fn compile_lock_nests_on_the_holding_thread_and_excludes_other_threads() {
        let outer = COMPILE_LOCK.lock();
        assert!(COMPILE_LOCK.held_here());
        // Nested: returns instead of deadlocking, and changes nothing.
        let inner = COMPILE_LOCK.lock();
        drop(inner);
        assert!(COMPILE_LOCK.held_here());
        // Another thread still waits while the outer guard lives ...
        let (tx, rx) = std::sync::mpsc::channel();
        std::thread::spawn(move || {
            let _guard = COMPILE_LOCK.lock();
            tx.send(()).unwrap();
        });
        assert!(rx.recv_timeout(std::time::Duration::from_millis(200)).is_err());
        drop(outer);
        assert!(!COMPILE_LOCK.held_here());
        // ... and acquires it once the outer guard is gone.
        rx.recv_timeout(std::time::Duration::from_secs(10)).unwrap();
    }
}
