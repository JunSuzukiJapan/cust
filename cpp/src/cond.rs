//! Conditional-inclusion stack machine (`#if` family).

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CondError {
    pub message: String,
}

impl CondError {
    fn new(message: impl Into<String>) -> Self {
        CondError {
            message: message.into(),
        }
    }
}

#[derive(Debug, Clone)]
struct Group {
    taken: bool,
    else_seen: bool,
    active: bool,
    opened_at: u32,
}

/// How the driver should treat an `#elif` line.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ElifAction {
    /// Evaluate the expression and report back with [`CondStack::elif_end`].
    Evaluate,
    /// Skip it; the branch stays inactive.
    Skip,
}

#[derive(Debug, Clone, Default)]
pub struct CondStack {
    groups: Vec<Group>,
}

impl CondStack {
    pub fn new() -> Self {
        CondStack { groups: Vec::new() }
    }

    /// Whether the current position emits.
    pub fn active(&self) -> bool {
        self.groups.iter().all(|g| g.active)
    }

    /// Whether everything enclosing the innermost group emits. The
    /// driver reads it ahead of `#elif` to tell a taken-branch skip
    /// (which lengthens the resync gap) from an inactive one.
    pub fn enclosing_active(&self) -> bool {
        let n = self.groups.len().saturating_sub(1);
        self.groups[..n].iter().all(|g| g.active)
    }

    /// Opens a group whose controlling condition evaluated to `cond`. The
    /// driver evaluates only when [`CondStack::active`] holds and passes
    /// `false` without evaluating otherwise.
    pub fn open(&mut self, cond: bool, line: u32) {
        let take = self.active() && cond;
        self.groups.push(Group {
            taken: take,
            else_seen: false,
            active: take,
            opened_at: line,
        });
    }

    /// Starts an `#elif` line: structural checks plus the evaluate/skip
    /// decision. The expression is evaluated only when the enclosing groups
    /// emit and no branch was taken yet.
    pub fn elif_begin(&mut self) -> Result<ElifAction, CondError> {
        let enclosing = self.enclosing_active();
        let group = self
            .groups
            .last_mut()
            .ok_or_else(|| CondError::new("#elif without #if"))?;
        if group.else_seen {
            return Err(CondError::new("#elif after #else"));
        }
        if !enclosing || group.taken {
            group.active = false;
            Ok(ElifAction::Skip)
        } else {
            Ok(ElifAction::Evaluate)
        }
    }

    /// Reports an evaluated `#elif` condition. Only called after `Evaluate`.
    pub fn elif_end(&mut self, cond: bool) {
        let group = self.groups.last_mut().expect("elif_end follows elif_begin");
        group.active = cond;
        group.taken |= cond;
    }

    pub fn else_(&mut self) -> Result<(), CondError> {
        let enclosing = self.enclosing_active();
        let group = self
            .groups
            .last_mut()
            .ok_or_else(|| CondError::new("#else without #if"))?;
        if group.else_seen {
            return Err(CondError::new("#else after #else"));
        }
        group.else_seen = true;
        group.active = enclosing && !group.taken;
        group.taken = true;
        Ok(())
    }

    pub fn close(&mut self) -> Result<(), CondError> {
        self.groups
            .pop()
            .map(|_| ())
            .ok_or_else(|| CondError::new("#endif without #if"))
    }

    /// Number of open groups. The driver records it when entering a file so
    /// a group left open at that file's end can be reported there.
    pub fn depth(&self) -> usize {
        self.groups.len()
    }

    /// Physical line where the group at stack index `depth` was opened.
    pub fn opener_line(&self, depth: usize) -> Option<u32> {
        self.groups.get(depth).map(|g| g.opened_at)
    }

}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn if_true_emits_if_false_skips() {
        let mut s = CondStack::new();
        s.open(true, 1);
        assert!(s.active());
        s.close().unwrap();
        s.open(false, 3);
        assert!(!s.active());
        s.close().unwrap();
        assert!(s.active());
    }

    #[test]
    fn else_flips_under_active_parent_only() {
        let mut s = CondStack::new();
        // Taken branch: else deactivates.
        s.open(true, 1);
        s.else_().unwrap();
        assert!(!s.active());
        s.close().unwrap();
        // Untaken branch under active parent: else activates.
        s.open(false, 4);
        s.else_().unwrap();
        assert!(s.active());
        s.close().unwrap();
        // Untaken branch under inactive parent: else stays dark.
        s.open(false, 7);
        s.open(false, 8);
        s.else_().unwrap();
        assert!(!s.active());
        s.close().unwrap();
        s.close().unwrap();
    }

    #[test]
    fn elif_first_true_wins_and_rest_skip() {
        let mut s = CondStack::new();
        s.open(false, 1);
        assert_eq!(s.elif_begin().unwrap(), ElifAction::Evaluate);
        s.elif_end(false);
        assert!(!s.active());
        assert_eq!(s.elif_begin().unwrap(), ElifAction::Evaluate);
        s.elif_end(true);
        assert!(s.active());
        assert_eq!(s.elif_begin().unwrap(), ElifAction::Skip);
        assert!(!s.active());
        s.else_().unwrap();
        assert!(!s.active());
        s.close().unwrap();
    }

    #[test]
    fn elif_under_inactive_parent_skips_without_evaluating() {
        let mut s = CondStack::new();
        s.open(false, 1);
        s.open(false, 2);
        assert_eq!(s.elif_begin().unwrap(), ElifAction::Skip);
        assert!(!s.active());
        s.else_().unwrap();
        assert!(!s.active());
        s.close().unwrap();
        s.close().unwrap();
    }

    #[test]
    fn nesting_composes() {
        let mut s = CondStack::new();
        s.open(true, 1);
        s.open(true, 2);
        assert!(s.active());
        s.close().unwrap();
        s.open(false, 4);
        assert!(!s.active());
        s.close().unwrap();
        assert!(s.active());
        s.close().unwrap();
        assert!(s.active());
    }

    #[test]
    fn structural_errors() {
        let mut s = CondStack::new();
        assert_eq!(s.elif_begin().unwrap_err().message, "#elif without #if");
        assert_eq!(s.else_().unwrap_err().message, "#else without #if");
        assert_eq!(s.close().unwrap_err().message, "#endif without #if");

        s.open(true, 10);
        s.else_().unwrap();
        assert_eq!(s.else_().unwrap_err().message, "#else after #else");
        assert_eq!(s.elif_begin().unwrap_err().message, "#elif after #else");
        s.close().unwrap();

        s.open(true, 20);
        assert_eq!(s.depth(), 1);
        assert_eq!(s.opener_line(0), Some(20));
        assert_eq!(s.opener_line(1), None);
        s.close().unwrap();
        assert_eq!(s.depth(), 0);
    }
}
