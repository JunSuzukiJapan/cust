//! Wall-clock helpers for the date/time predefined macros.
//!
//! `__DATE__`, `__TIME__` and the fallback `__TIMESTAMP__` describe the
//! translation start in local time; `__TIMESTAMP__` prefers the mtime of the
//! file containing the use. Formats match clang (`"Mmm dd yyyy"`,
//! `"HH:MM:SS"`, ctime).

use std::path::Path;
use std::time::{SystemTime, UNIX_EPOCH};

const MONTHS: [&str; 12] = [
    "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
];
const WEEKDAYS: [&str; 7] = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];

fn break_down(t: libc::time_t) -> libc::tm {
    let mut out: libc::tm = unsafe { std::mem::zeroed() };
    unsafe {
        libc::localtime_r(&t, &mut out);
    }
    out
}

fn now_stamp() -> libc::time_t {
    unsafe { libc::time(std::ptr::null_mut()) }
}

fn system_time_stamp(t: SystemTime) -> Option<libc::time_t> {
    t.duration_since(UNIX_EPOCH).ok().and_then(|d| d.as_secs().try_into().ok())
}

/// `(date, time, ctime)` for the translation start, each already quoted.
pub fn translation_time() -> (String, String, String) {
    let tm = break_down(now_stamp());
    (format_date(&tm), format_time(&tm), format_ctime(&tm))
}

/// ctime string (already quoted) for a file's mtime, if it can be read.
pub fn file_timestamp(path: &Path) -> Option<String> {
    let mtime = std::fs::metadata(path).ok()?.modified().ok()?;
    let stamp = system_time_stamp(mtime)?;
    Some(format_ctime(&break_down(stamp)))
}

fn month(tm: &libc::tm) -> &'static str {
    MONTHS.get(tm.tm_mon as usize).copied().unwrap_or("???")
}

fn weekday(tm: &libc::tm) -> &'static str {
    WEEKDAYS.get(tm.tm_wday as usize).copied().unwrap_or("???")
}

fn format_date(tm: &libc::tm) -> String {
    format!("\"{} {:2} {:4}\"", month(tm), tm.tm_mday, tm.tm_year + 1900)
}

fn format_time(tm: &libc::tm) -> String {
    format!("\"{:02}:{:02}:{:02}\"", tm.tm_hour, tm.tm_min, tm.tm_sec)
}

fn format_ctime(tm: &libc::tm) -> String {
    format!(
        "\"{} {} {:2} {:02}:{:02}:{:02} {:4}\"",
        weekday(tm),
        month(tm),
        tm.tm_mday,
        tm.tm_hour,
        tm.tm_min,
        tm.tm_sec,
        tm.tm_year + 1900
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn formats_match_clang_shapes() {
        let (date, time, ctime) = translation_time();
        // "Mmm dd yyyy": day space-padded to width 2.
        assert_eq!(date.len(), 13);
        assert!(date.starts_with('"') && date.ends_with('"'));
        assert_eq!(&date[1..4], month(&break_down(now_stamp())));
        // "HH:MM:SS"
        assert_eq!(time.len(), 10);
        assert_eq!(time.chars().filter(|c| *c == ':').count(), 2);
        // ctime: "Www Mmm dd HH:MM:SS yyyy"
        assert_eq!(ctime.len(), 26);
    }

    #[test]
    fn file_timestamp_reads_mtime() {
        let dir = std::env::temp_dir();
        let path = dir.join(format!("cpp-clock-{}.h", std::process::id()));
        std::fs::write(&path, "int x;\n").unwrap();
        let stamp = file_timestamp(&path).expect("mtime readable");
        assert_eq!(stamp.len(), 26);
        std::fs::remove_file(&path).ok();
        assert!(file_timestamp(&path).is_none());
    }
}
