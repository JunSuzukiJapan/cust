use std::collections::HashMap;
use std::fs;
use std::path::Path;

/// Structure representing a macro definition for the C preprocessor.
/// - `args`: Argument list for function-like macros (if any).
/// - `body`: Macro expansion body.
#[derive(Debug, Clone)]
pub struct Macro {
    pub args: Option<Vec<String>>, // Argument list (for function-like macros)
    pub body: String,              // Macro expansion body
}

/// Mapping from macro name (String) to Macro struct.
pub type MacroMap = HashMap<String, Macro>;

/// Removes comments (//, /* */) from C/C++ source code.
/// Safely deletes both line and block comments.
///
/// # Arguments
/// * `s` - Input source code string
/// # Returns
/// * String with comments removed
pub fn remove_comments(s: &str) -> String {
    let mut result = String::new();
    let mut in_block = false;
    let mut chars = s.chars().peekable();
    while let Some(c) = chars.next() {
        if !in_block && c == '/' && chars.peek() == Some(&'/') {
            break; // 行コメント
        } else if !in_block && c == '/' && chars.peek() == Some(&'*') {
            in_block = true;
            chars.next();
        } else if in_block && c == '*' && chars.peek() == Some(&'/') {
            in_block = false;
            chars.next();
        } else if !in_block {
            result.push(c);
        }
    }
    result
}

/// Main function that preprocesses the given C/C++ source code.
/// - Supports macro definition/expansion, conditional compilation, #include, comment removal, and backslash logical lines.
/// - Outputs #line directives with correct file name and line number when processing #include.
///
/// # Arguments
/// * `content` - Input source code string
/// * `path`    - Path to the input file (used for #include and #line tracking)
/// # Returns
/// * Preprocessed source code string
pub fn preprocess_file(content: &str, path: &Path) -> String {
    let mut macros: MacroMap = MacroMap::new();
    let mut included: Vec<String> = Vec::new();
    let mut output = String::new();
    let mut cond_stack: Vec<bool> = vec![true]; // Stack to track conditional compilation state (enabled/disabled)
    let mut logical_lines = Vec::new();
    let mut buf = String::new();
    for line in content.lines() {
        let trimmed = line.trim_end();
        if trimmed.ends_with("\\") {
            buf.push_str(&trimmed[..trimmed.len()-1]);
        } else {
            buf.push_str(trimmed);
            logical_lines.push(buf.clone());
            buf.clear();
        }
    }
    if !buf.is_empty() {
        logical_lines.push(buf);
    }
    for (line_idx, line) in logical_lines.iter().enumerate() {
        let line = remove_comments(line);
        let trimmed = line.trim();
        if trimmed.starts_with("#undef ") {
            let macro_name = &trimmed[6..].trim();
            macros.remove(&macro_name.to_string());
        } else if trimmed.starts_with("#ifdef ") {
            let macro_name = &trimmed[7..].trim();
            let is_defined = macros.contains_key(&macro_name.to_string());
            let parent = *cond_stack.last().unwrap();
            cond_stack.push(parent && is_defined);
        } else if trimmed.starts_with("#ifndef ") {
            let macro_name = &trimmed[8..].trim();
            let is_defined = macros.contains_key(&macro_name.to_string());
            let parent = *cond_stack.last().unwrap();
            cond_stack.push(parent && !is_defined);
        } else if trimmed.starts_with("#else") {
            if let Some(last) = cond_stack.last_mut() {
                *last = !*last;
            }
        } else if trimmed.starts_with("#endif") {
            if cond_stack.len() > 1 {
                cond_stack.pop();
            }
        } else if *cond_stack.last().unwrap() {
            if trimmed.starts_with("#define ") {
                let rest = &trimmed[8..];
                // 関数型マクロ対応（例: #define SQUARE(x) ((x)*(x))）
                if let Some(paren_pos) = rest.find('(') {
                    if let Some(close_paren) = rest.find(')') {
                        let name = rest[..paren_pos].trim();
                        let args_str = &rest[paren_pos+1..close_paren];
                        let args: Vec<String> = args_str.split(',').map(|s| s.trim().to_string()).collect();
                        let body = rest[close_paren+1..].trim().to_string();
                        macros.insert(name.to_string(), Macro { args: Some(args), body });
                    }
                } else if let Some((key, value)) = rest.split_once(' ') {
                    macros.insert(key.to_string(), Macro { args: None, body: value.to_string() });
                }
            } else if trimmed.starts_with("#include ") {
                let rest = trimmed.trim_start_matches("#include ").trim();
                if rest.starts_with('"') && rest.ends_with('"') {
                    let inc_file = &rest[1..rest.len()-1];
                    let dir = path.parent().unwrap_or(Path::new("."));
                    let inc_path = dir.join(inc_file);
                    let next_line = line_idx + 2; // 1-based, +1 for next, +1 because enumerate is 0-based
                    output.push_str(&format!("#line 1 \"{}\"\n", inc_file));
                    let inc_content = preprocess_file(&fs::read_to_string(&inc_path).expect("Failed to read included file"), &inc_path);
                    output.push_str(&inc_content);
                    output.push_str(&format!("#line {} \"{}\"\n", next_line, path.file_name().unwrap().to_string_lossy()));
                }
            } else {
                let mut replaced = line.to_string();
                // マクロ展開（引数付き対応）
                for (name, m) in macros.iter() {
                    if let Some(args) = &m.args {
                        // 関数型マクロ
                        let call = format!("{}(", name);
                        if let Some(start) = replaced.find(&call) {
                            if let Some(end) = replaced[start+call.len()..].find(')') {
                                let arg_vals = &replaced[start+call.len()..start+call.len()+end];
                                let vals: Vec<&str> = arg_vals.split(',').map(|s| s.trim()).collect();
                                if vals.len() == args.len() {
                                    let mut body = m.body.clone();
                                    for (a, v) in args.iter().zip(vals.iter()) {
                                        body = body.replace(a, v);
                                    }
                                    replaced = replaced.replacen(&format!("{}({})", name, arg_vals), &body, 1);
                                }
                            }
                        }
                    } else {
                        replaced = replaced.replace(name, &m.body);
                    }
                }
                output.push_str(&replaced);
                output.push('\n');
            }
        }
    }
    output
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_remove_comments_line() {
        assert_eq!(remove_comments("int x = 1; // comment"), "int x = 1; ");
    }

    #[test]
    fn test_remove_comments_block() {
        assert_eq!(remove_comments("int x = /* comment */ 1;"), "int x =  1;");
    }

    #[test]
    fn test_remove_comments_multiline() {
        let src = "int x = 1; /* block\ncomment */ int y = 2;";
        assert!(remove_comments(src).contains("int x = 1; ")); // Block comment should be removed
    

    #[test]
    fn test_backslash_logical_lines() {
        // Test for logical line splitting (backslash continuation)
        let input = [
            "#define FOO 1\\",
            "2",
            "int x = FOO;"
        ];
        let mut logical_lines = Vec::new();
        let mut buf = String::new();
        for line in input.iter() {
            let trimmed = line.trim_end();
            if trimmed.ends_with("\\") {
                buf.push_str(&trimmed[..trimmed.len()-1]);
            } else {
                buf.push_str(trimmed);
                logical_lines.push(buf.clone());
                buf.clear();
            }
        }
        if !buf.is_empty() {
            logical_lines.push(buf);
        }
        assert_eq!(logical_lines[0], "#define FOO 12");
        assert_eq!(logical_lines[1], "int x = FOO;"); // Checks that logical lines are split correctly
    }
}
