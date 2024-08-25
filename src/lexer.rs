use regex::Regex;

pub fn program_to_tokens(program: String) -> Vec<String> {
    // setup all of our token regex
    let token_rx= vec![
        ("Identifier", Regex::new(r"[a-zA-Z_]\w*\b").unwrap()),
        ("Constant", Regex::new(r"[0-9]+\b").unwrap()),
        ("Open Paren", Regex::new(r"\(").unwrap()),
        ("Close Paren", Regex::new(r"\)").unwrap()),
        ("Open Brace", Regex::new(r"\{").unwrap()),
        ("Close Brace", Regex::new(r"\}").unwrap()),
        ("Semicolon brace", Regex::new(r";").unwrap()),
    ];

    // go through the input
    let mut position = 0;

    let mut tokens = Vec::new();
    while position < program.len() {
        let slice = &program[position..];
        let mut matched = false;

        // go through all of the regexs
        for (_, regex) in &token_rx {
            // if we find one
            if let Some(mat) = regex.find(slice) {
                // make sure it is the next element in the input
                if mat.start() == 0 {
                    tokens.push(mat.as_str().to_string());
                    matched = true;
                    position += mat.end();
                    break;
                }
            }
        }
        if !matched { position += 1; }
    }
    tokens
}