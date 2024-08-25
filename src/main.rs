use std::env;
use regex::Regex;
use std::fs;
fn main() {

    // get the args and confirm we have a path
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        panic!("Please run the compiler with your .c file")
    }

    // read in the contents
    let file = &args[1];
    let program = fs::read_to_string(file).unwrap();

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
    while position < program.len() {
        let slice = &program[position..];
        let mut matched = false;

        // go through all of the regexs
        for (_, regex) in &token_rx {
            // if we find one
            if let Some(mat) = regex.find(slice) {
                // make sure it is the next element in the input
                if mat.start() == 0 {
                    matched = true;
                    position += mat.end();
                    break;
                }
            }
        }
        if !matched { position += 1; }
    }
}