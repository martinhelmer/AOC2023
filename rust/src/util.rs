use std::fs ;

pub fn get_input(f : &str) -> String  {
    let contents = fs::read_to_string(["../input/",f].join(""))
    .expect("Should have been able to read the file");
    contents
}