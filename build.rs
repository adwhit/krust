use std::env;

fn main() {
    let mut dir = env::current_dir().unwrap();
    dir.push("src");
    dir.push("c");
    println!("cargo:rustc-link-search={}", dir.to_str().unwrap());
}
