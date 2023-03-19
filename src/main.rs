use std::io;
use std::env;
use std::process;

mod eval;

fn main() -> io::Result<()> {
    let args = env::args().skip(1);
    let equation = args.collect::<String>();

    match eval::eval(equation.clone()) {
        Ok(v) => println!("Eval: {}", v),
        Err(err) => {
            eprintln!("{}", err);
            process::exit(1);
        }
    }

    Ok(())
}
