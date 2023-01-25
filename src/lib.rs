mod ast;
mod check;
mod ir;

#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum Error {
    #[error("type error: expected {expected}, got {got}")]
    TypeError { expected: check::Ty, got: check::Ty },

    #[error("return type mismatch: returned {first} and then {second}")]
    ReturnTypeMismatch { first: check::Ty, second: check::Ty },

    #[error("undefined variable: {0}")]
    UndefinedVariable(String),

    #[error("dead code after return")]
    DeadCode,

    #[error("wrong number of arguments")]
    ArgumentLen { expected: usize, got: usize },

    #[error("function might or might not return")]
    FunctionIndefiniteReturn,

    #[error("attempted to return from main")]
    ReturnFromMain,

    #[error("unsupported construct for code generation")]
    UnsupportedConstruct,
}

/// Run the source file.
///
/// # Errors
/// If parsing or evaluation fails.
///
pub fn run(source: String) -> anyhow::Result<()> {
    let contents = std::fs::read_to_string(source).expect("should have been able to read the file");
    let prgm = contents.parse::<ast::Prgm>()?;
    for inst in prgm.gen()? {
        println!("{inst}");
    }
    Ok(())
}
