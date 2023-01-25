// use ast::Prgm;
use clap::Parser;

use dwislpyc::run;

/// The slpy programming language.
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// The file to run
    #[clap(value_parser)]
    file: String,
}
fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    run(args.file)
}
