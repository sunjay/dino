use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    lalrpop::process_root()?;

    Ok(())
}
