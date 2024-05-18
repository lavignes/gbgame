use std::{
    error::Error,
    fs::File,
    io::{self, Read, Write},
    path::PathBuf,
    process::ExitCode,
};

use clap::Parser;
use tracing::Level;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Args {
    /// ROM file
    rom: PathBuf,

    /// Output file (default: stdout)
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// One of `TRACE`, `DEBUG`, `INFO`, `WARN`, or `ERROR`
    #[arg(short, long, default_value_t = Level::INFO)]
    log_level: Level,
}

fn main() -> ExitCode {
    let args = Args::parse();
    tracing_subscriber::fmt()
        .with_max_level(args.log_level)
        .with_writer(io::stderr)
        .init();

    if let Err(e) = main_real(args) {
        tracing::error!("{e}");
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}

fn main_real(args: Args) -> Result<(), Box<dyn Error>> {
    let mut rom = File::open(args.rom).map_err(|e| format!("cant open file: {e}"))?;
    let mut bytes = Vec::new();

    tracing::trace!("reading");
    rom.read_to_end(&mut bytes)?;

    tracing::trace!("computing checksum");
    let mut header_checksum = 0u8;
    for i in 0x0134..=0x014C {
        header_checksum = header_checksum.wrapping_sub(bytes[i]).wrapping_sub(1);
    }
    bytes[0x014D] = header_checksum;

    let mut output: Box<dyn Write> = match args.output {
        Some(path) => Box::new(
            File::options()
                .write(true)
                .create(true)
                .truncate(true)
                .open(path)
                .map_err(|e| format!("cant open file: {e}"))?,
        ),
        None => Box::new(io::stdout()),
    };
    tracing::trace!("writing");
    output.write_all(&bytes)?;

    Ok(())
}
