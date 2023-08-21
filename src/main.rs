use std::error::Error;
use std::fs::File;
use std::io::{BufReader, BufWriter, Read, Write};
use std::path::PathBuf;
use std::process::ExitCode;
use svg_hush::{Filter, data_url_filter};

fn main() -> ExitCode {
    let mut args = std::env::args_os().skip(1);
    let input_path = match args.next() {
        Some(p) if p.as_os_str() != "-h" => PathBuf::from(p),
        _ => { usage(); return ExitCode::FAILURE; }
    };
    let output_path = args.next()
        .filter(|out| out.as_os_str() != "-").map(PathBuf::from);

    let mut stdio;
    let mut filein;
    let input: &mut dyn Read = if input_path.as_os_str() != "-" {
        match File::open(&input_path) {
            Ok(f) => {
                filein = f;
                &mut filein
            },
            Err(e) => {
                eprintln!("error: Can't open input file {}: {e}", input_path.display());
                return ExitCode::FAILURE;
            }
        }
    } else {
        stdio = std::io::stdin().lock();
        &mut stdio
    };

    let mut stdout;
    let mut fileout;
    let output: &mut dyn Write = match output_path {
        Some(output_path) => match File::create(&output_path) {
            Ok(f) => {
                fileout = f;
                &mut fileout
            },
            Err(e) => {
                eprintln!("error: Can't open output file {}: {e}", output_path.display());
                return ExitCode::FAILURE;
            },
        },
        None => {
            stdout = std::io::stdout().lock();
            &mut stdout
        },
    };

    let mut filter = Filter::new();
    filter.set_data_url_filter(data_url_filter::allow_standard_images);
    if let Err(e) = filter.filter(BufReader::new(input), BufWriter::new(output)) {
        eprintln!("error: {e}");
        let mut source = e.source();
        while let Some(e) = source {
            eprintln!("    {e}");
            source = e.source();
        }
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}

fn usage() {
    eprintln!("SVG hush {} sanitizer https://lib.rs/svg-hush

Output to stdout:
    svg_hush <input_path>
From stdin
    svg_hush -
Or from file to file
    svg_hush <input_path> <output_path>", env!("CARGO_PKG_VERSION"));
}
