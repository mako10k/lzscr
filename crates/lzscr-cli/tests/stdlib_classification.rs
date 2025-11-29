use std::io;
use std::path::PathBuf;
use std::process::Command;

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..").join("..")
}

fn run_python(script: &PathBuf) -> io::Result<std::process::Output> {
    let candidates = ["python3", "python"];
    let repo = repo_root();
    let mut last_err: Option<io::Error> = None;
    for candidate in candidates {
        match Command::new(candidate).arg(script).current_dir(&repo).output() {
            Ok(output) => {
                return Ok(output);
            }
            Err(err) => {
                if err.kind() == io::ErrorKind::NotFound {
                    last_err = Some(err);
                    continue;
                }
                return Err(err);
            }
        }
    }
    Err(last_err.unwrap_or_else(|| io::Error::new(io::ErrorKind::NotFound, "python not found")))
}

#[test]
fn stdlib_classification_table_up_to_date() {
    let script = repo_root().join("scripts").join("check_stdlib_classification.py");
    let output = run_python(&script).expect("failed to invoke python interpreter");
    assert!(
        output.status.success(),
        "classification script failed:\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}
