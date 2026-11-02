//! Generated WASI 0.3.2 command guest for the §12.11 toolchain evidence.
//!
//! Built from this source with the pinned Rust nightly + wasm32-wasip3
//! target by tools/build-toolchain-guest.sh. It exercises the generated
//! component ABI, argv, the environment and stdout, so the same artifact
//! is a real command-world guest rather than a hand-authored fixture.

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let marker = std::env::var("WMD_TOOLCHAIN").unwrap_or_else(|_| "missing".into());
    println!("toolchain-guest:{}:{}", args.len(), marker);
}
