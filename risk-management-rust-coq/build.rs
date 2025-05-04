// trading-bot-monorepo/risk-management-rust-coq/build.rs

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let proto_dir = "../../proto";

    let protos = [
        format!("{}/risk_management.proto", proto_dir),
        format!("{}/market_data.proto", proto_dir),
        format!("{}/account_state.proto", proto_dir),
        format!("{}/strategy_engine.proto", proto_dir), // Include new strategy_engine proto
        // Add other shared protos needed
    ];

    let out_dir = std::path::Path::new(std::env::var("OUT_DIR")?).join("proto");
    std::fs::create_dir_all(&out_dir)?;

    println!("cargo:rerun-if-changed={}", proto_dir);

    tonic_build::configure()
        // Optional: Configure prost/tonic_build options here
        // .compile_well_known_types(true)
        .out_dir(out_dir)
        .compile(&protos, &[proto_dir])?;

    Ok(())
}