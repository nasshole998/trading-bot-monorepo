fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Assume .proto files are located in a 'proto' directory at the monorepo root.
    // Adjust this path if your monorepo structure differs.
    let proto_dir = "../proto";

    tonic_build::configure()
        .build_server(true) // We need a server to receive execution requests
        .build_client(true) // We need a client to send market data streams and order updates
        // Set the path where Tonic will output the generated Rust code relative to crate root
        .out_dir("src/")
        .compile(
            &[
                format!("{}/market_data.proto", proto_dir),
                format!("{}/execution.proto", proto_dir),
            ],
            &[proto_dir], // Include the directory containing the .proto files
        )?;

    // Optional: Rerun build if .proto files change
    println!("cargo:rerun-if-changed={}/market_data.proto", proto_dir);
    println!("cargo:rerun-if-changed={}/execution.proto", proto_dir);

    Ok(())
}