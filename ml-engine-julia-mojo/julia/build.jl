# ml-engine-julia-mojo/julia/build.jl
# This script compiles .proto files into Julia code using ProtoBuf.jl
# Run this manually or via Pkg.build("MLEngine") if configured in Project.toml

using ProtoBuf
using Pkg

# Ensure ProtoBuf is available
# Pkg.add("ProtoBuf") # Uncomment if running standalone and ProtoBuf isn't installed

# Define paths relative to the build.jl script location
script_dir = @__DIR__
proto_dir = joinpath(script_dir, "..", "..", "proto") # Assumes proto dir is two levels up
output_dir = joinpath(script_dir, "src", "generated") # Output generated Julia files here
proto_file = joinpath(proto_dir, "ml_predictions.proto")
google_proto_dir = joinpath(proto_dir, "google", "protobuf") # Path for google/protobuf/timestamp.proto if needed locally

println("Proto Directory: ", proto_dir)
println("Proto File: ", proto_file)
println("Output Directory: ", output_dir)

if !isdir(proto_dir)
    error("Proto directory not found: $(proto_dir)")
end
if !isfile(proto_file)
    error("Proto file not found: $(proto_file)")
end

# Create output directory if it doesn't exist
mkpath(output_dir)

# Compile the proto file
# The `include_dirs` argument tells protoc where to find imported .proto files
# like google/protobuf/timestamp.proto. ProtoBuf.jl often finds standard ones automatically,
# but explicitly adding the path can help.
try
    ProtoBuf.protoc(`-I=$proto_dir --julia_out=$output_dir $proto_file`)
    println("Successfully compiled $proto_file to $output_dir")
catch e
    println("Error compiling proto file: ", e)
    error("Proto compilation failed.")
end

# Optional: Precompile the package after generating code
# println("Precompiling MLEngine...")
# Pkg.precompile("MLEngine")

println("Build script finished.")
```
**Note:** You need to run this script (e.g., `julia build.jl`) from within the `ml-engine-julia-mojo/julia/` directory *after* creating the `ml_predictions.proto` file. It generates corresponding `.jl` files in `src/generated