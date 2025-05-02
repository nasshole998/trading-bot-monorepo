cmake_minimum_required(VERSION 3.15)
project(indicator_engine CXX)

set(CMAKE_CXX_STANDARD 17) # Use C++17 or later
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF) # Enforce standard compliance

# Enable compiler optimizations and warnings
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -pedantic -O3 -DNDEBUG")

# Enable specific SIMD instruction sets (adjust based on target architecture)
# Example for AVX and FMA:
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -mavx -mfma")


# --- Find Dependencies ---

# Find Protobuf
find_package(Protobuf REQUIRED)
message(STATUS "Using Protobuf: ${Protobuf_LIBRARIES}")

# Find gRPC
find_package(gRPC CONFIG REQUIRED)
message(STATUS "Using gRPC: ${gRPC_LIBRARIES}")

# Find Threads
find_package(Threads REQUIRED)

# Find Eigen (or Blaze)
find_package(Eigen3 REQUIRED)
message(STATUS "Using Eigen3: ${EIGEN3_INCLUDE_DIR}")

# Find CUDA (Required for GPU acceleration demonstration)
find_package(CUDA REQUIRED)
message(STATUS "Using CUDA: ${CUDA_TOOLKIT_ROOT_DIR}")

# Find YAML-CPP for configuration loading
find_package(yaml-cpp REQUIRED)
message(STATUS "Using yaml-cpp: ${YAMLCPP_LIBRARIES}")

# Find Asio for health check server
find_package(asio REQUIRED) # Assumes Asio is installed or found by find_package
message(STATUS "Using Asio: ${asio_LIBRARIES}")

# Find spdlog for logging
find_package(spdlog REQUIRED)
message(STATUS "Using spdlog: ${spdlog_LIBRARIES}")


# --- Protobuf and gRPC Compilation ---
set(PROTO_DIR "${CMAKE_SOURCE_DIR}/../../proto") # Adjust path

# Define the output directory for generated C++ headers
set(PROTO_GENERATED_DIR "${CMAKE_SOURCE_DIR}/include/generated")
file(MAKE_DIRECTORY ${PROTO_GENERATED_DIR})

# List your proto files
set(PROTO_FILES
    "${PROTO_DIR}/market_data.proto"
    "${PROTO_DIR}/indicator_data.proto"
)

# Generate C++ from proto files using Protobuf and gRPC plugins
protobuf_generate_cpp(PROTO_SRCS PROTO_HDRS ${PROTO_FILES}
    PROTOC_INCLUDE_DIRS ${PROTO_DIR} # Tell protoc where to find imported proto files
    IMPORT_DIRS ${PROTO_DIR}
)
grpc_generate_cpp(GRPC_SRCS GRPC_HDRS ${PROTO_FILES}
    GRPC_VER_HEADERS_DIR ${PROTO_GENERATED_DIR}/grpc++ # Output gRPC version headers here
    PROTOC_INCLUDE_DIRS ${PROTO_DIR} # Tell protoc where to find imported proto files
    IMPORT_DIRS ${PROTO_DIR}
)

# Combine generated sources and headers
set(GENERATED_SRCS ${PROTO_SRCS} ${GRPC_SRCS})
set(GENERATED_HDRS ${PROTO_HDRS} ${GRPC_HDRS})


# --- Source Files ---
set(CPP_SRCS
    src/main.cpp
    src/indicator_service_impl.cpp
    src/data_manager.cpp
    src/indicator_calculator.cpp # Keep as placeholder for coordination
    src/indicators/sma.cpp
    src/indicators/rsi.cpp
    src/indicators/macd.cpp
    src/indicators/ema.cpp
    src/utils/conversions.cpp
    src/health_check.cpp # Health check implementation
    # spdlog is header-only, no .cpp needed usually
)

# List CUDA source files
set(CUDA_SRCS
    src/cuda_kernels.cu # CUDA kernel placeholder
)


# --- Add Executable ---
add_executable(indicator_engine ${CPP_SRCS} ${GENERATED_SRCS} ${CUDA_SRCS})

# --- Include Directories ---
target_include_directories(indicator_engine PUBLIC
    ${CMAKE_SOURCE_DIR}/include         # Your own headers
    ${PROTO_GENERATED_DIR}              # Generated proto headers
    ${Protobuf_INCLUDE_DIRS}            # Protobuf includes
    ${gRPC_INCLUDE_DIRS}                # gRPC includes
    ${EIGEN3_INCLUDE_DIR}               # Eigen includes
    ${yaml-cpp_INCLUDE_DIRS}            # yaml-cpp includes
    ${asio_INCLUDE_DIR}                 # Asio includes
    ${CUDA_INCLUDE_DIRS}                # CUDA includes
    ${spdlog_INCLUDE_DIRS}              # spdlog includes
)

# --- Link Libraries ---
target_link_libraries(indicator_engine PUBLIC
    ${Protobuf_LIBRARIES}
    ${gRPC_LIBRARIES}
    ${gRPC_REFLECTION_LIBRARIES} # Optional: Include gRPC reflection for debugging
    ${Threads_LIBRARIES}         # For std::thread, mutex, etc.
    ${yaml-cpp_LIBRARIES}        # yaml-cpp libraries
    ${asio_LIBRARIES}            # Asio libraries
    ${CUDA_LIBRARIES}            # Link CUDA runtime libraries
    ${spdlog_LIBRARIES}          # spdlog libraries
    # Optional: Link Google Test if building tests with main executable (less common)
)

# --- CUDA Specific Flags ---
# Example: Set CUDA architecture (adjust sm_xx based on your target GPU)
set_property(TARGET indicator_engine PROPERTY CUDA_ARCHITECTURES "86") # Example for Ampere

# --- Add Tests ---
enable_testing()
add_executable(indicator_tests tests/indicator_tests.cpp)
target_link_libraries(indicator_tests PUBLIC
    indicator_engine # Link against your main library/executable if needed for tests
    ${Protobuf_LIBRARIES}
    ${gRPC_LIBRARIES}
    ${EIGEN3_INCLUDE_DIR} # Link test executable against dependencies it uses
    ${yaml-cpp_LIBRARIES}
    ${asio_LIBRARIES}
    ${CUDA_LIBRARIES}
    ${spdlog_LIBRARIES} # Link spdlog for tests
    # Add your testing framework here, e.g:
    find_package(GTest CONFIG REQUIRED)
    GTest::gmock_main # Or GTest::gtest_main depending on your test structure
)
# configure_test(indicator_tests ...) # Configure test properties if using CTest