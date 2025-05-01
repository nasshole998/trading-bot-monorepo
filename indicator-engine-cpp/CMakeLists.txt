# indicator-engine-cpp/CMakeLists.txt
cmake_minimum_required(VERSION 3.16)
project(IndicatorEngineCpp VERSION 0.1.0 LANGUAGES CXX CUDA) # Add CUDA if using GPU

# --- Project Settings ---
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)
set(CMAKE_CXX_EXTENSIONS OFF) # Prefer explicit standard features

# Enable IPO/LTO for release builds for better optimization
set(CMAKE_INTERPROCEDURAL_OPTIMIZATION_RELEASE ON)

# --- Options ---
option(INDICATOR_ENGINE_ENABLE_GPU "Enable GPU (CUDA) acceleration" OFF) # Default OFF
option(INDICATOR_ENGINE_USE_TA_LIB "Use TA-Lib for standard indicators" ON) # Default ON
option(INDICATOR_ENGINE_ENABLE_OMP "Enable OpenMP for CPU parallelism" ON) # Default ON

# --- Find Dependencies ---
find_package(Protobuf REQUIRED)      # Protocol Buffers compiler and libraries
find_package(gRPC REQUIRED)         # gRPC libraries
find_package(Threads REQUIRED)      # Standard threads library
find_package(Eigen3 REQUIRED)       # Eigen for vectorized math

if(INDICATOR_ENGINE_ENABLE_OMP)
  find_package(OpenMP REQUIRED)     # OpenMP for parallelism
endif()

if(INDICATOR_ENGINE_USE_TA_LIB)
  find_package(TALib REQUIRED)      # TA-Lib C library
  # Note: You might need to provide a FindTALib.cmake module or ensure TA-Lib's
  # pkg-config file is discoverable if find_package doesn't work out-of-the-box.
  # Example using pkg-config directly if find_package fails:
  # find_package(PkgConfig REQUIRED)
  # pkg_check_modules(TALIB REQUIRED ta-lib)
endif()

if(INDICATOR_ENGINE_ENABLE_GPU)
  find_package(CUDA REQUIRED)         # CUDA Toolkit
  # Add other CUDA libs like cuBLAS, cuFFT if needed
  message(STATUS "CUDA Found: ${CUDA_FOUND}")
  message(STATUS "CUDA Version: ${CUDA_VERSION}")
  # Ensure CUDA architecture is set (e.g., for your target GPU)
  # set(CMAKE_CUDA_ARCHITECTURES "75;86") # Example: Turing, Ampere
endif()

# Logging library (example: spdlog - header only or compiled)
# Option 1: FetchContent (CMake 3.11+) - Downloads and builds if not found
include(FetchContent)
FetchContent_Declare(
  spdlog
  GIT_REPOSITORY https://github.com/gabime/spdlog.git
  GIT_TAG        v1.14.1 # Use a specific tag/commit
)
FetchContent_MakeAvailable(spdlog)
# Option 2: find_package if spdlog is installed system-wide or via Nix/Conan

# --- Protocol Buffer and gRPC Code Generation ---
# Assumes proto files are in ../../proto relative to CMakeLists.txt
set(PROTO_DIR "${CMAKE_CURRENT_SOURCE_DIR}/../../proto")
set(PROTO_FILES
  "${PROTO_DIR}/market_data.proto"
  # Add other proto files if this service needs them (e.g., maybe an indicator output proto)
)
set(PROTO_GENERATED_DIR "${CMAKE_CURRENT_BINARY_DIR}/generated/proto")
set(GRPC_GENERATED_DIR "${CMAKE_CURRENT_BINARY_DIR}/generated/grpc")

# Create directories for generated files
file(MAKE_DIRECTORY "${PROTO_GENERATED_DIR}" "${GRPC_GENERATED_DIR}")

# Generate Protobuf C++ sources
protobuf_generate_cpp(PROTO_SRCS PROTO_HDRS ${PROTO_FILES})
# Generate gRPC C++ sources
protobuf_generate_grpc_cpp(GRPC_SRCS GRPC_HDRS ${PROTO_FILES})

# --- Executable Target ---
add_executable(indicator_engine_server
  src/main.cpp
  src/interface.cpp
  src/config.cpp
  # Indicator implementations
  src/indicators/SMA.cpp
  src/indicators/RSI.cpp
  src/indicators/EMA.cpp # Add other indicators
  # Generated sources
  ${PROTO_SRCS} ${PROTO_HDRS}
  ${GRPC_SRCS} ${GRPC_HDRS}
)

# --- Include Directories ---
target_include_directories(indicator_engine_server PRIVATE
  "${CMAKE_CURRENT_SOURCE_DIR}/include" # Project includes
  "${PROTO_GENERATED_DIR}"              # Generated protobuf headers
  "${GRPC_GENERATED_DIR}"              # Generated gRPC headers
  ${EIGEN3_INCLUDE_DIRS}                # Eigen headers
  ${Protobuf_INCLUDE_DIRS}              # Protobuf headers
  ${gRPC_INCLUDE_DIRS}                  # gRPC headers
  # Add spdlog include dir (FetchContent handles this automatically usually)
  # ${spdlog_SOURCE_DIR}/include # If needed explicitly
)
if(INDICATOR_ENGINE_USE_TA_LIB)
  target_include_directories(indicator_engine_server PRIVATE ${TALIB_INCLUDE_DIRS})
endif()
if(INDICATOR_ENGINE_ENABLE_GPU)
  target_include_directories(indicator_engine_server PRIVATE ${CUDA_INCLUDE_DIRS})
endif()

# --- Link Libraries ---
target_link_libraries(indicator_engine_server PRIVATE
  Threads::Threads                  # C++ Threads
  Eigen3::Eigen                     # Eigen
  ${Protobuf_LIBRARIES}             # Protobuf runtime
  ${gRPC_LIBRARIES}                 # gRPC runtime (includes grpc++, grpc, address_sorting, upb)
  spdlog::spdlog                    # Logging library (header only or compiled target)
)
if(INDICATOR_ENGINE_USE_TA_LIB)
  target_link_libraries(indicator_engine_server PRIVATE ${TALIB_LIBRARIES})
endif()
if(INDICATOR_ENGINE_ENABLE_OMP)
  target_link_libraries(indicator_engine_server PRIVATE OpenMP::OpenMP_CXX)
endif()
if(INDICATOR_ENGINE_ENABLE_GPU)
  target_link_libraries(indicator_engine_server PRIVATE CUDA::cudart) # Link CUDA runtime
  # Link other CUDA libs if used (e.g., CUDA::cublas)
endif()

# --- CUDA Specifics (if enabled) ---
if(INDICATOR_ENGINE_ENABLE_GPU)
  # Add CUDA source files
  target_sources(indicator_engine_server PRIVATE
    src/cuda_kernels.cu
  )
  # Set CUDA specific compile options if needed
  # target_compile_options(indicator_engine_server PRIVATE $<$<COMPILE_LANGUAGE:CUDA>:-arch=sm_75>)
  set_target_properties(indicator_engine_server PROPERTIES CUDA_SEPARABLE_COMPILATION ON) # If needed
endif()

# --- Compile Definitions/Flags ---
target_compile_definitions(indicator_engine_server PRIVATE
  # Example definition, could be used to switch logging levels etc.
  # SPDLOG_ACTIVE_LEVEL=SPDLOG_LEVEL_DEBUG
)
if(INDICATOR_ENGINE_USE_TA_LIB)
  target_compile_definitions(indicator_engine_server PRIVATE USE_TA_LIB)
endif()
if(INDICATOR_ENGINE_ENABLE_GPU)
  target_compile_definitions(indicator_engine_server PRIVATE USE_GPU)
endif()

# Compiler specific flags for optimization and warnings
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
  target_compile_options(indicator_engine_server PRIVATE
    -Wall -Wextra -Wpedantic         # Enable common warnings
    -Wno-unused-parameter           # Often noisy with generated code/callbacks
    # Optimization flags (Release build type usually sets -O3)
    $<$<CONFIG:Release>:-O3 -DNDEBUG>
    # Debug flags (Debug build type usually sets -g)
    $<$<CONFIG:Debug>:-g>
  )
endif()

# --- Installation (Optional) ---
# install(TARGETS indicator_engine_server DESTINATION bin)
# install(FILES config/default.yaml DESTINATION etc/indicator_engine)

# --- Testing (Optional - using CTest/GoogleTest) ---
# enable_testing()
# find_package(GTest REQUIRED)
# add_executable(indicator_tests tests/test_main.cpp tests/test_sma.cpp ...)
# target_link_libraries(indicator_tests PRIVATE indicator_engine_server GTest::gtest_main)
# include(GoogleTest)
# gtest_discover_tests(indicator_tests)

# --- Output ---
message(STATUS "Configuration Summary:")
message(STATUS "  CXX Standard: ${CMAKE_CXX_STANDARD}")
message(STATUS "  Build Type: ${CMAKE_BUILD_TYPE}")
message(STATUS "  GPU Enabled: ${INDICATOR_ENGINE_ENABLE_GPU}")
message(STATUS "  TA-Lib Enabled: ${INDICATOR_ENGINE_USE_TA_LIB}")
message(STATUS "  OpenMP Enabled: ${INDICATOR_ENGINE_ENABLE_OMP}")
message(STATUS "  Using Eigen3: ${EIGEN3_INCLUDE_DIRS}")
message(STATUS "  Using Protobuf: ${Protobuf_LIBRARIES}")
message(STATUS "  Using gRPC: ${gRPC_LIBRARIES}")
```
**Note on `FindTALib.cmake`:** If `find_package(TALib)` doesn't work, you might need to create a `cmake/FindTALib.cmake` file to help CMake locate the TA-Lib library and headers, or rely on `pkg-confi