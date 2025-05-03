# Use a .NET SDK image
FROM mcr.microsoft.com/dotnet/sdk:8.0 AS build

# Install system dependencies (like protoc)
RUN apt-get update && apt-get install -y --no-install-recommends \
    protobuf-compiler \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /src

# Copy shared proto files and solution/project files
COPY proto/ ./proto/
COPY backtester-fsharp/backtester.fsproj backtester-fsharp/
COPY backtester-fsharp/Directory.Build.props backtester-fsharp/

# Restore dependencies (this will also trigger Protobuf code generation via Directory.Build.props)
RUN dotnet restore backtester-fsharp/backtester.fsproj

# Copy the rest of the source code
COPY backtester-fsharp/ ./backtester-fsharp/

WORKDIR /src/backtester-fsharp

# Build the F# project
RUN dotnet build -c Release -o /app/build

# Publish the application
RUN dotnet publish -c Release -o /app/publish --no-self-contained

# Stage 2: Create a smaller runtime image
FROM mcr.microsoft.com/dotnet/aspnet:8.0 AS final

WORKDIR /app

# Copy the published application from the build stage
COPY --from=build /app/publish .

# Copy configuration and data
COPY backtester-fsharp/config/ ./config/
COPY backtester-fsharp/data/ ./data/

# Set the entrypoint
ENTRYPOINT ["dotnet", "backtester-fsharp.dll"]

# Default command (optional, can be used to pass args)
# CMD ["--urls", "http://0.0.0.0:50054"] # Example setting listen address via args if not in config

# Expose the gRPC port
EXPOSE 50054