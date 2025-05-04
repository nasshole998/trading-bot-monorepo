package main

import (
	"fmt"
	"log"
	"net"

	"google.golang.org/grpc"

	// Import local packages
	"trading-bot-monorepo/infrastructure-go/pkg/config"
	"trading-bot-monorepo/infrastructure-go/pkg/configservice"
	"trading-bot-monorepo/infrastructure-go/pkg/configstore"
	"trading-bot-monorepo/infrastructure-go/pkg/discoveryservice" // **NEW**
	"trading-bot-monorepo/infrastructure-go/pkg/serviceregistry"  // **NEW**

	// Import generated protobuf packages
	// Ensure your go.mod has the correct replace directive if needed
	pb_config "trading-bot-monorepo/proto/config_service"
	pb_discovery "trading-bot-monorepo/proto/discovery_service" // **NEW**
)

func main() {
	// --- 1. Load Infrastructure Service Configuration ---
	appConfig, err := config.LoadConfig("./config/infrastructure_config.yaml")
	if err != nil {
		log.Fatalf("Failed to load application configuration: %v", err)
	}

	// --- 2. Initialize Configuration Store (for ConfigService) ---
	configStore := configstore.NewConfigStore()
	err = configStore.LoadConfigsFromDirectory(appConfig.ConfigsDirectory)
	if err != nil {
		// Log this as a warning or error, but don't necessarily fatal if config directory is optional
		// For now, let's keep it fatal if directory is specified but loading fails
		log.Fatalf("Failed to load configurations from directory %s: %v", appConfig.ConfigsDirectory, err)
	}


	// --- 3. Initialize Service Registry (for DiscoveryService) --- **NEW**
	serviceRegistry := serviceregistry.NewServiceRegistry()
	fmt.Println("Service Registry initialized")


	// --- 4. Setup gRPC Server ---
	listenAddr := appConfig.Grpc.ListenAddress
	lis, err := net.Listen("tcp", listenAddr)
	if err != nil {
		log.Fatalf("Failed to listen on %s: %v", listenAddr, err)
	}
	fmt.Printf("Infrastructure Service (Config & Discovery) listening on %s\n", listenAddr)

	grpcServer := grpc.NewServer()

	// --- 5. Register Services with gRPC Server ---
	// Register ConfigService
	configServiceImpl := configservice.NewConfigServiceServer(configStore)
	pb_config.RegisterConfigServiceServer(grpcServer, configServiceImpl)
	fmt.Println("ConfigService registered")

	// Register DiscoveryService **NEW**
	discoveryServiceImpl := discoveryservice.NewDiscoveryServiceServer(serviceRegistry)
	pb_discovery.RegisterDiscoveryServiceServer(grpcServer, discoveryServiceImpl)
	fmt.Println("DiscoveryService registered")


	// --- 6. Start gRPC Server ---
	fmt.Println("Starting gRPC server...")
	if err := grpcServer.Serve(lis); err != nil {
		log.Fatalf("Failed to serve gRPC server: %v", err)
	}
}