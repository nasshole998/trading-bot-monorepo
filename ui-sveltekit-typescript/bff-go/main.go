package main

import (
	"log"
	"net/http" // Import http here

	"github.com/99designs/gqlgen/graphql/playground" // Import playground here

	"trading-bot-monorepo/ui-sveltekit-typescript/bff-go/config"
	"trading-bot-monorepo/ui-sveltekit-typescript/bff-go/graph"
	"trading-bot-monorepo/ui-sveltekit-typescript/bff-go/pkg/backtesterclient"
	"trading-bot-monorepo/ui-sveltekit-typescript/bff-go/pkg/accountstateclient"
	"trading-bot-monorepo/ui-sveltekit-typescript/bff-go/pkg/riskmanagementclient"
	"trading-bot-monorepo/ui-sveltekit-typescript/bff-go/pkg/infrastructureclient" // **NEW**
)

func main() {
	// --- 1. Load BFF Configuration ---
	appConfig, err := config.LoadConfig("./config/bff_config.yaml")
	if err != nil {
		log.Fatalf("Failed to load application configuration: %v", err)
	}

	// --- 2. Initialize Backend gRPC Clients ---
	backtesterClient, err := backtesterclient.NewClient(appConfig.GrpcBackends.BacktesterAddress)
	if err != nil {
		log.Fatalf("Failed to initialize Backtester gRPC client: %v", err)
	}
	defer backtesterClient.Close()

	accountStateClient, err := accountstateclient.NewClient(appConfig.GrpcBackends.DataIngestionAddress)
	if err != nil {
		log.Fatalf("Failed to initialize Account State gRPC client: %v", err)
	}
	defer accountStateClient.Close()

	riskManagementClient, err := riskmanagementclient.NewClient(appConfig.GrpcBackends.RiskManagementAddress)
	if err != nil {
		log.Fatalf("Failed to initialize Risk Management gRPC client: %v", err)
	}
	defer riskManagementClient.Close()

	// **NEW:** Initialize Infrastructure client
	infrastructureClient, err := infrastructureclient.NewClient(appConfig.GrpcBackends.InfrastructureAddress)
	if err != nil {
		log.Fatalf("Failed to initialize Infrastructure gRPC client: %v", err)
	}
	defer infrastructureClient.Close()


	// --- 3. Initialize GraphQL Resolver ---
	// **NEW:** Pass all initialized clients and config to the resolver
	resolver := graph.NewResolver(backtesterClient, accountStateClient, riskManagementClient, infrastructureClient, appConfig)

	// --- 4. Setup and Run GraphQL HTTP Server ---
	graphqlHandler := initGraphQLServer(resolver)
	playgroundHandler := playground.Handler("GraphQL playground", "/query")
	http.Handle("/", playgroundHandler)
	http.Handle("/query", graphqlHandler)

	log.Printf("Starting GraphQL server on %s", appConfig.HTTP.ListenAddress)
	runHTTPServer(http.DefaultServeMux, appConfig.HTTP.ListenAddress)

	log.Println("BFF shutting down")
}