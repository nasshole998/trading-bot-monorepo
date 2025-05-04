package main

import (
	"log"

	"trading-bot-monorepo/ui-sveltekit-typescript/bff-go/config" // Local config
	"trading-bot-monorepo/ui-sveltekit-typescript/bff-go/graph"   // GraphQL graph setup
	"trading-bot-monorepo/ui-sveltekit-typescript/bff-go/pkg/backtesterclient" // Backtester gRPC client
	// Import other backend clients as needed in future messages
)

func main() {
	// --- 1. Load BFF Configuration ---
	appConfig, err := config.LoadConfig("./config/bff_config.yaml")
	if err != nil {
		log.Fatalf("Failed to load application configuration: %v", err)
	}

	// --- 2. Initialize Backend gRPC Clients ---
	// Initialize Backtester client
	backtesterClient, err := backtesterclient.NewClient(appConfig.GrpcBackends.BacktesterAddress)
	if err != nil {
		// Log the error and decide if it's fatal.
		// For now, fatal if a core dependency client fails to connect on startup.
		log.Fatalf("Failed to initialize Backtester gRPC client: %v", err)
	}
	defer backtesterClient.Close() // Ensure client connection is closed on exit


	// Initialize other clients here in future messages (Risk Management, Data Ingestion, Infrastructure)


	// --- 3. Initialize GraphQL Resolver ---
	// The resolver holds dependencies needed to fetch data
	resolver := graph.NewResolver(backtesterClient)
	// Pass other clients to the resolver as they are initialized


	// --- 4. Setup and Run GraphQL HTTP Server ---
	// Create the GraphQL HTTP handler
	graphqlHandler := initGraphQLServer(resolver)

	// Optional: Setup GraphQL Playground handler (useful for testing)
	playgroundHandler := playground.Handler("GraphQL playground", "/query")

	// Create a simple HTTP router
	http.Handle("/", playgroundHandler) // Serve playground on root
	http.Handle("/query", graphqlHandler) // Serve GraphQL API on /query

	// Run the HTTP server
	log.Printf("Starting GraphQL server on %s", appConfig.HTTP.ListenAddress)
	runHTTPServer(http.DefaultServeMux, appConfig.HTTP.ListenAddress)

	// The runHTTPServer is blocking (log.Fatal), so code below is unreachable unless it fails to start.
	log.Println("BFF shutting down")
}