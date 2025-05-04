// server.go
package main

import (
	"log"
	"net/http"
	"os"

	"github.com/99designs/gqlgen/graphql/handler"
	"github.com/99designs/gqlgen/graphql/playground" // Optional: GraphQL Playground

	"trading-bot-monorepo/ui-sveltekit-typescript/bff-go/graph" // Import the graph package
)

const defaultPort = "8080"

// initGraphQLServer sets up and returns the GraphQL HTTP handler
func initGraphQLServer(resolver *graph.Resolver) *handler. যাঁরা {
	// Create a new executable schema
	cfg := graph.NewExecutableSchema(graph.Config{Resolvers: resolver})

	// Create a new GraphQL handler
	srv := handler.NewDefaultServer(cfg)

	// Optional: Add middleware (e.g., logging, auth)
	// srv.AroundOperations(func(ctx context.Context, next gqlgen.OperationHandler) gqlgen.ResponseHandler { ... })

	return srv
}


// runHTTPServer starts the HTTP server with the GraphQL handler
func runHTTPServer(handler http.Handler, listenAddr string) {
	log.Printf("connect to http://localhost:%s/ for GraphQL playground", extractPort(listenAddr))

	// Start the HTTP server
	log.Fatal(http.ListenAndServe(listenAddr, handler))
}

// extractPort is a helper to get the port from an address string (e.g., "0.0.0.0:8080" -> "8080")
func extractPort(addr string) string {
	_, port, err := net.SplitHostPort(addr)
	if err != nil {
		return defaultPort // Default if parsing fails
	}
	return port
}