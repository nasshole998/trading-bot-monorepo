// pkg/infrastructureclient/client.go
package infrastructureclient

import (
	"context"
	"fmt"
	"log"
	"time"

	"google.golang.org/grpc"
	"google.golang.org/grpc/connectivity" // **NEW** Import connectivity package
	"google.golang.org/grpc/credentials/insecure"

	// We don't necessarily need the generated client *services* here
	// unless we plan to call their RPCs directly from the BFF later.
	// For status check, we just need the channel state.
	// If needed:
	// pb_config "trading-bot-monorepo/proto/config_service"
	// pb_discovery "trading-bot-monorepo/proto/discovery_service"
)

// Client is a gRPC client for the Infrastructure service endpoint.
// It can connect to the endpoint hosting both Config and Discovery services.
type Client struct {
	conn *grpc.ClientConn
	// If needed later, add clients for specific services hosted at this endpoint:
	// ConfigServiceClient   pb_config.ConfigServiceClient
	// DiscoveryServiceClient pb_discovery.DiscoveryServiceClient
}

// NewClient creates a new Infrastructure gRPC client
func NewClient(addr string) (*Client, error) {
	// Set up a connection to the server. This connection will be long-lived.
	conn, err := grpc.Dial(addr,
		grpc.WithTransportCredentials(insecure.NewCredentials()), // Use insecure for local dev
		grpc.WithBlock(), // Block until connection is established or timeout
		grpc.WithTimeout(5*time.Second), // Connection timeout
	)
	if err != nil {
		return nil, fmt.Errorf("did not connect to infrastructure service at %s: %w", addr, err)
	}
	log.Printf("Connected to Infrastructure service at %s", addr)

	// If needed later, initialize specific service clients using this connection:
	// configClient := pb_config.NewConfigServiceClient(conn)
	// discoveryClient := pb_discovery.NewDiscoveryServiceClient(conn)

	return &Client{
		conn:   conn,
		// ConfigServiceClient: configClient,
		// DiscoveryServiceClient: discoveryClient,
	}, nil
}

// Close closes the gRPC client connection
func (c *Client) Close() {
	if c.conn != nil {
		c.conn.Close()
		log.Println("Infrastructure gRPC client connection closed")
	}
}

// **NEW:** CheckConnectivity gets the current state of the gRPC channel.
func (c *Client) CheckConnectivity() connectivity.State {
	if c.conn == nil {
		return connectivity.Shutdown // Or some custom error state
	}
	// GetState() returns the current state of the channel without attempting to change it.
	// For a more active check, you could use WaitForStateChange in a goroutine,
	// but GetState is sufficient for a snapshot status check.
	return c.conn.GetState()
}

// **NEW:** GetAddress returns the address this client is connected to.
func (c *Client) GetAddress() string {
	if c.conn == nil {
		return "unknown"
	}
	return c.conn.Target() // Returns the address string used in grpc.Dial
}