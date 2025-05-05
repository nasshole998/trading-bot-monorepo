// pkg/accountstateclient/client.go
package accountstateclient

import (
	"context"
	"fmt"
	"log"

	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"

	pb "trading-bot-monorepo/proto/account_state" // Import generated account_state proto
)

// Client is a gRPC client for the AccountStateService
type Client struct {
	conn   *grpc.ClientConn
	client pb.AccountStateServiceClient
}

// NewClient creates a new AccountState gRPC client
func NewClient(addr string) (*Client, error) {
	conn, err := grpc.Dial(addr, grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		return nil, fmt.Errorf("did not connect to account state service at %s: %w", addr, err)
	}
	log.Printf("Connected to Account State service at %s", addr)

	client := pb.NewAccountStateServiceClient(conn)

	return &Client{
		conn:   conn,
		client: client,
	}, nil
}

// Close closes the gRPC client connection
func (c *Client) Close() {
	if c.conn != nil {
		c.conn.Close()
		log.Println("Account State gRPC client connection closed")
	}
}

// **NEW:** GetAccountSnapshot calls the GetAccountSnapshot RPC
func (c *Client) GetAccountSnapshot(ctx context.Context, accountID string) (*pb.AccountUpdate, error) {
	log.Printf("Calling GetAccountSnapshot for account: %s", accountID)
	req := &pb.GetAccountSnapshotRequest{
		AccountId: accountID,
	}
	res, err := c.client.GetAccountSnapshot(ctx, req)
	if err != nil {
		return nil, fmt.Errorf("failed to call GetAccountSnapshot for account %s: %w", accountID, err)
	}
	log.Printf("Received account snapshot for account: %s", accountID)
	return res, nil
}

// Note: SubscribeToAccountUpdates and SubscribeToExecutionReports methods could be added later
// if GraphQL subscriptions or server-sent events are implemented for live updates.