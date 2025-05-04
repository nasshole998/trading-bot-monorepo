// pkg/backtesterclient/client.go
package backtesterclient

import (
	"context"
	"fmt"
	"log"

	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure" // Use insecure for local dev

	pb "trading-bot-monorepo/proto/backtester" // Import generated backtester proto
)

// Client is a gRPC client for the Backtester service
type Client struct {
	conn   *grpc.ClientConn
	client pb.BacktesterServiceClient
}

// NewClient creates a new Backtester gRPC client
func NewClient(addr string) (*Client, error) {
	conn, err := grpc.Dial(addr, grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		return nil, fmt.Errorf("did not connect to backtester service at %s: %w", addr, err)
	}
	log.Printf("Connected to Backtester service at %s", addr)

	client := pb.NewBacktesterServiceClient(conn)

	return &Client{
		conn:   conn,
		client: client,
	}, nil
}

// Close closes the gRPC client connection
func (c *Client) Close() {
	if c.conn != nil {
		c.conn.Close()
		log.Println("Backtester gRPC client connection closed")
	}
}

// ListBacktests calls the ListBacktests RPC on the Backtester service
func (c *Client) ListBacktests(ctx context.Context) ([]*pb.BacktestResultSummary, error) {
	log.Println("Calling ListBacktests on Backtester service...")
	req := &pb.ListBacktestsRequest{}
	res, err := c.client.ListBacktests(ctx, req)
	if err != nil {
		return nil, fmt.Errorf("failed to call ListBacktests: %w", err)
	}
	log.Printf("Received %d backtest summaries", len(res.GetSummaries()))
	return res.GetSummaries(), nil
}

// **NEW:** GetBacktestDetails calls the GetBacktestResult RPC on the Backtester service
func (c *Client) GetBacktestDetails(ctx context.Context, backtestID string) (*pb.BacktestResult, error) {
	log.Printf("Calling GetBacktestResult on Backtester service for ID: %s", backtestID)
	req := &pb.GetBacktestResultRequest{
		BacktestId: backtestID,
	}
	res, err := c.client.GetBacktestResult(ctx, req)
	if err != nil {
		return nil, fmt.Errorf("failed to call GetBacktestResult for ID %s: %w", backtestID, err)
	}
	log.Printf("Received detailed backtest result for ID: %s", backtestID)
	return res, nil
}