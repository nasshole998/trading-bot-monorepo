// pkg/discoveryservice/discoveryservice.go
package discoveryservice

import (
	"context"
	"fmt"

	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status" // Use status package for rich gRPC errors

	// Import the generated protobuf code
	pb "trading-bot-monorepo/proto/discovery_service"
	"trading-bot-monorepo/infrastructure-go/pkg/serviceregistry" // Import service registry
)

// DiscoveryServiceServer implements the gRPC server interface for the DiscoveryService
type DiscoveryServiceServer struct {
	pb.UnimplementedDiscoveryServiceServer // Embed for forward compatibility
	registry *serviceregistry.ServiceRegistry
}

// NewDiscoveryServiceServer creates a new DiscoveryServiceServer instance
func NewDiscoveryServiceServer(registry *serviceregistry.ServiceRegistry) *DiscoveryServiceServer {
	return &DiscoveryServiceServer{
		registry: registry,
	}
}

// RegisterService handles incoming gRPC requests to register a service instance
func (s *DiscoveryServiceServer) RegisterService(ctx context.Context, req *pb.RegisterServiceRequest) (*pb.RegisterServiceResponse, error) {
	if req.GetInstance() == nil {
		return nil, status.Errorf(codes.InvalidArgument, "service instance details are required")
	}

	instance := req.GetInstance()
	if instance.GetServiceName() == "" || instance.GetServiceVersion() == "" || instance.GetAddress() == "" {
		return nil, status.Errorf(codes.InvalidArgument, "service name, version, and address are required")
	}

	err := s.registry.RegisterService(instance)
	if err != nil {
		// Log the error internally but return a gRPC error status to the client
		fmt.Printf("Error registering service %s/%s@%s: %v\n",
			instance.GetServiceName(), instance.GetServiceVersion(), instance.GetAddress(), err)
		return &pb.RegisterServiceResponse{
			Success:       false,
			Error: err.Error(), // Return application-level error message
		}, nil // Return gRPC OK with application-level error flag
		// Alternatively, return status.Errorf(codes.Internal, "registration failed: %v", err)
	}

	return &pb.RegisterServiceResponse{
		Success: true,
	}, nil
}

// LookupService handles incoming gRPC requests to lookup service instances
func (s *DiscoveryServiceServer) LookupService(ctx context.Context, req *pb.LookupServiceRequest) (*pb.LookupServiceResponse, error) {
	if req.GetServiceName() == "" {
		return nil, status.Errorf(codes.InvalidArgument, "service_name is required")
	}
	if req.GetServiceVersion() == "" {
		// Assume "*" if version is not provided, or enforce requirement
		// Let's allow "*" for any version
		req.ServiceVersion = "*"
	}

	instances := s.registry.LookupService(req.GetServiceName(), req.GetServiceVersion())

	// Return success=true even if the list is empty, as the lookup itself was successful
	// The error_message field is typically reserved for failures in the lookup *process*.
	return &pb.LookupServiceResponse{
		Success:   true,
		Instances: instances, // Can be empty slice
	}, nil
}

// RegisterService registers the DiscoveryServiceServer with a gRPC server
func (s *DiscoveryServiceServer) RegisterService(grpcServer *grpc.Server) {
	pb.RegisterDiscoveryServiceServer(grpcServer, s)
}