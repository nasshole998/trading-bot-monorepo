// pkg/configservice/configservice.go
package configservice

import (
	"context"

	"google.golang.org/grpc"

	// Import the generated protobuf code
	pb "trading-bot-monorepo/proto/config_service"
	"trading-bot-monorepo/infrastructure-go/pkg/configstore"
)

// ConfigServiceServer implements the gRPC server interface for the ConfigService
type ConfigServiceServer struct {
	pb.UnimplementedConfigServiceServer // Embed for forward compatibility
	store                             *configstore.ConfigStore
}

// NewConfigServiceServer creates a new ConfigServiceServer instance
func NewConfigServiceServer(store *configstore.ConfigStore) *ConfigServiceServer {
	return &ConfigServiceServer{
		store: store,
	}
}

// GetConfig handles incoming gRPC requests to retrieve configuration
func (s *ConfigServiceServer) GetConfig(ctx context.Context, req *pb.GetConfigRequest) (*pb.GetConfigResponse, error) {
	// Basic validation
	if req.GetServiceName() == "" {
		return &pb.GetConfigResponse{
			Success:       false,
			Error: "service_name is required",
		}, nil // Return gRPC OK with application-level error
		// Or return grpc.Errorf(codes.InvalidArgument, "service_name is required") for gRPC error
	}
	if req.GetServiceVersion() == "" {
		return &pb.GetConfigResponse{
			Success:       false,
			Error: "service_version is required",
		}, nil // Return gRPC OK with application-level error
		// Or return grpc.Errorf(codes.InvalidArgument, "service_version is required") for gRPC error
	}


	configData, err := s.store.GetConfig(req.GetServiceName(), req.GetServiceVersion())
	if err != nil {
		return &pb.GetConfigResponse{
			Success:       false,
			Error: err.Error(), // Return the error from the store
		}, nil // Return gRPC OK with application-level error
		// Or return grpc.Errorf(codes.NotFound, "config not found: %v", err) for gRPC error
	}

	return &pb.GetConfigResponse{
		Success:   true,
		ConfigData: configData,
	}, nil
}

// RegisterService registers the ConfigServiceServer with a gRPC server
func (s *ConfigServiceServer) RegisterService(grpcServer *grpc.Server) {
	pb.RegisterConfigServiceServer(grpcServer, s)
}