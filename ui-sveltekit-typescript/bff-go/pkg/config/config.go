// pkg/config/config.go
package config

import (
	"os"

	"gopkg.in/yaml.v3"
)

// HTTPConfig holds HTTP related configuration for the GraphQL server
type HTTPConfig struct {
	ListenAddress string `yaml:"listen_address"`
}

// GrpcBackendsConfig holds addresses for backend gRPC services
type GrpcBackendsConfig struct {
	BacktesterAddress string `yaml:"backtester_address"`
	// Add fields for other backend addresses
	// RiskManagementAddress string `yaml:"risk_management_address"`
	// DataIngestionAddress string `yaml:"data_ingestion_address"`
	// InfrastructureAddress string `yaml:"infrastructure_address"`
}

// AppConfig holds the overall configuration for the BFF service
type AppConfig struct {
	HTTP         HTTPConfig         `yaml:"http"`
	GrpcBackends GrpcBackendsConfig `yaml:"grpc_backends"`
	// Add other config fields
}

// LoadConfig loads the application configuration from a YAML file
func LoadConfig(filePath string) (*AppConfig, error) {
	data, err := os.ReadFile(filePath)
	if err != nil {
		return nil, err
	}

	var cfg AppConfig
	err = yaml.Unmarshal(data, &cfg)
	if err != nil {
		return nil, err
	}

	return &cfg, nil
}