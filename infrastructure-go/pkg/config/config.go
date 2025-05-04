// pkg/config/config.go
package config

import (
	"os"

	"gopkg.in/yaml.v3"
)

// GrpcConfig holds gRPC related configuration
type GrpcConfig struct {
	ListenAddress string `yaml:"listen_address"`
	// Note: DiscoveryService address is the same as ListenAddress in this setup
}

// AppConfig holds the overall configuration for the infrastructure service
type AppConfig struct {
	Grpc           GrpcConfig `yaml:"grpc"`
	ConfigsDirectory string     `yaml:"configs_directory"` // For ConfigService
	// Add other infrastructure config fields here (e.g., discovery settings)
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