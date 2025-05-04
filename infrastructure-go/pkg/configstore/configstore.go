// pkg/configstore/configstore.go
package configstore

import (
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"sync"
)

// ConfigStore holds configuration data in memory
// Structure: map[service_name]map[service_version]map[file_name][]byte
type ConfigStore struct {
	store map[string]map[string]map[string][]byte
	mu    sync.RWMutex // Mutex for concurrent access
}

// NewConfigStore creates a new ConfigStore
func NewConfigStore() *ConfigStore {
	return &ConfigStore{
		store: make(map[string]map[string]map[string][]byte),
	}
}

// LoadConfigsFromDirectory loads configuration files from a specified base directory.
// The directory structure is expected to be <baseDir>/<service_name>/<service_version>/<config_file.yaml|json|etc>
func (cs *ConfigStore) LoadConfigsFromDirectory(baseDir string) error {
	cs.mu.Lock() // Acquire write lock during loading
	defer cs.mu.Unlock()

	cs.store = make(map[string]map[string]map[string][]byte) // Clear existing store

	entries, err := os.ReadDir(baseDir)
	if err != nil {
		return fmt.Errorf("failed to read base config directory %s: %w", baseDir, err)
	}

	for _, entry := range entries {
		if !entry.IsDir() {
			continue // Skip non-directories at the service_name level
		}
		serviceName := entry.Name()
		servicePath := filepath.Join(baseDir, serviceName)

		versionEntries, err := os.ReadDir(servicePath)
		if err != nil {
			return fmt.Errorf("failed to read service directory %s: %w", servicePath, err)
		}

		cs.store[serviceName] = make(map[string]map[string][]byte) // Initialize map for the service

		for _, versionEntry := range versionEntries {
			if !versionEntry.IsDir() {
				continue // Skip non-directories at the service_version level
			}
			serviceVersion := versionEntry.Name()
			versionPath := filepath.Join(servicePath, serviceVersion)

			configFiles, err := os.ReadDir(versionPath)
			if err != nil {
				return fmt.Errorf("failed to read version directory %s: %w", versionPath, err)
			}

			cs.store[serviceName][serviceVersion] = make(map[string][]byte) // Initialize map for the version

			for _, configFileEntry := range configFiles {
				if configFileEntry.IsDir() {
					continue // Skip directories within the version directory
				}
				configFileName := configFileEntry.Name()
				configFilePath := filepath.Join(versionPath, configFileName)

				data, err := os.ReadFile(configFilePath)
				if err != nil {
					return fmt.Errorf("failed to read config file %s: %w", configFilePath, err)
				}

				cs.store[serviceName][serviceVersion][configFileName] = data // Store config data

				fmt.Printf("Loaded config: %s/%s/%s\n", serviceName, serviceVersion, configFileName)
			}
		}
	}

	return nil
}

// GetConfig retrieves configuration data for a specific service and version.
// It concatenates all files found for that service version into a single byte slice,
// or returns the content of a single expected file (e.g., "settings.yaml").
// For now, we'll just return the content of the first file found, or require a specific filename.
// Let's adjust this to return the content of "settings.yaml" if it exists.
func (cs *ConfigStore) GetConfig(serviceName, serviceVersion string) ([]byte, error) {
	cs.mu.RLock() // Acquire read lock for retrieval
	defer cs.mu.RUnlock()

	versions, ok := cs.store[serviceName]
	if !ok {
		return nil, fmt.Errorf("service '%s' not found", serviceName)
	}

	configs, ok := versions[serviceVersion]
	if !ok {
		return nil, fmt.Errorf("version '%s' for service '%s' not found", serviceVersion, serviceName)
	}

	// Retrieve a specific config file, e.g., "settings.yaml"
	// A more advanced store might merge files or return a map of filenames to content.
	// For simplicity, we assume clients are requesting the main config file like "settings.yaml".
	configData, ok := configs["settings.yaml"] // Hardcode filename assumption for now
	if !ok {
		// If "settings.yaml" isn't found, try returning the first file? Or error? Error is safer.
		return nil, fmt.Errorf("config file 'settings.yaml' not found for service '%s' version '%s'", serviceName, serviceVersion)
	}


	return configData, nil
}