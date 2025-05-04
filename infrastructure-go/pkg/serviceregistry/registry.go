// pkg/serviceregistry/registry.go
package serviceregistry

import (
	"fmt"
	"sync"
	"time"

	"google.golang.org/protobuf/types/known/timestamppb"

	pb "trading-bot-monorepo/proto/discovery_service" // Import discovery proto
)

// ServiceInstance represents a registered service instance with metadata
// Using the protobuf ServiceInstance message directly is convenient
// type ServiceInstance pb.ServiceInstance // Can alias or use directly

// ServiceRegistry holds the in-memory map of registered services
// Structure: map[service_name][service_version]map[instance_address]pb.ServiceInstance
type ServiceRegistry struct {
	registry map[string]map[string]map[string]*pb.ServiceInstance
	mu       sync.RWMutex // Mutex for concurrent access
	// Add a mechanism for cleanup (e.g., background goroutine, heartbeat expiration)
}

// NewServiceRegistry creates a new ServiceRegistry
func NewServiceRegistry() *ServiceRegistry {
	return &ServiceRegistry{
		registry: make(map[string]map[string]map[string]*pb.ServiceInstance),
	}
}

// RegisterService registers a service instance.
// It replaces an existing instance if the address, name, and version match.
func (sr *ServiceRegistry) RegisterService(instance *pb.ServiceInstance) error {
	if instance == nil || instance.GetServiceName() == "" || instance.GetServiceVersion() == "" || instance.GetAddress() == "" {
		return fmt.Errorf("invalid service instance details: name, version, and address are required")
	}

	// Ensure registration timestamp is set if missing
	if instance.RegistrationTimestamp == nil {
		instance.RegistrationTimestamp = timestamppb.Now()
	}

	sr.mu.Lock() // Acquire write lock for registration
	defer sr.mu.Unlock()

	// Get or create map for service name
	versions, ok := sr.registry[instance.GetServiceName()]
	if !ok {
		versions = make(map[string]map[string]*pb.ServiceInstance)
		sr.registry[instance.GetServiceName()] = versions
	}

	// Get or create map for service version
	instances, ok := versions[instance.GetServiceVersion()]
	if !ok {
		instances = make(map[string]*pb.ServiceInstance)
		versions[instance.GetServiceVersion()] = instances
	}

	// Store or update the instance by address
	instances[instance.GetAddress()] = instance

	fmt.Printf("Registered service: %s/%s at %s (Timestamp: %v)\n",
		instance.GetServiceName(), instance.GetServiceVersion(), instance.GetAddress(), instance.GetRegistrationTimestamp().AsTime().Format(time.RFC3339))

	return nil
}

// LookupService finds service instances by name and version.
// Use "*" as serviceVersion to find all versions.
func (sr *ServiceRegistry) LookupService(serviceName, serviceVersion string) []*pb.ServiceInstance {
	sr.mu.RLock() // Acquire read lock for lookup
	defer sr.mu.RUnlock()

	var foundInstances []*pb.ServiceInstance

	versions, ok := sr.registry[serviceName]
	if !ok {
		return foundInstances // Service name not found, return empty list
	}

	if serviceVersion == "*" {
		// Lookup all versions for this service name
		for _, instances := range versions {
			for _, instance := range instances {
				foundInstances = append(foundInstances, instance)
			}
		}
	} else {
		// Lookup specific version
		instances, ok := versions[serviceVersion]
		if !ok {
			return foundInstances // Specific version not found, return empty list
		}
		for _, instance := range instances {
			foundInstances = append(foundInstances, instance)
		}
	}

	return foundInstances
}

// DeregisterService removes a service instance from the registry.
// In a real system, this would be called on graceful shutdown or via a cleanup process.
func (sr *ServiceRegistry) DeregisterService(instance *pb.ServiceInstance) error {
     if instance == nil || instance.GetServiceName() == "" || instance.GetServiceVersion() == "" || instance.GetAddress() == "" {
		return fmt.Errorf("invalid service instance details: name, version, and address are required")
	}

     sr.mu.Lock() // Acquire write lock for deregistration
     defer sr.mu.Unlock()

     versions, ok := sr.registry[instance.GetServiceName()]
     if !ok {
          return fmt.Errorf("service '%s' not found for deregistration", instance.GetServiceName())
     }

     instances, ok := versions[instance.GetServiceVersion()]
     if !ok {
          return fmt.Errorf("version '%s' for service '%s' not found for deregistration", instance.GetServiceVersion(), instance.GetServiceName())
     }

     if _, ok := instances[instance.GetAddress()]; ok {
          delete(instances, instance.GetAddress())
          fmt.Printf("Deregistered service: %s/%s at %s\n",
               instance.GetServiceName(), instance.GetServiceVersion(), instance.GetAddress())

          // Optional: Clean up empty version/service maps if they become empty
          if len(instances) == 0 {
              delete(versions, instance.GetServiceVersion())
          }
          if len(versions) == 0 {
              delete(sr.registry, instance.GetServiceName())
          }

          return nil
     } else {
          return fmt.Errorf("instance '%s/%s@%s' not found for deregistration",
               instance.GetServiceName(), instance.GetServiceVersion(), instance.GetAddress())
     }
}

// Note: For a robust production system, you would need:
// 1. Heartbeats: Services send periodic heartbeats to keep their registration alive.
// 2. Cleanup: A background goroutine in ServiceRegistry to remove instances whose heartbeats have expired.
// 3. Leadership Election: If running multiple registry instances, they need to coordinate (e.g., using etcd or Consul).
// 4. Persistence: Store the registry state to disk or a database.