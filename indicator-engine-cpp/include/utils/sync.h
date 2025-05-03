#ifndef INDICATOR_ENGINE_UTILS_SYNC_H
#define INDICATOR_ENGINE_UTILS_SYNC_H

// Include standard library synchronization primitives used throughout the project.
// No custom synchronization classes are defined here currently.

#include <mutex>              // For std::mutex, std::lock_guard, std::unique_lock
#include <condition_variable> // For std::condition_variable


namespace Utils {
namespace Sync {

// Standard library synchronization primitives are used directly.
// This namespace can be used for custom synchronization helpers if needed in the future.

} // namespace Sync
} // namespace Utils

#endif // INDICATOR_ENGINE_UTILS_SYNC_H