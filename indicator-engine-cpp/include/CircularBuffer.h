// indicator-engine-cpp/include/CircularBuffer.h
#pragma once

#include <vector>
#include <stdexcept>
#include <numeric>
#include <cmath>
#include <mutex> // For thread safety if needed

// Forward declaration for potential friend classes (like indicators)
// template <typename T> class SMA;

/**
 * @brief A thread-safe circular buffer optimized for time-series data.
 * Stores elements of type T. Fixed capacity. Not designed for resizing.
 * Provides methods for adding elements, accessing recent elements, and calculating sums/means.
 *
 * @tparam T The type of elements to store (e.g., double for prices).
 */
template <typename T>
class CircularBuffer {
public:
    /**
     * @brief Construct a new Circular Buffer object.
     *
     * @param capacity The maximum number of elements the buffer can hold. Must be > 0.
     */
    explicit CircularBuffer(size_t capacity) :
        m_capacity(capacity),
        m_data(capacity),
        m_head(0),
        m_size(0),
        m_sum(0.0) // Initialize sum
    {
        if (capacity == 0) {
            throw std::invalid_argument("CircularBuffer capacity must be positive.");
        }
    }

    // Disable copy constructor and assignment operator
    CircularBuffer(const CircularBuffer&) = delete;
    CircularBuffer& operator=(const CircularBuffer&) = delete;

    // Enable move constructor and assignment operator (optional, default might be fine)
    CircularBuffer(CircularBuffer&& other) noexcept = default;
    CircularBuffer& operator=(CircularBuffer&& other) noexcept = default;


    /**
     * @brief Adds a new element to the buffer.
     * If the buffer is full, the oldest element is overwritten.
     * Updates the running sum efficiently.
     * Thread-safe via mutex lock.
     *
     * @param value The element to add.
     */
    void push(const T& value) {
        std::lock_guard<std::mutex> lock(m_mutex); // Lock for thread safety

        if (m_size == m_capacity) {
            // Buffer is full, overwrite the oldest element (at m_head)
            m_sum -= m_data[m_head]; // Subtract the element being overwritten
            m_data[m_head] = value;
            m_sum += value;          // Add the new element
            m_head = (m_head + 1) % m_capacity; // Move head forward
        } else {
            // Buffer is not full, add at the next available position
            size_t tail = (m_head + m_size) % m_capacity;
            m_data[tail] = value;
            m_sum += value;
            m_size++;
        }
    }

    /**
     * @brief Checks if the buffer is full.
     * Thread-safe.
     * @return true If the buffer has reached its capacity.
     * @return false Otherwise.
     */
    bool is_full() const {
        std::lock_guard<std::mutex> lock(m_mutex);
        return m_size == m_capacity;
    }

    /**
     * @brief Gets the current number of elements in the buffer.
     * Thread-safe.
     * @return size_t The number of elements.
     */
    size_t size() const {
        std::lock_guard<std::mutex> lock(m_mutex);
        return m_size;
    }

    /**
     * @brief Gets the maximum capacity of the buffer.
     * @return size_t The capacity.
     */
    size_t capacity() const {
        return m_capacity;
    }

    /**
     * @brief Gets the most recently added element.
     * Throws std::out_of_range if the buffer is empty.
     * Thread-safe.
     * @return const T& Reference to the most recent element.
     */
    const T& latest() const {
        std::lock_guard<std::mutex> lock(m_mutex);
        if (m_size == 0) {
            throw std::out_of_range("CircularBuffer is empty.");
        }
        // The latest element is at the position before the current head (wrapping around)
        size_t latest_idx = (m_head + m_size - 1) % m_capacity;
        return m_data[latest_idx];
    }

     /**
     * @brief Gets the element at a specific index relative to the most recent element.
     * Index 0 is the most recent, index 1 is the second most recent, etc.
     * Throws std::out_of_range if the index is invalid or buffer doesn't have enough elements.
     * Thread-safe.
     * @param index The relative index (0 = latest).
     * @return const T& Reference to the element.
     */
    const T& operator[](size_t index) const {
         std::lock_guard<std::mutex> lock(m_mutex);
        if (index >= m_size) {
            throw std::out_of_range("Index out of range for CircularBuffer.");
        }
        // Calculate the actual index in the underlying vector
        size_t actual_idx = (m_head + m_size - 1 - index) % m_capacity;
         // Handle potential negative result from subtraction before modulo if m_head + m_size - 1 - index < 0
        if (m_head + m_size <= index) { // Check if wrap around calculation is needed differently
             actual_idx = (m_head + m_size + m_capacity - 1 - index) % m_capacity;
        } else {
             actual_idx = (m_head + m_size - 1 - index) % m_capacity;
        }

        return m_data[actual_idx];
    }


    /**
     * @brief Calculates the sum of all elements currently in the buffer.
     * Uses the efficiently maintained running sum.
     * Thread-safe.
     * @return T The sum of the elements.
     */
    T sum() const {
        std::lock_guard<std::mutex> lock(m_mutex);
        // Ensure sum calculation is correct even for floating point types
        // For integers, m_sum is exact. For floats, it's an approximation.
        // Recalculating might be more accurate for floats but slower.
        // Let's trust the running sum for performance.
        return m_sum;
    }

    /**
     * @brief Calculates the mean (average) of all elements currently in the buffer.
     * Returns std::numeric_limits<T>::quiet_NaN() if the buffer is empty.
     * Thread-safe.
     * @return T The mean of the elements.
     */
    T mean() const {
        std::lock_guard<std::mutex> lock(m_mutex);
        if (m_size == 0) {
            return std::numeric_limits<T>::quiet_NaN(); // Or throw? Or return 0? NaN is common.
        }
        // Cast size to T to avoid potential integer division issues if T is float/double
        return m_sum / static_cast<T>(m_size);
    }

    /**
     * @brief Provides access to the underlying data in the order they were added (oldest to newest).
     * This involves copying data into a new vector. Use with caution on large buffers in hot paths.
     * Thread-safe.
     * @return std::vector<T> A vector containing the elements in chronological order.
     */
    std::vector<T> get_data_chronological() const {
        std::lock_guard<std::mutex> lock(m_mutex);
        std::vector<T> result;
        result.reserve(m_size);
        if (m_size == 0) {
            return result;
        }

        size_t current = m_head;
        for (size_t i = 0; i < m_size; ++i) {
            result.push_back(m_data[current]);
            current = (current + 1) % m_capacity;
        }
        return result;
    }


private:
    size_t m_capacity;           // Maximum number of elements
    std::vector<T> m_data;       // Underlying data storage
    size_t m_head;               // Index of the oldest element (next to be overwritten)
    size_t m_size;               // Current number of elements in the buffer
    T m_sum;                     // Running sum of elements (use double for precision if T is float)
    mutable std::mutex m_mutex;  // Mutex for thread safety
};
