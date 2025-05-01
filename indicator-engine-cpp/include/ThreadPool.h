// indicator-engine-cpp/include/ThreadPool.h
#pragma once

#include <vector>
#include <queue>
#include <thread>
#include <mutex>
#include <condition_variable>
#include <functional>
#include <future>
#include <stdexcept>
#include <memory>

#include "logging.h" // Include logging header

class ThreadPool {
public:
    /**
     * @brief Constructs a ThreadPool with a fixed number of worker threads.
     * @param num_threads The number of worker threads to create.
     */
    explicit ThreadPool(size_t num_threads) : m_stop(false) {
        if (num_threads == 0) {
            num_threads = std::thread::hardware_concurrency(); // Default to hardware concurrency
            if (num_threads == 0) {
                num_threads = 2; // Fallback if hardware concurrency detection fails
            }
        }
        LOG_INFO("Initializing ThreadPool with {} worker threads.", num_threads);

        m_workers.reserve(num_threads);
        for (size_t i = 0; i < num_threads; ++i) {
            m_workers.emplace_back([this] {
                while (true) {
                    std::function<void()> task;
                    {
                        std::unique_lock<std::mutex> lock(this->m_queue_mutex);
                        // Wait until there's a task or the pool is stopped
                        this->m_condition.wait(lock, [this] {
                            return this->m_stop || !this->m_tasks.empty();
                        });

                        // If stopped and no more tasks, exit the thread
                        if (this->m_stop && this->m_tasks.empty()) {
                            return;
                        }

                        // Get the next task from the queue
                        task = std::move(this->m_tasks.front());
                        this->m_tasks.pop();
                    } // Release lock before executing task

                    try {
                        task(); // Execute the task
                    } catch (const std::exception& e) {
                        LOG_ERROR("ThreadPool task threw an exception: {}", e.what());
                    } catch (...) {
                        LOG_ERROR("ThreadPool task threw an unknown exception.");
                    }
                }
            });
        }
    }

    /**
     * @brief Destructor. Joins all worker threads. Waits for pending tasks to complete.
     */
    ~ThreadPool() {
        {
            std::unique_lock<std::mutex> lock(m_queue_mutex);
            m_stop = true; // Signal workers to stop
        }
        m_condition.notify_all(); // Wake up all waiting threads

        // Join all worker threads
        for (std::thread& worker : m_workers) {
            if (worker.joinable()) {
                worker.join();
            }
        }
        LOG_INFO("ThreadPool destroyed.");
    }

    // Disable copy and assignment
    ThreadPool(const ThreadPool&) = delete;
    ThreadPool& operator=(const ThreadPool&) = delete;

    /**
     * @brief Enqueues a task to be executed by a worker thread.
     *
     * @tparam F The type of the function/callable object.
     * @tparam Args The types of the arguments to the function.
     * @param f The function or callable object to execute.
     * @param args The arguments to pass to the function.
     * @return std::future<typename std::result_of<F(Args...)>::type> A future to get the task's result.
     */
    template<class F, class... Args>
    auto enqueue(F&& f, Args&&... args)
        -> std::future<typename std::invoke_result_t<F, Args...>> // C++17 invoke_result
    {
        using return_type = typename std::invoke_result_t<F, Args...>;

        // Create a packaged_task to wrap the function and its arguments
        auto task = std::make_shared<std::packaged_task<return_type()>>(
            std::bind(std::forward<F>(f), std::forward<Args>(args)...)
        );

        // Get the future associated with the packaged_task
        std::future<return_type> res = task->get_future();

        { // Lock the queue to add the task
            std::unique_lock<std::mutex> lock(m_queue_mutex);

            // Don't allow enqueueing after stopping
            if (m_stop) {
                throw std::runtime_error("enqueue on stopped ThreadPool");
            }

            // Add the task (as a lambda that executes the packaged_task) to the queue
            m_tasks.emplace([task]() { (*task)(); });
        } // Release lock

        m_condition.notify_one(); // Notify one waiting worker thread
        return res;
    }

private:
    std::vector<std::thread> m_workers;           // Worker threads
    std::queue<std::function<void()>> m_tasks;    // Task queue

    std::mutex m_queue_mutex;                     // Mutex for queue access
    std::condition_variable m_condition;          // Condition variable for waiting threads
    bool m_stop;                                  // Flag to signal workers to stop
};
