use warp::Filter;
use tracing::info;
use crate::Result; // Use custom Result
use tokio_util::sync::CancellationToken;


/// Task to start a simple HTTP health check server.
pub async fn start_health_check_server_task(listen_addr: String, cancel_token: CancellationToken) -> Result<()> {
    let addr = listen_addr.parse()?;
    info!("Starting Health Check server on {}", addr);

    // Define a simple health check route
    let health_route = warp::path("healthz")
        .and(warp::get())
        .map(|| {
            // In a real application, check internal states here (e.g., are streams connected?)
            warp::reply::with_status("OK", warp::http::StatusCode::OK)
        });

    // Combine routes
    let routes = health_route;

    // Serve the routes with graceful shutdown
    let (_addr, server) = warp::serve(routes)
        .bind_with_graceful_shutdown(addr, cancel_token.cancelled());

    // Await the server to finish
    info!("Health Check server running.");
    server.await;
    info!("Health Check server stopped.");

    Ok(())
}