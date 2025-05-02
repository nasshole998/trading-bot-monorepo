use rust_decimal::Decimal;
use crate::Result; // Use the custom Result type
use std::str::FromStr;
use tracing::error;


// Helper to parse Decimal from string, returning an error on failure
pub fn parse_decimal(s: &str) -> Result<Decimal> {
    Decimal::from_str(s).map_err(Into::into) // Convert rust_decimal::Error to DataIngestionError
}

// Helper to format Decimal to string (standard to_string is fine, but wrap if needed)
pub fn format_decimal(d: Decimal) -> String {
    d.to_string()
}

// Add URL encoding helper from http and form_urlencoded
pub mod url_encoding {
    use form_urlencoded::Serializer;

    pub fn form_urlencoded(params: impl IntoIterator<Item = (String, String)>) -> String {
        let mut serializer = Serializer::new(String::new());
        serializer.extend_pairs(params);
        serializer.finish()
    }
}

// Add a simple placeholder for rate limiting key (governor uses Identity by default)
// A more complex key could be per endpoint or per exchange weight category.
// struct BinanceRateLimitKey;