// src/lib/graphql/client.ts
import { GraphQLClient } from 'graphql-request';
import { PUBLIC_GRAPHQL_ENDPOINT } from '$env/static/public'; // Use SvelteKit environment variable

// Get the GraphQL endpoint URL from environment variables
// This needs to be set during build/runtime (e.g., in Dockerfile or .env file)
const endpoint = PUBLIC_GRAPHQL_ENDPOINT || 'http://localhost:8080/query'; // Default to BFF's common address/port

// Create a new GraphQL client instance
export const graphqlClient = new GraphQLClient(endpoint);

// Optional: Add headers for authentication etc.
// export const graphqlClient = new GraphQLClient(endpoint, { headers: { ... } });

console.log(`GraphQL client initialized for endpoint: ${endpoint}`);