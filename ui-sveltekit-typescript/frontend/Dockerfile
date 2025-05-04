# Use a Node.js base image for building the SvelteKit app
FROM node:20-bookworm as builder

WORKDIR /app/frontend

# Copy package.json and package-lock.json (if you use it)
COPY ui-sveltekit-typescript/frontend/package*.json ./

# Install dependencies
RUN npm install

# Copy the rest of the frontend source code
COPY ui-sveltekit-typescript/frontend/ ./

# --- Build Steps ---
# Generate GraphQL code (requires the schema from the running BFF)
# This step requires the BFF to be accessible during the build.
# In a real CI/CD, you might download the schema first or build BFF then frontend.
# For simplicity here, we'll assume localhost:8080 is accessible during build.
# Or, you can pre-generate this locally and copy the generated file.
# Let's assume pre-generation locally for simplicity in this Dockerfile.
# If you prefer generating in Docker, you'd add steps to wait for BFF or copy schema.graphqls.

# Build the SvelteKit application
# This command outputs to the .svelte-kit/output directory
RUN npm run build

# --- Stage 2: Create a smaller runtime image ---
# Use a minimal Nginx or Caddy image to serve the static files, or Node.js again
FROM nginx:stable-alpine as runner

# Copy built application files from builder stage
# The adapter (adapter-auto) typically outputs to .svelte-kit/output/<adapter-name>
# We need to check the output directory depending on the adapter used.
# For adapter-static, it's typically 'build'.
# Let's assume 'build' directory based on adapter-auto often defaulting to static-like output for simple apps.
COPY --from=builder /app/frontend/build /usr/share/nginx/html

# Copy default nginx configuration (optional, default is fine for static serving)
# COPY nginx.conf /etc/nginx/nginx.conf

# Expose the port SvelteKit serves on (default for adapter-auto often uses Node.js on 3000 or serves static on 80/8080)
# Nginx default is 80
EXPOSE 80

# Command to run Nginx
CMD ["nginx", "-g", "daemon off;"]

# If using adapter-node:
# FROM node:20-bookworm as runner
# WORKDIR /app/frontend
# COPY --from=builder /app/frontend/.svelte-kit/output .
# CMD node index.js # Or your adapter's entrypoint
# EXPOSE 3000 # Default for adapter-node