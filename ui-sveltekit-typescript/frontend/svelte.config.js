import adapter from '@sveltejs/adapter-auto';

/** @type {import('@sveltejs/kit').Config} */
const config = {
	kit: {
		// adapter-auto only supports some environments, see https://kit.svelte.dev/docs/adapters for a list.
		// If your environment is not supported, or you settle on a specific environment not supported by adapter-auto,
		// remove this comment and use the appropriate adapter.
		adapter: adapter()
	}
};

export default config;