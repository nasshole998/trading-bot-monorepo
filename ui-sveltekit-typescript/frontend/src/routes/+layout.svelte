<script lang="ts">
	import './styles.css'; // Basic global styles (create this file)
</script>

<div class="layout">
	<header>
		<h1>Trading Bot UI</h1>
		<nav>
			<a href="/backtests">Backtests</a>
			</nav>
	</header>

	<main>
		<slot />
	</main>

	<footer>
		<p>&copy; 2025 Trading Bot</p>
	</footer>
</div>

<style>
	/* Basic layout styling */
	.layout {
		display: flex;
		flex-direction: column;
		min-height: 100vh;
	}

	header {
		background-color: #f0f0f0;
		padding: 1rem;
		text-align: center;
	}

	nav a {
		margin: 0 10px;
		text-decoration: none;
	}

	main {
		flex-grow: 1; /* Main content takes up available space */
		padding: 1rem;
	}

	footer {
		background-color: #f0f0f0;
		padding: 1rem;
		text-align: center;
		margin-top: auto; /* Push footer to the bottom */
	}
</style>

<style>
	/* src/routes/styles.css */
	body {
		font-family: sans-serif;
		margin: 0;
	}
</style>