<script lang="ts">
	import './styles.css'; // Basic global styles
</script>

<div class="layout">
	<header>
		<h1>Trading Bot UI</h1>
		<nav>
            <a href="/dashboard">Dashboard</a>
			<a href="/backtests">Backtests</a>
            <a href="/status">Status</a> </nav>
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
		color: #333;
	}

    nav a:hover {
        text-decoration: underline;
    }


	main {
		flex-grow: 1;
		padding: 1rem;
	}

	footer {
		background-color: #f0f0f0;
		padding: 1rem;
		text-align: center;
		margin-top: auto;
	}
</style>

<style>
	/* src/routes/styles.css */
	body {
		font-family: sans-serif;
		margin: 0;
	}
</style>