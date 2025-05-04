<script lang="ts">
	import type { PageData } from './$types';
	import MetricsTable from '$lib/components/MetricsTable.svelte'; // Component for metrics (create this)
	import TradeList from '$lib/components/TradeList.svelte';     // Component for trades (create this)
	import EquityChart from '$lib/components/EquityChart.svelte'; // Component for equity chart (create this)

	export let data: PageData;

	const { backtestId, details, error } = data;

	// Data needed for the EquityChart component
	// Transform equityCurve data if needed for the chart library
	let chartData = [];
	if (details && details.equityCurve) {
		chartData = details.equityCurve.map(point => ({
			// Chart.js often uses {x: value, y: value} or [x, y] or just y array with labels
			// Using objects {t: time, y: equity} is common for time series charts
			t: new Date(point.timestamp), // Parse timestamp string into Date object
			y: parseFloat(point.equity) // Parse equity string into number
		}));
		// Ensure data is sorted by timestamp if necessary for the chart library
		chartData.sort((a, b) => a.t.getTime() - b.t.getTime());
	}

	// Data needed for the MetricsTable and TradeList components
	const metrics = details?.metrics;
	const trades = details?.trades;

</script>

<h1>Backtest Details: {backtestId}</h1>

{#if error}
	<p style="color: red;">{error}</p>
{:else if details}
	<p>Strategy: <strong>{details.strategyName}</strong></p>
	<p>Symbol: <strong>{details.symbol}</strong></p>
	<p>Timeframe: {details.startTime} to {details.endTime}</p>
	{#if details.success}
		<p style="color: green;">Status: <strong>Success</strong></p>
	{:else}
		<p style="color: red;">Status: <strong>Failed</strong> - {details.errorMessage}</p>
	{/if}

	<h2>Performance</h2>

	{#if metrics}
		<MetricsTable {metrics} />
	{:else}
		<p>Metrics not available.</p>
	{/if}

	<h2>Equity Curve</h2>

	{#if chartData.length > 0}
		<EquityChart data={chartData} />
	{:else}
		<p>Equity curve data not available.</p>
	{/if}

	<h2>Trades</h2>

	{#if trades && trades.length > 0}
		<TradeList {trades} />
	{:else}
		<p>No trades executed during this backtest.</p>
	{/if}


{:else}
	<p>Loading backtest details...</p>
{/if}

<style>
	/* Add specific styles for this page if needed */
	h2 {
		margin-top: 1.5rem;
		margin-bottom: 0.8rem;
	}
</style>