<script lang="ts">
    // Import the generated GraphQL Metrics type for type safety
	import type { BacktestMetrics } from '$lib/graphql/generated/graphql';

	export let metrics: BacktestMetrics;
</script>

<h3>Metrics Summary</h3>

<table>
	<tbody>
		<tr><td>Total Profit</td><td>{metrics.totalProfit}</td></tr>
		<tr><td>Sharpe Ratio</td><td>{metrics.sharpeRatio}</td></tr>
		<tr><td>Max Drawdown</td><td>{metrics.maxDrawdown}</td></tr>
		<tr><td>Win Rate</td><td>{metrics.winRate}</td></tr>
		<tr><td>Total Trades</td><td>{metrics.totalTrades}</td></tr>
		</tbody>
</table>

<style>
	table {
		border-collapse: collapse;
		margin-bottom: 1rem;
	}
	td {
		border: 1px solid #ddd;
		padding: 6px 10px;
	}
	td:first-child {
		font-weight: bold;
		background-color: #f8f8f8;
	}
</style>