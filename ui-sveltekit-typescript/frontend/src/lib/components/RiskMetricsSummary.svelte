<script lang="ts">
	// Import the generated GraphQL RiskMetrics type
	import type { RiskMetrics } from '$lib/graphql/generated/graphql';

	export let data: RiskMetrics;
</script>

<table>
	<tbody>
		<tr><td>Account ID</td><td>{data.accountId}</td></tr>
		<tr><td>Snapshot Time</td><td>{data.timestamp}</td></tr>
		<tr><td>Trading Halted</td><td>{data.tradingHalted ? 'Yes' : 'No'}</td></tr>
		<tr><td>Total Equity</td><td>{data.totalEquity}</td></tr>
		<tr><td>Peak Equity</td><td>{data.peakEquity}</td></tr>
		<tr><td>Total Realized Loss</td><td>{data.totalRealizedLoss}</td></tr>
		<tr><td>Daily Realized Loss</td><td>{data.dailyRealizedLoss}</td></tr>
		<tr><td>Last Daily Loss Reset</td><td>{data.lastDailyLossReset}</td></tr>
		<tr><td>Total Net Exposure (Quote)</td><td>{data.currentTotalNetExposureQuote}</td></tr>
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