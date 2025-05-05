<script lang="ts">
	import type { PageData } from './$types';
	import AccountSummary from '$lib/components/AccountSummary.svelte'; // Component for account summary (create this)
	import PositionsTable from '$lib/components/PositionsTable.svelte'; // Component for positions (create this)
	import OpenOrdersTable from '$lib/components/OpenOrdersTable.svelte'; // Component for open orders (create this)
	import RiskMetricsSummary from '$lib/components/RiskMetricsSummary.svelte'; // Component for risk metrics (create this)

	export let data: PageData;

	const { accountState, riskMetrics, error } = data;

	// Data needed for components
	const accountSummaryData = accountState ? {
		accountId: accountState.accountId,
		timestamp: accountState.timestamp,
		totalEquity: accountState.totalEquity,
		totalCapital: accountState.totalCapital,
	} : null;

	const positionsData = accountState?.positions;
	const openOrdersData = accountState?.openOrders;
	const riskMetricsData = riskMetrics; // Direct pass

</script>

<h1>Dashboard</h1>

{#if error}
	<p style="color: red;">{error}</p>
{:else}

	<h2>Account Summary</h2>
	{#if accountSummaryData}
		<AccountSummary data={accountSummaryData} />
	{:else}
		<p>Account summary not available.</p>
	{/if}

	<h2>Risk Metrics</h2>
	{#if riskMetricsData}
		<RiskMetricsSummary data={riskMetricsData} />
	{:else}
		<p>Risk metrics not available.</p>
	{/if}

	<h2>Current Positions</h2>
	{#if positionsData && positionsData.length > 0}
		<PositionsTable positions={positionsData} />
	{:else if positionsData}
		<p>No open positions.</p>
	{:else}
		<p>Position data not available.</p>
	{/if}


	<h2>Open Orders</h2>
	{#if openOrdersData && openOrdersData.length > 0}
		<OpenOrdersTable orders={openOrdersData} />
	{:else if openOrdersData}
		<p>No open orders.</p>
	{:else}
		<p>Open order data not available.</p>
	{/if}


{/if}


<style>
	h2 {
		margin-top: 1.5rem;
		margin-bottom: 0.8rem;
		border-bottom: 1px solid #eee;
		padding-bottom: 5px;
	}
</style>