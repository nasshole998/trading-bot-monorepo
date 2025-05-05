<script lang="ts">
	import type { PageData } from './$types';
	import ServiceStatusTable from '$lib/components/ServiceStatusTable.svelte'; // Component for status table (create this)

	export let data: PageData;

	const { systemStatus, error } = data;
</script>

<h1>System Status</h1>

{#if error}
	<p style="color: red;">{error}</p>
{:else if systemStatus && systemStatus.length > 0}
	<ServiceStatusTable statusList={systemStatus} />
{:else}
	<p>System status data not available.</p>
{/if}

<style>
	/* Add specific styles for this page if needed */
</style>