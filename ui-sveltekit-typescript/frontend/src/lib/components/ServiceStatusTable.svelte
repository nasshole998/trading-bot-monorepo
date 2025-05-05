<script lang="ts">
	// Import the generated GraphQL ServiceStatus type
	import type { ServiceStatus } from '$lib/graphql/generated/graphql';

	export let statusList: ServiceStatus[];
</script>

<h3>Service Connectivity Status</h3>

<table>
	<thead>
		<tr>
			<th>Service Name</th>
			<th>Configured Address</th>
			<th>Status</th>
			<th>Error</th>
		</tr>
	</thead>
	<tbody>
		{#each statusList as serviceStatus}
			<tr>
				<td>{serviceStatus.name}</td>
				<td>{serviceStatus.address}</td>
				<td>{serviceStatus.status}</td>
				<td>{serviceStatus.error ?? '-'}</td> {# Display error if available }
			</tr>
		{/each}
	</tbody>
</table>

<style>
	table {
		width: 100%;
		border-collapse: collapse;
		margin-bottom: 1rem;
	}
	th, td {
		border: 1px solid #ddd;
		padding: 8px;
		text-align: left;
	}
	th {
		background-color: #f2f2f2;
	}
	/* Add styling for status colors if desired */
	/* td:nth-child(3) { font-weight: bold; } */
	/* td:nth-child(3):contains('READY') { color: green; } */
	/* td:nth-child(3):contains('TRANSIENT_FAILURE'), td:nth-child(3):contains('SHUTDOWN') { color: red; } */
</style>