<script lang="ts">
	// Import the generated GraphQL Position type
	import type { Position } from '$lib/graphql/generated/graphql';

	export let positions: Position[];
</script>

<table>
	<thead>
		<tr>
			<th>Symbol</th>
			<th>Side</th>
			<th>Quantity</th>
			<th>Avg Entry Price</th>
			<th>Unrealized P/L</th>
		</tr>
	</thead>
	<tbody>
		{#each positions as position}
			<tr>
				<td>{position.symbol}</td>
				<td>{position.side}</td>
				<td>{position.quantity}</td>
				<td>{position.avgEntryPrice}</td>
				<td>{position.unrealizedPnl}</td>
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
</style>