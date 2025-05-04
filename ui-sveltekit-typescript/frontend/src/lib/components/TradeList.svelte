<script lang="ts">
	import type { Trade } from '$lib/graphql/generated/graphql';

	export let trades: Trade[];
</script>

<h3>Trade History</h3>

<table>
	<thead>
		<tr>
			<th>Timestamp</th>
			<th>Symbol</th>
			<th>Side</th>
			<th>Quantity</th>
			<th>Price</th>
			<th>Fee</th>
			<th>Realized P/L</th>
		</tr>
	</thead>
	<tbody>
		{#each trades as trade}
			<tr>
				<td>{trade.timestamp}</td>
				<td>{trade.symbol}</td>
				<td>{trade.side}</td>
				<td>{trade.quantity}</td>
				<td>{trade.price}</td>
				<td>{trade.fee} {trade.feeAsset}</td>
				<td>{trade.realizedPnl ?? '-'}</td> {#if realizedPnl is null/undefined, display -}
			</tr>
		{/each}
	</tbody>
</table>

<style>
	table {
		width: 100%;
		border-collapse: collapse;
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