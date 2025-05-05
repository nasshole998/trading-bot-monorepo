<script lang="ts">
	// Import the generated GraphQL OpenOrder type
	import type { OpenOrder } from '$lib/graphql/generated/graphql';

	export let orders: OpenOrder[];
</script>

<table>
	<thead>
		<tr>
			<th>Client ID</th>
            <th>Exchange ID</th>
			<th>Symbol</th>
			<th>Side</th>
			<th>Type</th>
			<th>Status</th>
			<th>Quantity</th>
			<th>Filled</th>
			<th>Price</th>
			<th>Placed At</th>
		</tr>
	</thead>
	<tbody>
		{#each orders as order}
			<tr>
				<td>{order.clientOrderId}</td>
                <td>{order.exchangeOrderId ?? '-'}</td>
				<td>{order.symbol}</td>
				<td>{order.side}</td>
				<td>{order.type}</td>
				<td>{order.status}</td>
				<td>{order.quantity}</td>
				<td>{order.filledQuantity}</td>
				<td>{order.price}</td>
				<td>{order.placedTime}</td>
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