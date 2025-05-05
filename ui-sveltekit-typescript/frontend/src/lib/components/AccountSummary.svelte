<script lang="ts">
    // Define the expected data structure
	export let data: {
        accountId: string;
        timestamp: string;
        totalEquity: string;
        totalCapital: string;
    };
</script>

<table>
	<tbody>
		<tr><td>Account ID</td><td>{data.accountId}</td></tr>
        <tr><td>Snapshot Time</td><td>{data.timestamp}</td></tr>
		<tr><td>Total Equity</td><td>{data.totalEquity}</td></tr>
		<tr><td>Available Capital</td><td>{data.totalCapital}</td></tr>
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