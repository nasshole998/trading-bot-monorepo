<script lang="ts">
	import { onMount } from 'svelte';
	import Chart from 'chart.js/auto'; // Import Chart.js

	export let data: { t: Date; y: number }[]; // Expects data in Chart.js time series format

	let canvasElement: HTMLCanvasElement; // Bind to the canvas element

	onMount(() => {
		const ctx = canvasElement.getContext('2d');
		if (!ctx) {
			console.error('Failed to get canvas context');
			return;
		}

		const chart = new Chart(ctx, {
			type: 'line', // Line chart for equity curve
			data: {
				datasets: [{
					label: 'Equity',
					data: data,
					borderColor: 'rgb(75, 192, 192)',
					tension: 0.1,
					fill: false, // Don't fill area under the line
					pointRadius: 0, // Hide points
				}]
			},
			options: {
				responsive: true,
				maintainAspectRatio: false, // Allow control over chart size via CSS
				scales: {
					x: {
						type: 'time', // Use time scale for timestamps
						time: {
							unit: 'day' // Adjust unit based on backtest duration
							// tooltipFormat: 'YYYY-MM-DD HH:mm' // Format for tooltips
						},
						title: {
							display: true,
							text: 'Time'
						}
					},
					y: {
						title: {
							display: true,
							text: 'Equity'
						}
					}
				},
				plugins: {
					legend: {
						display: true // Show legend for the dataset
					},
					tooltip: {
						enabled: true // Enable tooltips on hover
					}
				}
			}
		});

		// Destroy chart on component unmount
		return () => chart.destroy();
	});
</script>

<div style="position: relative; height: 400px; width: 100%;">
    <canvas bind:this={canvasElement}></canvas>
</div>

<style>
	/* Styling for the chart container */
	div {
		margin-bottom: 1rem;
	}
</style>