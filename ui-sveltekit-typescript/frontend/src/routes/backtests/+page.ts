// src/routes/backtests/+page.ts
import { graphqlClient } from '$lib/graphql/client';
import { GetBacktestDetailsDocument } from '$lib/graphql/generated/graphql';
import type { PageLoad } from './$types'; // This import will work after 'npm run check'

export const load: PageLoad = async ({ params, fetch }) => { // 'params' and 'fetch' will be typed correctly after 'npm run check'
  const backtestId = params.id; // Get the ID from the route parameters

  try {
    // Fetch detailed data from the GraphQL endpoint using the ID variable
    // We use the generated document (GetBacktestDetailsDocument) and pass the variable object.
    const data = await graphqlClient.request(GetBacktestDetailsDocument, { backtestId });

    // data will contain { backtest: Backtest | null | undefined } based on the schema and resolver.
    // Handle potential null/undefined results if backtest not found.

    if (!data || !data.backtest) {
        // Handle case where backtest with this ID is not found or data fetching failed structurally
         console.warn(`Backtest with ID "${backtestId}" not found or data is incomplete.`);
         return { // Return data structure expected by +page.svelte
            backtestId,
            error: `Backtest with ID "${backtestId}" not found or failed to load.`,
            details: null // Explicitly set details to null
         };
    }


    return { // Return data structure expected by +page.svelte
      backtestId, // Pass the ID back
      details: data.backtest, // Pass the detailed backtest data
      error: null // No loading error
    };
  } catch (error: any) { // Catch potential network or parsing errors
    console.error(`Error fetching backtest details for ID "${backtestId}":`, error);
    return { // Return data structure expected by +page.svelte
      backtestId,
      error: `Failed to load backtest details for ID "${backtestId}". Reason: ${error.message || error}`,
      details: null
    };
  }
};