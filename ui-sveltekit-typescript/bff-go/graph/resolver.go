package graph

import (
	"context"
	"fmt"
	"log"
	"time"

	"google.golang.org/grpc/connectivity" // **NEW**
	"google.golang.org/protobuf/types/known/timestamppb"

	pb_backtester "trading-bot-monorepo/proto/backtester"
	pb_account "trading-bot-monorepo/proto/account_state"
	pb_risk "trading-bot-monorepo/proto/risk_management"

	"trading-bot-monorepo/ui-sveltekit-typescript/bff-go/pkg/backtesterclient"
	"trading-bot-monorepo/ui-sveltekit-typescript/bff-go/pkg/accountstateclient"
	"trading-bot-monorepo/ui-sveltekit-typescript/bff-go/pkg/riskmanagementclient"
	"trading-bot-monorepo/ui-sveltekit-typescript/bff-go/pkg/infrastructureclient" // **NEW**
	"trading-bot-monorepo/ui-sveltekit-typescript/bff-go/pkg/config"
)

// Resolver is the root resolver struct. It holds dependencies like gRPC clients and config.
type Resolver struct {
	BacktesterClient     *backtesterclient.Client
	AccountStateClient *accountstateclient.Client
	RiskManagementClient *riskmanagementclient.Client
	InfrastructureClient *infrastructureclient.Client // **NEW**
	Config               *config.AppConfig
}

// NewResolver creates a new root resolver instance
func NewResolver(
	backtesterClient *backtesterclient.Client,
	accountStateClient *accountstateclient.Client,
	riskManagementClient *riskmanagementclient.Client,
	infrastructureClient *infrastructureclient.Client, // **NEW**
	appConfig *config.AppConfig,
) *Resolver {
	return &Resolver{
		BacktesterClient:     backtesterClient,
		AccountStateClient: accountStateClient,
		RiskManagementClient: riskManagementClient,
		InfrastructureClient: infrastructureClient, // **NEW**
		Config:               appConfig,
	}
}


// Query returns the QueryResolver implementation.
func (r *Resolver) Query() QueryResolver {
	return &queryResolver{r}
}

type queryResolver struct{ *Resolver }


// listBacktests resolves the listBacktests query
func (r *queryResolver) ListBacktests(ctx context.Context) ([]*Backtest, error) {
	if r.BacktesterClient == nil {
		log.Println("Backtester client is not initialized")
		return nil, fmt.Errorf("backtester client not available")
	}
	summaries, err := r.BacktesterClient.ListBacktests(ctx)
	if err != nil {
		log.Printf("Error calling ListBacktests: %v", err)
		return nil, fmt.Errorf("failed to fetch backtest summaries: %w", err)
	}
	var backtests []*Backtest
	for _, summary := range summaries {
		backtests = append(backtests, &Backtest{
			ID: summary.Id, StrategyName: summary.StrategyName, Symbol: summary.Symbol,
			StartTime: formatTimestamp(summary.StartTime), EndTime: formatTimestamp(summary.EndTime),
			Success: summary.Success, ErrorMessage: summary.ErrorMessage,
		})
	}
	log.Printf("Resolved listBacktests query, returning %d backtests", len(backtests))
	return backtests, nil
}

// backtest resolves the backtest(id: ID!) query
func (r *queryResolver) Backtest(ctx context.Context, id string) (*Backtest, error) {
	if r.BacktesterClient == nil {
		log.Println("Backtester client is not initialized")
		return nil, fmt.Errorf("backtester client not available")
	}
	result, err := r.BacktesterClient.GetBacktestDetails(ctx, id)
	if err != nil {
		log.Printf("Error calling GetBacktestResult for ID %s: %v", id, err)
		return nil, fmt.Errorf("failed to fetch backtest details for ID %s: %w", id, err)
	}
	metrics := mapBacktestMetrics(result.GetMetrics())
	trades := mapTradeResults(result.GetTrades())
	equityCurve := mapEquityPoints(result.GetEquityCurve())
	backtest := &Backtest{
		ID: result.GetSummary().GetId(), StrategyName: result.GetSummary().GetStrategyName(), Symbol: result.GetSummary().GetSymbol(),
		StartTime: formatTimestamp(result.GetSummary().GetStartTime()), EndTime: formatTimestamp(result.GetSummary().GetEndTime()),
		Success: result.GetSummary().GetSuccess(), ErrorMessage: result.GetSummary().GetErrorMessage(),
		Metrics: metrics, Trades: trades, EquityCurve: equityCurve,
	}
	log.Printf("Resolved backtest query for ID %s", id)
	return backtest, nil
}

// accountState resolves the accountState(accountId: String!) query
func (r *queryResolver) AccountState(ctx context.Context, accountID string) (*AccountState, error) {
	if r.AccountStateClient == nil {
		log.Println("Account State client is not initialized")
		return nil, fmt.Errorf("account state client not available")
	}
	if accountID == "" && r.Config != nil { accountID = r.Config.DefaultAccountID }
	if accountID == "" { return nil, fmt.Errorf("account ID is required or must be set in config") }
	snapshot, err := r.AccountStateClient.GetAccountSnapshot(ctx, accountID)
	if err != nil {
		log.Printf("Error calling GetAccountSnapshot for account %s: %v", accountID, err)
		return nil, fmt.Errorf("failed to fetch account state for account %s: %w", accountID, err)
	}
	accountState := mapAccountState(snapshot)
	log.Printf("Resolved accountState query for account %s", accountID)
	return accountState, nil
}

// riskMetrics resolves the riskMetrics(accountId: String!) query
func (r *queryResolver) RiskMetrics(ctx context.Context, accountID string) (*RiskMetrics, error) {
	if r.RiskManagementClient == nil {
		log.Println("Risk Management client is not initialized")
		return nil, fmt.Errorf("risk management client not available")
	}
	if accountID == "" && r.Config != nil { accountID = r.Config.DefaultAccountID }
	if accountID == "" { return nil, fmt.Errorf("account ID is required or must be set in config") }
	snapshot, err := r.RiskManagementClient.GetRiskState(ctx, accountID)
	if err != nil {
		log.Printf("Error calling GetRiskState for account %s: %v", accountID, err)
		return nil, fmt.Errorf("failed to fetch risk metrics for account %s: %w", accountID, err)
	}
	riskMetrics := mapRiskMetrics(snapshot)
	log.Printf("Resolved riskMetrics query for account %s", accountID)
	return riskMetrics, nil
}


// **NEW:** systemStatus resolves the systemStatus query
func (r *queryResolver) SystemStatus(ctx context.Context) ([]*ServiceStatus, error) {
	statuses := []*ServiceStatus{}

	// Check status for each configured backend client
	// We get the connection state from the already initialized clients.
	if r.BacktesterClient != nil && r.BacktesterClient.conn != nil {
		status := r.BacktesterClient.conn.GetState()
		statuses = append(statuses, &ServiceStatus{
			Name: "Backtester",
			Address: r.BacktesterClient.GetAddress(),
			Status: status.String(), // Convert connectivity.State enum to string
			Error: "", // We don't get the error directly from GetState, would need more complex monitoring
		})
	} else {
         statuses = append(statuses, &ServiceStatus{Name: "Backtester", Address: r.Config.GrpcBackends.BacktesterAddress, Status: connectivity.Shutdown.String(), Error: "Client not initialized"})
    }

	if r.AccountStateClient != nil && r.AccountStateClient.conn != nil {
		status := r.AccountStateClient.conn.GetState()
		statuses = append(statuses, &ServiceStatus{
			Name: "DataIngestion (Account/Exec)",
			Address: r.AccountStateClient.GetAddress(),
			Status: status.String(),
			Error: "",
		})
	} else {
        statuses = append(statuses, &ServiceStatus{Name: "DataIngestion (Account/Exec)", Address: r.Config.GrpcBackends.DataIngestionAddress, Status: connectivity.Shutdown.String(), Error: "Client not initialized"})
    }


	if r.RiskManagementClient != nil && r.RiskManagementClient.conn != nil {
		status := r.RiskManagementClient.conn.GetState()
		statuses = append(statuses, &ServiceStatus{
			Name: "RiskManagement",
			Address: r.RiskManagementClient.GetAddress(),
			Status: status.String(),
			Error: "",
		})
	} else {
         statuses = append(statuses, &ServiceStatus{Name: "RiskManagement", Address: r.Config.GrpcBackends.RiskManagementAddress, Status: connectivity.Shutdown.String(), Error: "Client not initialized"})
    }


	// Check Infrastructure service status using its dedicated client
	if r.InfrastructureClient != nil && r.InfrastructureClient.conn != nil {
		status := r.InfrastructureClient.CheckConnectivity() // Use the client's method
		statuses = append(statuses, &ServiceStatus{
			Name: "Infrastructure (Config/Discovery)",
			Address: r.InfrastructureClient.GetAddress(),
			Status: status.String(),
			Error: "", // Simplified: Actual error would need more complex handling
		})
	} else {
         statuses = append(statuses, &ServiceStatus{Name: "Infrastructure (Config/Discovery)", Address: r.Config.GrpcBackends.InfrastructureAddress, Status: connectivity.Shutdown.String(), Error: "Client not initialized"})
    }


	// Add checks for other services if needed (e.g., ML Engine client)

	log.Println("Resolved systemStatus query")
	return statuses, nil
}


// Helper function to format protobuf Timestamp to string
func formatTimestamp(ts *timestamppb.Timestamp) string {
	if ts == nil { return "" }
	return ts.AsTime().Format(time.RFC3339)
}

// --- Mapping Helpers (from Message 3) ---
func mapBacktestMetrics(pbMetrics *pb_backtester.BacktestMetrics) *BacktestMetrics { /* ... */ return nil }
func mapTradeResults(pbTrades []*pb_backtester.TradeResult) []*Trade { /* ... */ return nil}
func mapEquityPoints(pbPoints []*pb_backtester.EquityPoint) []*EquityPoint { /* ... */ return nil }
func mapOrderSide(pbSide pb_backtester.OrderSide) OrderSide { /* ... */ return OrderSideUnknownOrderSide }
func mapOrderType(pbType pb_backtester.OrderType) OrderType { /* ... */ return OrderTypeUnknownOrderType }
func mapOrderStatus(pbStatus pb_backtester.OrderStatus) OrderStatus { /* ... */ return OrderStatusUnknownOrderStatus }
func mapAccountState(pbAccount *pb_account.AccountUpdate) *AccountState { /* ... */ return nil }
func mapRiskMetrics(pbRisk *pb_risk.RiskState) *RiskMetrics { /* ... */ return nil }

