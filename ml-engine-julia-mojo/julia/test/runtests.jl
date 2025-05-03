# MLEngine.jl/test/runtests.jl

using MLEngine
using Test
using Dates # For timestamps

@testset "MLEngine.jl Tests" begin
    # Write your tests here.

    # Example Test: DataManager basic add/retrieve
    @testset "DataManager" begin
        dm = MLEngine.DataManager()
        MLEngine.configure!(dm, 10) # Max history 10
        symbol_data = MLEngine.get_or_create_symbol_data(dm, "TEST_SYMBOL")

        # Add some market data
        ts1 = Dates.now()
        ts2 = ts1 + Dates.Second(1)
        ts3 = ts2 + Dates.Second(1)
        MLEngine.handle_market_data_event(market_data.MarketDataEvent(trade=market_data.Trade(symbol="TEST_SYMBOL", price="10.0", timestamp=google.protobuf.Timestamp(seconds=Dates.datetime2unix(ts1)))), dm)
        MLEngine.handle_market_data_event(market_data.MarketDataEvent(trade=market_data.Trade(symbol="TEST_SYMBOL", price="11.0", timestamp=google.protobuf.Timestamp(seconds=Dates.datetime2unix(ts2)))), dm)
        MLEngine.handle_market_data_event(market_data.MarketDataEvent(trade=market_data.Trade(symbol="TEST_SYMBOL", price="12.0", timestamp=google.protobuf.Timestamp(seconds=Dates.datetime2unix(ts3)))), dm)

        # Add some indicator data
        ts_ind1 = ts2 + Dates.Millisecond(500) # Indicator arrives between price points
        MLEngine.handle_indicator_value(indicator_data.IndicatorValue(symbol="TEST_SYMBOL", indicator_name="SMA_10", value="10.5", timestamp=google.protobuf.Timestamp(seconds=Dates.datetime2unix(ts_ind1))), dm)

        # Test get latest price
        latest_price_ts = MLEngine.get_latest_price_with_timestamp(dm, "TEST_SYMBOL")
        @test !isnothing(latest_price_ts)
        @test latest_price_ts[1] == 12.0
        @test latest_price_ts[2] == ts3

        # Test get recent prices
        recent_prices = MLEngine.get_recent_prices(dm, "TEST_SYMBOL", 2)
        @test length(recent_prices) == 2
        @test recent_prices == [11.0, 12.0]

        # Test get aligned data sequence
        # Requires at least N+1 data points for N lookback steps
        # Let's assume a dummy model requires 3 data points (lookback 3)
        # Sequence: [(ts1, 10.0), (ts2, 11.0), (ts3, 12.0)]
        # Indicators: SMA_10: [(ts_ind1, 10.5)]
        # Request sequence of 3 points ending at ts3.
        # Points needed: at ts1, ts2, ts3.
        # Aligned sequence should be:
        # [ (ts1, 10.0, indicator @<=ts1), (ts2, 11.0, indicator @<=ts2), (ts3, 12.0, indicator @<=ts3) ]
        # Indicator @<=ts1: None, use 0.0
        # Indicator @<=ts2: None, use 0.0
        # Indicator @<=ts3: (ts_ind1, 10.5), use 10.5
        # Expected sequence of feature vectors: [[10.0, 0.0], [11.0, 0.0], [12.0, 10.5]] (assuming 1 indicator)
        # Need to set up ModelManager and Config for this test

        # Mock ModelManager and Config for sequence test
        mock_mm = MLEngine.ModelManager()
        mock_config = MLEngine.Config(
             grpc = MLEngine.GrpcConfig("0.0.0.0:50000"),
             health_check = MLEngine.HealthCheckConfig("0.0.0.0:8000"),
             data = MLEngine.DataConfig(10),
             model = MLEngine.ModelConfig(instances=[MLEngine.ModelInstanceConfig(name="test_model", type="FluxLSTM", symbol="TEST_SYMBOL", required_data_points=3)]), # Requires 3 points
             predictions = MLEngine.PredictionsConfig(expose=[MLEngine.PredictionConfig(type="forecast", symbol="TEST_SYMBOL")]),
             training = MLEngine.TrainingConfig(),
             indicator_engine = MLEngine.IndicatorEngineConfig("localhost:50001")
         )
        mock_mm.model_configs = mock_config.model.instances
         mock_mm.prediction_configs = mock_config.predictions.expose # Need prediction configs to get required_data_points

        required_points = mock_mm.model_configs[1].required_data_points
        aligned_seq = MLEngine.get_aligned_data_sequence(dm, "TEST_SYMBOL", required_points, ts3)

        @test !isnothing(aligned_seq)
        @test length(aligned_seq) == required_points # Should get 3 points
        # Check values (order: price, SMA_10)
        @test aligned_seq[1] == [10.0, 0.0] # Assuming 0.0 for missing indicator
        @test aligned_seq[2] == [11.0, 0.0]
        @test aligned_seq[3] == [12.0, 10.5] # Indicator at ts_ind1 <= ts3

         # Test with not enough data for sequence
         aligned_seq_short = MLEngine.get_aligned_data_sequence(dm, "TEST_SYMBOL", 5, ts3) # Require 5 points, only have 3 prices
         @test isnothing(aligned_seq_short)

    end

    # Example Test: Basic Model Definition (Placeholder check)
     @testset "ModelDefinition" begin
        input_f = 2
        seq_len = 10
        hidden_s = 32
        output_f = 1
        model = MLEngine.build_lstm_model(input_f, seq_len, hidden_s, output_f)
        @test model isa Flux.Chain
        @test model[1] isa Flux.LSTM # Check first layer is LSTM
        @test model[end] isa Flux.Dense # Check last layer is Dense
        # You could run a dummy tensor through it to check shapes if needed
        # dummy_input = rand(Float32, input_f, seq_len, 1)
        # @test size(model(dummy_input)) == (output_f, 1)
     end


    # TODO: Add tests for:
    # - Config loading (success, invalid format, missing fields)
    # - ModelManager loading/saving (mock file system interactions or use temp files)
    # - Predictor logic (feed mock sequences, check output - challenging for complex models)
    # - Trainer logic (feed mock data, check if model parameters change, check save/load)
    # - gRPC server handlers (requires mocking gRPC streams)
    # - Indicator client (requires mocking gRPC server responses)
    # - Health check (check response codes based on mock Data/Model Manager state)
end