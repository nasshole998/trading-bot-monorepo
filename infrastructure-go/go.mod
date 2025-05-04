module trading-bot-monorepo/infrastructure-go

go 1.22

require (
	google.golang.org/grpc v1.63.0
	google.golang.org/protobuf v1.33.0
	gopkg.in/yaml.v3 v3.0.1 # For loading infrastructure config and other service configs
)

require (
	github.com/go-yaml/yaml v2.1.0+incompatible // indirect
	golang.org/x/net v0.21.0 // indirect
	golang.org/x/sys v0.17.0 // indirect
	golang.org/x/text v0.14.0 // indirect
	google.golang.org/genproto/googleapis/rpc v0.0.0-20240227224415-6ceb2ff114de // indirect
)

// Optional: Replace paths for local development if needed
// replace trading-bot-monorepo/proto => ../proto