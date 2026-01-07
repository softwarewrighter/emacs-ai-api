# LLM Gateway & Tools for Emacs

A comprehensive system for managing multiple LLM providers (cloud and local) through a unified interface, with special focus on Emacs integration, cost management, and per-project budgeting.

- [Video Explainer](https://www.youtube.com/watch?v=e1OoixGDMXs)

## Features

- **Unified Gateway**: Single endpoint for all LLM providers via LiteLLM
- **Multi-Provider Support**: OpenAI, Anthropic, Google Gemini, DeepSeek, local models (Ollama, llama.cpp)
- **Cost Management**: Per-project budgets with virtual keys
- **Emacs Integration**: Full gptel configuration with backend switching
- **MCP Support**: Model Context Protocol for tool use (Playwright, custom Rust servers)
- **CLI Tools**: Rust-based utilities for monitoring usage, costs, and status
- **Test Scripts**: Comprehensive testing for each provider

## Quick Start

### 1. Build Rust CLI Tools

```bash
# Build all Rust tools and servers
./scripts/build-rust.sh

# The following binaries will be available:
# - rust-wip/target/release/llm-status    - Check LLM provider status
# - rust-wip/target/release/llm-history   - View API usage history
# - rust-wip/target/release/llm-costs     - View cost summaries
# - rust-wip/target/release/llm-stats     - View usage statistics
# - rust-wip/target/release/llm-config    - Manage configuration
# - emacs-mcp-server/target/release/mcp-server - MCP server for Emacs
```

### 2. Setup Environment

```bash
# Clone and navigate to the project
cd emacs-ai-api/llm-gateway

# The project uses ~/.env for all API keys (centralized, secure)
# Edit ~/.env to add your API keys:
#   OPENAI_API_KEY=sk-...
#   ANTHROPIC_API_KEY=sk-ant-...
#   DEEPSEEK_API_KEY=sk-...
#   etc.

# The project .env is symlinked to ~/.env automatically
# If not, create the symlink:
ln -sf ~/.env .env
```

### 2. Start LiteLLM Gateway

```bash
# Start the gateway with Docker (recommended)
docker compose up -d

# Access the web UI
# http://localhost:4000/ui
# Login: admin / sk-local-test-key-123
```

### 3. Test the Setup

```bash
# Test gateway health
curl http://localhost:4000/health/readiness

# Test local Ollama models
./test-ollama-usage.sh

# Test OpenAI models
./test-openai-batch.sh

# Check usage
./check-usage.sh
```

### 5. Configure Emacs

Load the unified configuration:

```elisp
;; Single configuration for all providers
(load-file "~/path/to/llm-gateway/emacs/gptel-unified.el")
```

Press `C-c C-h` for comprehensive help after loading.

## Key Bindings (gptel-unified.el)

All commands use the consistent `C-c C-*` pattern:

| Key | Command | Description |
|-----|---------|-------------|
| **C-c C-h** | **Help** | **Comprehensive help - START HERE** |
| C-c C-m | Select model | Choose any model interactively |
| C-c C-1 | Best quality | Auto-select Claude/GPT-4o |
| C-c C-2 | Fast/cheap | Auto-select Haiku/GPT-4o-mini |
| C-c C-3 | Local | Auto-select Ollama models |
| C-c RET | Send | Send buffer/paragraph at point |
| C-c C-SPC | Send region | Send selected text |
| C-c C-n | New chat | Open new gptel buffer |
| C-c C-u | Usage | View costs and tokens |
| C-c C-r | Refresh | Re-discover models from LiteLLM |
| C-c C-l | List models | Show all available models |
| C-c C-? | Status | Show current configuration |

## Project Structure

```
emacs-ai-api/
  llm-gateway/
    docker-compose.yml        # LiteLLM + PostgreSQL setup
    .env.example             # Environment template
    .gitignore               # Excludes sensitive files
    litellm/
      config-openai.yaml     # Configuration with OpenAI
      config.yaml           # Base configuration
    emacs/
      gptel-unified.el      # Unified configuration for all providers
      gptel-ollama-selector.el # Dynamic Ollama selection
      subject.el            # Subject prompt helpers
    docs/
      testing-openai.md     # OpenAI testing guide
    check-usage.sh          # View usage statistics
    test-ollama-usage.sh    # Test Ollama models
    test-openai-batch.sh    # Test OpenAI models

  rust-wip/
    src/
      client.rs             # HTTP clients for LiteLLM, Ollama, llama.cpp
      config.rs             # Configuration management
      database.rs           # SQLite database operations
      models.rs             # Data models
      bin/
        status.rs           # llm-status: Check provider status
    Cargo.toml              # Rust dependencies

  emacs-mcp-server/
    src/
      mcp.rs                # MCP protocol implementation
      emacs.rs              # Emacs-specific tools
      tools.rs              # Available tools
    Cargo.toml              # Rust dependencies

  scripts/
    build-rust.sh           # Build Rust tools
    test-all.sh             # Test all providers
    llm-history.sh          # View recent API calls
    show-budget.sh          # View budget information
    check-usage.sh          # View usage statistics
```

## Rust CLI Tools

The project includes Rust-based CLI tools for managing LLM usage and monitoring provider status.

### Available Tools

#### llm-status
Check the status of all LLM providers and models.

```bash
# Check all providers
./rust-wip/target/release/llm-status

# Check specific provider
./rust-wip/target/release/llm-status --provider litellm

# Verbose output with all model details
./rust-wip/target/release/llm-status --verbose

# Output format: table, json, or simple
./rust-wip/target/release/llm-status --format json
```

Output example:
```
LLM Provider Status Check
=========================

Service      Status      Latency    Models         Endpoint
LiteLLM      ● Online    45ms       12 models      http://localhost:4000/v1
Ollama       ● Online    23ms       8 models       http://localhost:11434
llama.cpp    ✗ Offline   -          -              http://localhost:8080
```

#### llm-history
View API usage history from the database.

```bash
# View recent usage (default: 100 entries)
./rust-wip/target/release/llm-history

# Limit to specific number
./rust-wip/target/release/llm-history --limit 50

# Filter by project
./rust-wip/target/release/llm-history --project my-app

# JSON output
./rust-wip/target/release/llm-history --format json
```

#### llm-costs
View cost summaries for usage.

```bash
# View costs for today
./rust-wip/target/release/llm-costs --period today

# View costs for this week
./rust-wip/target/release/llm-costs --period week

# View costs by model
./rust-wip/target/release/llm-costs --by model

# Filter by project
./rust-wip/target/release/llm-costs --project my-app
```

#### llm-stats
Display usage statistics.

```bash
# Overall statistics
./rust-wip/target/release/llm-stats

# Statistics for specific provider
./rust-wip/target/release/llm-stats --provider openai

# Top models by usage
./rust-wip/target/release/llm-stats --top models

# Top projects by cost
./rust-wip/target/release/llm-stats --top projects
```

#### llm-config
Manage configuration settings.

```bash
# Show current configuration
./rust-wip/target/release/llm-config show

# Set default model
./rust-wip/target/release/llm-config set default.model gpt-4o

# Add project-specific virtual key
./rust-wip/target/release/llm-config add-project my-project --key sk-virt-123

# List all projects
./rust-wip/target/release/llm-config list-projects
```

### MCP Server for Emacs

The `emacs-mcp-server` provides Model Context Protocol support for Emacs tool use.

```bash
# Start the MCP server
./emacs-mcp-server/target/release/mcp-server

# With custom configuration
./emacs-mcp-server/target/release/mcp-server --config /path/to/config.toml
```

Available tools:
- **file operations**: read, write, list files
- **code analysis**: search, grep, analyze code
- **terminal operations**: execute commands

### Building from Source

```bash
# Build all Rust tools (release mode)
./scripts/build-rust.sh

# Build with debug information
cd rust-wip && cargo build
cd ../emacs-mcp-server && cargo build

# Build specific binary
cd rust-wip && cargo build --release --bin llm-status
```

### Database Configuration

The Rust tools use SQLite by default for storing usage data.

```bash
# Database location: ~/.local/share/llm-tools/history.db
# Or via environment variable: DATABASE_URL=sqlite:/custom/path.db

# Migration files: rust-wip/migrations/
# Schema includes: api_usage table with timestamps, costs, tokens
```

Configuration example (`~/.config/llm-tools/config.toml`):

```toml
[litellm]
base_url = "http://localhost:4000/v1"
master_key = "sk-local-test-key-123"

[database]
url = "sqlite:/Users/mike/.local/share/llm-tools/history.db"
history_retention_days = 90

[defaults]
model = "gpt-4o"
temperature = 0.7
max_tokens = 1024

[projects.my-app]
virtual_key = "sk-virt-myapp"
default_model = "gpt-4o-mini"
budget_limit = 10.0
budget_period = "monthly"
```

### Examples

#### Check all services status

```bash
./rust-wip/target/release/llm-status --format simple
```

Output:
```
✓ LiteLLM Gateway - 12 models available
✓ Ollama - 8 models available
✗ llama.cpp - Server not running (start with: ./server -m model.gguf)
```

#### Monitor usage in real-time

```bash
# Watch recent usage every 5 seconds
watch -n 5 './rust-wip/target/release/llm-history --limit 10 --format simple'
```

#### Cost report by project

```bash
./rust-wip/target/release/llm-costs --period month --by project
```

Output:
```
Cost Summary - 2025-01-01 to 2025-01-31
======================================

Total Cost: $45.23
Total Tokens: 1,234,567
Request Count: 342

By Project:
  my-app              $25.67
  documentation       $12.34
  testing             $7.22
```

## Configuration

### Model Names

The gateway now uses actual model names instead of confusing aliases:

- `llama3.2:latest` - Llama model on localhost
- `qwen2.5-coder:14b` - Qwen coder on localhost
- `qwen2.5:7b` - Qwen on big72
- `mistral:latest` - Mistral on big72
- `gpt-4o` - OpenAI GPT-4o
- `gpt-4o-mini` - OpenAI GPT-4o-mini (cheaper)
- `gpt-4-turbo` - OpenAI GPT-4-turbo

### Usage Report

The usage report (`C-c o u`) shows:
- Model name with location (e.g., `llama3.2:latest@localhost`)
- Provider and API endpoint
- Token counts (prompt + completion)
- Costs for paid models
- Filtered to show only actual usage (no 0-token entries)

## Documentation

### Available Documentation

- [OpenAI Testing Guide](llm-gateway/docs/testing-openai.md) - Comprehensive guide for testing OpenAI models
- [LiteLLM Configuration](llm-gateway/litellm/config-openai.yaml) - Model routing and configuration

## Cost Management

### Viewing Costs

1. **In Emacs**: `C-c o u` to see usage report with costs
2. **Web UI**: http://localhost:4000/ui (login: admin / sk-local-test-key-123)
3. **Command Line**: `./check-usage.sh`

### Cost Reference

Per 1M tokens:
- **GPT-4o-mini**: Input $0.15, Output $0.60
- **GPT-4o**: Input $5.00, Output $15.00
- **GPT-4-turbo**: Input $10.00, Output $30.00
- **Ollama models**: Free (local compute)

## Local Model Setup

### Ollama

```bash
# Install Ollama (if not installed)
# See: https://ollama.ai

# Pull models
ollama pull llama3.2:latest
ollama pull qwen2.5-coder:14b
ollama pull qwen2.5:7b
ollama pull mistral:latest

# Verify models
ollama list
```

## Troubleshooting

### LiteLLM Not Starting

```bash
# Check logs
docker compose logs litellm

# Restart
docker compose restart litellm

# Check health
curl http://localhost:4000/health
```

### Emacs Connection Issues

```elisp
;; Reload configuration
(load-file "path/to/gptel-openai.el")

;; Check current settings
C-c o ?

;; Test connection
C-c o c  ; Check available models
```

### Usage Not Showing

If usage data appears empty:
1. Ensure LiteLLM is running: `docker compose ps`
2. Check if requests are going through LiteLLM (port 4000), not directly to Ollama
3. Wait a moment for database to update
4. Refresh with `g` in the usage buffer

## Security Notes

- All API keys are stored in `~/.env` (outside project directory)
- Project `.env` is a symlink to `~/.env` (never committed)
- The `.gitignore` excludes all `.env.*` files except `.env.example`
- Use `sk-local-test-key-123` for local LiteLLM master key
- Keep your API keys secure and rotate if exposed
- One central location (`~/.env`) for all your API keys across projects

## License

MIT

## Support

For issues or questions, please open an issue on GitHub.
