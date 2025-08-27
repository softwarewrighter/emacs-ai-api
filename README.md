# LLM Gateway & Tools for Emacs

A comprehensive system for managing multiple LLM providers (cloud and local) through a unified interface, with special focus on Emacs integration, cost management, and per-project budgeting.

## Features

- **Unified Gateway**: Single endpoint for all LLM providers via LiteLLM
- **Multi-Provider Support**: OpenAI, Anthropic, Google Gemini, DeepSeek, local models (Ollama, llama.cpp)
- **Cost Management**: Per-project budgets with virtual keys
- **Emacs Integration**: Full gptel configuration with backend switching
- **MCP Support**: Model Context Protocol for tool use (Playwright, custom Rust servers)
- **CLI Tools**: Rust-based utilities for monitoring usage, costs, and status
- **Test Scripts**: Comprehensive testing for each provider

## Quick Start

### 1. Setup Environment

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

### 4. Configure Emacs

Load the appropriate configuration file based on your needs:

```elisp
;; For OpenAI + local models via LiteLLM
(load-file "~/path/to/llm-gateway/emacs/gptel-openai.el")

;; For dynamic Ollama endpoint selection
(load-file "~/path/to/llm-gateway/emacs/gptel-ollama-selector.el")
```

## Key Bindings

### OpenAI Configuration (gptel-openai.el)

| Key Binding | Function | Description |
|------------|----------|-------------|
| `C-c o g` | gptel-use-gpt4o | Select GPT-4o model |
| `C-c o m` | gptel-use-gpt4o-mini | Select GPT-4o-mini (cheaper) |
| `C-c o c` | gptel-check-openai-models | List available OpenAI models |
| `C-c o s` | gptel-safe-send | Send with error handling |
| `C-c o u` | gptel-check-usage | View usage report |
| `C-c o ?` | gptel-show-current-config | Show current settings |

### Ollama Selector (gptel-ollama-selector.el)

| Key Binding | Function | Description |
|------------|----------|-------------|
| `C-c g L` | gptel-list-endpoints | List all endpoints |
| `C-c g l` | gptel-list-models | List models at current endpoint |
| `C-c g S l` | gptel-select-localhost | Select localhost Ollama |
| `C-c g S b` | gptel-select-big72 | Select big72 Ollama |
| `C-c g S L` | gptel-select-litellm | Select LiteLLM gateway |
| `C-c g s q` | gptel-select-qwen | Select Qwen model |
| `C-c g s m` | gptel-select-mistral | Select Mistral model |
| `C-c g RET` | gptel-send | Send to current model |

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
      gptel-openai.el       # OpenAI + unified gateway config
      gptel-ollama-selector.el # Dynamic Ollama selection
    docs/
      testing-openai.md     # OpenAI testing guide
    check-usage.sh          # View usage statistics
    test-ollama-usage.sh    # Test Ollama models
    test-openai-batch.sh    # Test OpenAI models
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