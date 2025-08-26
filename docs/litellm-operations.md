# LiteLLM Operations Guide

Complete guide for starting, configuring, and managing LiteLLM with quotas, budgets, and monitoring.

## Table of Contents
1. [Starting LiteLLM](#starting-litellm)
2. [Adding Providers & Models](#adding-providers--models)
3. [Budget & Quota Management](#budget--quota-management)
4. [Cost Monitoring](#cost-monitoring)
5. [Common Operations](#common-operations)
6. [Troubleshooting](#troubleshooting)

## Starting LiteLLM

### Method 1: Docker Compose (Recommended)

```bash
cd llm-gateway

# First time setup
cp .env.example .env
# Edit .env with your settings

# Start all services
docker compose up -d

# Check status
docker compose ps

# View logs
docker compose logs -f litellm

# Stop services
docker compose down

# Stop and remove data (careful!)
docker compose down -v
```

### Method 2: Direct Python

```bash
# Install LiteLLM
pip install "litellm[proxy,extra]"

# Start with config file
export LITELLM_MASTER_KEY="sk-admin-yoursecretkey"
litellm --config llm-gateway/litellm/config.yaml --port 4000

# Start with database support
export DATABASE_URL="postgresql://user:pass@localhost/litellm"
litellm --config config.yaml --port 4000 --detailed_debug
```

### Method 3: Systemd Service (Linux)

```bash
# Create service file
sudo tee /etc/systemd/system/litellm.service > /dev/null <<EOF
[Unit]
Description=LiteLLM Gateway
After=network.target postgresql.service

[Service]
Type=simple
User=$USER
WorkingDirectory=/home/$USER/llm-gateway
Environment="LITELLM_MASTER_KEY=sk-admin-yoursecretkey"
Environment="DATABASE_URL=postgresql://litellm:password@localhost/litellm"
ExecStart=/usr/local/bin/litellm --config litellm/config.yaml --port 4000
Restart=on-failure
RestartSec=10

[Install]
WantedBy=multi-user.target
EOF

# Enable and start
sudo systemctl daemon-reload
sudo systemctl enable litellm
sudo systemctl start litellm
sudo systemctl status litellm
```

## Adding Providers & Models

### 1. Add Local URL (Ollama/llama.cpp)

Edit `llm-gateway/litellm/config.yaml`:

```yaml
model_list:
  # Add new local Ollama instance
  - model_name: my-local-ollama
    litellm_params:
      model: ollama/mistral:latest
      api_base: http://192.168.1.50:11434  # Your Ollama URL
      api_key: sk-local                     # Dummy key
      input_cost_per_token: 0.0            # Free!
      output_cost_per_token: 0.0

  # Add llama.cpp server
  - model_name: my-llamacpp
    litellm_params:
      model: local-model
      api_base: http://192.168.1.51:8080/v1
      api_key: sk-local
      input_cost_per_token: 0.0
      output_cost_per_token: 0.0
```

After editing, restart LiteLLM:
```bash
# Docker
docker compose restart litellm

# Or send reload signal (if supported)
docker compose exec litellm kill -HUP 1
```

### 2. Add Cloud Provider Key

#### Via Environment (.env file):
```bash
# Edit .env
OPENAI_API_KEY=sk-proj-xxxxx
ANTHROPIC_API_KEY=sk-ant-xxxxx
DEEPSEEK_API_KEY=sk-xxxxx

# Restart to apply
docker compose up -d
```

#### Via Config (config.yaml):
```yaml
model_list:
  - model_name: my-gpt4
    litellm_params:
      model: openai/gpt-4o
      api_key: sk-proj-xxxxx  # Direct in config
```

#### Via API (Runtime):
```bash
# Add a new deployment via API
curl -X POST http://localhost:4000/model/new \
  -H "Authorization: Bearer $LITELLM_MASTER_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "model_name": "gpt-4-new",
    "litellm_params": {
      "model": "openai/gpt-4o",
      "api_key": "sk-proj-xxxxx"
    }
  }'
```

### 3. Test New Model

```bash
# Quick test
cd scripts
MODEL=my-local-ollama ./probe.sh

# Or with curl
curl http://localhost:4000/v1/chat/completions \
  -H "Authorization: Bearer $LITELLM_MASTER_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "model": "my-local-ollama",
    "messages": [{"role": "user", "content": "Hello"}]
  }'
```

## Budget & Quota Management

### 1. Provider-Level Budgets

Set spending limits per provider in `config.yaml`:

```yaml
router_settings:
  provider_budget_config:
    openai:
      budget_limit: 100.0      # $100
      time_period: 30d         # per 30 days
    anthropic:
      budget_limit: 50.0       
      time_period: 1mo         # per month
    local:                     # Custom provider name
      budget_limit: 0.0        # Unlimited for local
      time_period: 1d
```

### 2. Model-Level Budgets

Set per-model limits:

```yaml
model_list:
  - model_name: gpt-4o
    litellm_params:
      model: openai/gpt-4o
      api_key: os.environ/OPENAI_API_KEY
      max_budget: 20.0         # $20 max for this model
      budget_duration: 1d      # per day
```

### 3. User/Project Budgets (Virtual Keys)

Create budget-limited keys for projects:

```bash
# Create a project key with $5/day limit
cd scripts
./gen-virtual-key.sh "mobile-app" 5 "1d" "gpt-4o-mini,claude-haiku"

# The script creates keys/mobile-app.env with:
# LITELLM_VIRTUAL_KEY=sk-xxx
# This key can only spend $5/day on specified models
```

### 4. Set Quotas via API

```bash
# Set user budget
curl -X POST http://localhost:4000/user/budget \
  -H "Authorization: Bearer $LITELLM_MASTER_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "user_id": "project-123",
    "max_budget": 10.0,
    "budget_duration": "1d"
  }'

# Update model budget
curl -X POST http://localhost:4000/model/update \
  -H "Authorization: Bearer $LITELLM_MASTER_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "model_name": "gpt-4o",
    "max_budget": 50.0,
    "budget_duration": "7d"
  }'
```

### 5. Rate Limiting

Add rate limits in `config.yaml`:

```yaml
model_list:
  - model_name: gpt-4o
    litellm_params:
      model: openai/gpt-4o
      api_key: os.environ/OPENAI_API_KEY
      rpm: 100            # Requests per minute
      tpm: 100000         # Tokens per minute
```

## Cost Monitoring

### 1. Check Current Spend

```bash
# Via script
cd scripts
./show-budget.sh

# Via API
curl http://localhost:4000/spend/total \
  -H "Authorization: Bearer $LITELLM_MASTER_KEY"

# Provider budgets
curl http://localhost:4000/provider/budgets \
  -H "Authorization: Bearer $LITELLM_MASTER_KEY"
```

### 2. Monitor from Emacs

```elisp
;; Add to Emacs config
(defun llm-check-spend ()
  "Check current LiteLLM spend."
  (interactive)
  (let* ((url "http://localhost:4000/spend/total")
         (auth (concat "Bearer " (getenv "LITELLM_MASTER_KEY")))
         (response (with-current-buffer
                      (url-retrieve-synchronously url nil t 5)
                    (goto-char (point-min))
                    (re-search-forward "^$" nil 'move)
                    (json-read-from-string 
                     (buffer-substring-no-properties (point) (point-max)))))
         (spend (cdr (assoc 'total_spend response))))
    (message "Total spend: $%.2f" spend)))

;; Bind to key
(global-set-key (kbd "C-c L $") 'llm-check-spend)
```

### 3. View Usage Dashboard

Access the built-in UI:
```bash
# Open in browser
open http://localhost:4000/ui

# Default login:
# Username: admin
# Password: [your LITELLM_MASTER_KEY]
```

### 4. Export Usage Data

```bash
# Get usage for date range
curl "http://localhost:4000/spend/logs?start_date=2024-01-01&end_date=2024-01-31" \
  -H "Authorization: Bearer $LITELLM_MASTER_KEY" \
  > usage_january.json

# Get per-model breakdown
curl http://localhost:4000/model/metrics \
  -H "Authorization: Bearer $LITELLM_MASTER_KEY"
```

## Common Operations

### 1. Add a New Model on the Fly

```bash
# Without restarting LiteLLM
curl -X POST http://localhost:4000/model/new \
  -H "Authorization: Bearer $LITELLM_MASTER_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "model_name": "temp-model",
    "litellm_params": {
      "model": "openai/gpt-3.5-turbo",
      "api_key": "sk-xxx"
    }
  }'
```

### 2. Disable/Enable a Model

```bash
# Disable a model
curl -X POST http://localhost:4000/model/disable \
  -H "Authorization: Bearer $LITELLM_MASTER_KEY" \
  -H "Content-Type: application/json" \
  -d '{"model_name": "gpt-4o"}'

# Re-enable
curl -X POST http://localhost:4000/model/enable \
  -H "Authorization: Bearer $LITELLM_MASTER_KEY" \
  -H "Content-Type: application/json" \
  -d '{"model_name": "gpt-4o"}'
```

### 3. Clear Budget/Reset Spend

```bash
# Reset user budget
curl -X POST http://localhost:4000/user/reset_spend \
  -H "Authorization: Bearer $LITELLM_MASTER_KEY" \
  -H "Content-Type: application/json" \
  -d '{"user_id": "project-123"}'
```

### 4. Backup Configuration

```bash
# Backup config and keys
cd llm-gateway
tar -czf backup-$(date +%Y%m%d).tar.gz \
  litellm/config.yaml \
  .env \
  keys/

# Backup database (if using PostgreSQL)
docker compose exec postgres pg_dump -U litellm > backup-$(date +%Y%m%d).sql
```

## Troubleshooting

### LiteLLM Won't Start

```bash
# Check port availability
lsof -i :4000

# Check config syntax
litellm --config litellm/config.yaml --test

# Run in debug mode
docker compose up litellm  # No -d flag, see output
```

### Model Not Working

```bash
# Test model directly
curl http://localhost:4000/v1/models | jq '.data[].id'

# Check model health
curl http://localhost:4000/model/info \
  -H "Authorization: Bearer $LITELLM_MASTER_KEY" \
  -H "Content-Type: application/json" \
  -d '{"model": "model-name"}'
```

### Budget Not Enforcing

```bash
# Check if budget tracking is enabled
curl http://localhost:4000/config \
  -H "Authorization: Bearer $LITELLM_MASTER_KEY"

# Verify database connection
docker compose exec litellm \
  python -c "import os; print(os.environ.get('DATABASE_URL'))"
```

### High Latency

```bash
# Check model latencies
curl http://localhost:4000/model/metrics \
  -H "Authorization: Bearer $LITELLM_MASTER_KEY" | jq

# Test routing strategy
# Edit config.yaml:
router_settings:
  routing_strategy: simple-shuffle  # Test without smart routing
```

## Quick Reference

### Essential Commands

```bash
# Start
docker compose up -d

# Stop
docker compose down

# Restart after config change
docker compose restart litellm

# View logs
docker compose logs -f litellm

# Test health
curl http://localhost:4000/health

# List models
curl http://localhost:4000/v1/models

# Check spend
./scripts/show-budget.sh

# Create project key
./scripts/gen-virtual-key.sh "project" 10 "1d"

# Test model
MODEL=model-name ./scripts/probe.sh
```

### Key Files

- `llm-gateway/.env` - Environment variables and API keys
- `llm-gateway/litellm/config.yaml` - Model and routing configuration  
- `keys/*.env` - Virtual keys for projects
- `scripts/show-budget.sh` - Budget monitoring
- `scripts/gen-virtual-key.sh` - Create budget-limited keys

### Useful URLs

- Health: http://localhost:4000/health
- UI Dashboard: http://localhost:4000/ui
- Models: http://localhost:4000/v1/models
- Metrics: http://localhost:4000/metrics
- API Docs: http://localhost:4000/docs