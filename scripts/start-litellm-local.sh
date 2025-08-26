#!/usr/bin/env bash
# Start LiteLLM with local-only configuration

set -euo pipefail

# Color output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

echo -e "${BLUE}Starting LiteLLM with Local Configuration${NC}"
echo "=========================================="
echo ""

# Change to llm-gateway directory
cd "$(dirname "$0")/../llm-gateway"

# Check if Ollama is running on localhost
echo -e "${BLUE}Checking local services...${NC}"
if curl -s http://localhost:11434/api/tags >/dev/null 2>&1; then
    echo -e "${GREEN}✓${NC} Ollama is running on localhost"
    MODELS=$(curl -s http://localhost:11434/api/tags | jq -r '.models[]?.name' | paste -sd ', ' -)
    echo "  Models: $MODELS"
else
    echo -e "${YELLOW}⚠${NC} Ollama not running on localhost"
    echo "  Start with: ollama serve"
fi

# Check if Ollama is running on big72.local
if curl -s --connect-timeout 2 http://big72.local:11434/api/tags >/dev/null 2>&1; then
    echo -e "${GREEN}✓${NC} Ollama is running on big72.local"
    MODELS=$(curl -s http://big72.local:11434/api/tags | jq -r '.models[]?.name' | paste -sd ', ' -)
    echo "  Models: $MODELS"
else
    echo -e "${YELLOW}⚠${NC} Ollama not accessible on big72.local"
    echo "  Ensure Ollama is running with: OLLAMA_HOST=0.0.0.0:11434 ollama serve"
fi

echo ""
echo -e "${BLUE}Starting LiteLLM...${NC}"

# Check if running with Docker or Python
if command -v docker >/dev/null 2>&1 && docker info >/dev/null 2>&1; then
    echo "Using Docker Compose..."
    
    # Stop any existing instance
    docker compose --env-file .env.local down >/dev/null 2>&1 || true
    
    # Start with local config
    docker compose --env-file .env.local up -d
    
    # Wait for it to be ready
    echo -n "Waiting for LiteLLM to start"
    for i in {1..30}; do
        if curl -s http://localhost:4000/health/readiness >/dev/null 2>&1; then
            echo -e " ${GREEN}✓${NC}"
            break
        fi
        echo -n "."
        sleep 1
    done
    
elif command -v litellm >/dev/null 2>&1; then
    echo "Using Python LiteLLM..."
    
    # Kill any existing litellm process
    pkill -f "litellm.*--port 4000" || true
    sleep 1
    
    # Start in background
    export LITELLM_MASTER_KEY="sk-local-test-key-123"
    nohup litellm --config litellm/config-local.yaml --port 4000 > litellm.log 2>&1 &
    
    echo -n "Waiting for LiteLLM to start"
    for i in {1..30}; do
        if curl -s http://localhost:4000/health/readiness >/dev/null 2>&1; then
            echo -e " ${GREEN}✓${NC}"
            break
        fi
        echo -n "."
        sleep 1
    done
    
else
    echo -e "${RED}Error: Neither Docker nor litellm command found${NC}"
    echo "Install with: pip install 'litellm[proxy]'"
    exit 1
fi

# Check if LiteLLM is running
if curl -s http://localhost:4000/health/readiness >/dev/null 2>&1; then
    echo ""
    echo -e "${GREEN}✓ LiteLLM is running at http://localhost:4000${NC}"
    echo ""
    
    # List available models
    echo -e "${BLUE}Available models:${NC}"
    curl -s http://localhost:4000/v1/models \
        -H "Authorization: Bearer sk-local-test-key-123" | \
        jq -r '.data[]?.id' | head -10
    
    echo ""
    echo -e "${GREEN}LiteLLM started successfully!${NC}"
    echo ""
    echo "Access points:"
    echo "  API:       http://localhost:4000/v1"
    echo "  Health:    http://localhost:4000/health"
    echo "  UI:        http://localhost:4000/ui"
    echo "  Docs:      http://localhost:4000/docs"
    echo ""
    echo "Master key: sk-local-test-key-123"
    echo ""
    echo "Test with: cd scripts && MODEL=localhost-llama3 ./probe.sh"
else
    echo -e "${RED}✗ LiteLLM failed to start${NC}"
    echo "Check logs with:"
    echo "  Docker: docker compose logs litellm"
    echo "  Python: cat llm-gateway/litellm.log"
    exit 1
fi