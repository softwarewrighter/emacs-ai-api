#!/usr/bin/env bash
# Verify complete local setup: Start LiteLLM and test both Ollama instances

set -euo pipefail

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

echo -e "${BLUE}═══════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}     Local LLM Setup Verification Script${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════${NC}"
echo ""

# Change to scripts directory
cd "$(dirname "$0")"

# Step 1: Check prerequisites
echo -e "${BLUE}Step 1: Checking prerequisites...${NC}"
echo ""

# Check localhost Ollama
if curl -s http://localhost:11434/api/tags >/dev/null 2>&1; then
    echo -e "${GREEN}✓${NC} Ollama is running on localhost"
    LOCALHOST_MODELS=$(curl -s http://localhost:11434/api/tags | jq -r '.models[]?.name' | paste -sd ', ' -)
    echo "  Models: $LOCALHOST_MODELS"
else
    echo -e "${RED}✗${NC} Ollama not running on localhost"
    echo "  Please start with: ollama serve"
    echo ""
    echo "Continuing anyway..."
fi

# Check big72.local Ollama
echo ""
if curl -s --connect-timeout 2 http://big72.local:11434/api/tags >/dev/null 2>&1; then
    echo -e "${GREEN}✓${NC} Ollama is running on big72.local"
    BIG72_MODELS=$(curl -s http://big72.local:11434/api/tags | jq -r '.models[]?.name' | paste -sd ', ' -)
    echo "  Models: $BIG72_MODELS"
    
    # Update config with actual big72 models
    if [ -n "$BIG72_MODELS" ]; then
        FIRST_MODEL=$(echo $BIG72_MODELS | cut -d',' -f1 | xargs)
        echo "  Using model: $FIRST_MODEL"
        # Update the config file to use the actual model
        sed -i.bak "s|ollama/llama3.2:latest  # Adjust based on what's on big72|ollama/$FIRST_MODEL|" \
            ../llm-gateway/litellm/config-local.yaml 2>/dev/null || true
    fi
else
    echo -e "${YELLOW}⚠${NC} Ollama not accessible on big72.local"
    echo "  Make sure Ollama is running with:"
    echo "  ssh big72.local 'OLLAMA_HOST=0.0.0.0:11434 ollama serve'"
fi

echo ""
echo -e "${BLUE}Step 2: Starting LiteLLM...${NC}"
echo ""

# Start LiteLLM
./start-litellm-local.sh

# Check if LiteLLM started successfully
if ! curl -s http://localhost:4000/health/readiness >/dev/null 2>&1; then
    echo -e "${RED}✗ LiteLLM failed to start${NC}"
    exit 1
fi

echo ""
echo -e "${BLUE}Step 3: Testing with probe script...${NC}"
echo ""

# Test localhost model
echo -e "${BLUE}Testing localhost model...${NC}"
if KEY=sk-local-test-key-123 MODEL=localhost-llama3 ./probe.sh 2>/dev/null | grep -q "Response received"; then
    echo -e "${GREEN}✓${NC} Localhost model test passed"
else
    echo -e "${YELLOW}⚠${NC} Localhost model test failed"
fi

# Test big72 model if available
if curl -s --connect-timeout 2 http://big72.local:11434/api/tags >/dev/null 2>&1; then
    echo ""
    echo -e "${BLUE}Testing big72.local model...${NC}"
    if KEY=sk-local-test-key-123 MODEL=big72-default ./probe.sh 2>/dev/null | grep -q "Response received"; then
        echo -e "${GREEN}✓${NC} big72.local model test passed"
    else
        echo -e "${YELLOW}⚠${NC} big72.local model test failed"
    fi
fi

echo ""
echo -e "${BLUE}Step 4: Testing with Emacs batch mode...${NC}"
echo ""

# Test Emacs with localhost
echo -e "${BLUE}Testing Emacs with localhost...${NC}"
if ./test-emacs-localhost.sh >/dev/null 2>&1; then
    echo -e "${GREEN}✓${NC} Emacs localhost test passed"
else
    echo -e "${YELLOW}⚠${NC} Emacs localhost test failed"
fi

# Test Emacs with big72 if available
if curl -s --connect-timeout 2 http://big72.local:11434/api/tags >/dev/null 2>&1; then
    echo ""
    echo -e "${BLUE}Testing Emacs with big72.local...${NC}"
    if ./test-emacs-big72.sh >/dev/null 2>&1; then
        echo -e "${GREEN}✓${NC} Emacs big72.local test passed"
    else
        echo -e "${YELLOW}⚠${NC} Emacs big72.local test failed"
    fi
fi

echo ""
echo -e "${BLUE}Step 5: Testing routing aliases...${NC}"
echo ""

# Test coding-local alias (should route to fastest available)
echo -e "${BLUE}Testing coding-local routing alias...${NC}"
if KEY=sk-local-test-key-123 MODEL=coding-local ./probe.sh 2>/dev/null | grep -q "Response received"; then
    echo -e "${GREEN}✓${NC} Routing alias test passed"
else
    echo -e "${YELLOW}⚠${NC} Routing alias test failed"
fi

echo ""
echo -e "${BLUE}═══════════════════════════════════════════════════════${NC}"
echo -e "${GREEN}     Setup Verification Complete!${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════${NC}"
echo ""
echo "LiteLLM is running at: http://localhost:4000"
echo "Master key: sk-local-test-key-123"
echo ""
echo "Available endpoints:"
echo "  • API:     http://localhost:4000/v1"
echo "  • Health:  http://localhost:4000/health"
echo "  • UI:      http://localhost:4000/ui"
echo "  • Models:  http://localhost:4000/v1/models"
echo ""
echo "Test individual models:"
echo "  MODEL=localhost-llama3 ./probe.sh"
echo "  MODEL=localhost-qwen-coder ./probe.sh"
echo "  MODEL=big72-default ./probe.sh"
echo ""
echo "Test from Emacs:"
echo "  ./test-emacs-localhost.sh"
echo "  ./test-emacs-big72.sh"
echo ""
echo -e "${GREEN}Next steps:${NC}"
echo "1. Add cloud API keys to llm-gateway/.env"
echo "2. Update llm-gateway/litellm/config.yaml with cloud models"
echo "3. Restart LiteLLM with full config"
echo ""