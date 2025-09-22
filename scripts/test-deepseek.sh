#!/usr/bin/env bash
# Test DeepSeek models via LiteLLM gateway

set -euo pipefail

# Help function
show_help() {
    cat << EOF
PURPOSE:
    Test DeepSeek AI models through the LiteLLM gateway.
    Verifies connectivity to both cloud API and local instances.

USAGE:
    $0 [OPTIONS]
    $0 -h | --help

OPTIONS:
    -h, --help    Show this help message

MODELS TESTED:
    - deepseek-chat: DeepSeek's general chat model (cloud)
    - deepseek-coder: Specialized coding model (cloud)
    - local-deepseek: Local instance via llama.cpp
    - ollama-deepseek: Local instance via Ollama

ENVIRONMENT:
    BASE                  API base URL (default: http://localhost:4000/v1)
    KEY or LITELLM_MASTER_KEY    API key for authentication
    LITELLM_VIRTUAL_KEY   Virtual key (alternative to master key)

REQUIREMENTS:
    - LiteLLM running at localhost:4000
    - DeepSeek API credentials configured (for cloud models)
    - Local DeepSeek models installed (for local testing)
    - Docker containers up (cd llm-gateway && docker compose up -d)

EXAMPLES:
    # Test with default settings
    $0

    # Test with custom API key
    KEY=sk-my-deepseek-key $0

    # Test against different endpoint
    BASE=http://192.168.1.100:4000/v1 $0

TROUBLESHOOTING:
    - Check LiteLLM status: ./scripts/health.sh
    - Verify DeepSeek config in litellm_config.yaml
    - Check API key: echo \$DEEPSEEK_API_KEY
    - View logs: docker compose logs litellm

EOF
    exit 0
}

# Check for help flag
if [[ "${1:-}" == "-h" ]] || [[ "${1:-}" == "--help" ]]; then
    show_help
fi

# Get script directory for relative paths
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# Source environment or use defaults
BASE="${BASE:-http://localhost:4000/v1}"
KEY="${KEY:-${LITELLM_MASTER_KEY:-${LITELLM_VIRTUAL_KEY:-sk-local-test-key-123}}}"

# Color output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

echo -e "${BLUE}Testing DeepSeek Models via LiteLLM${NC}"
echo "==================================="
echo ""

# Check which models are available
AVAILABLE_MODELS=$(curl -s -H "Authorization: Bearer $KEY" "$BASE/models" 2>/dev/null | jq -r '.data[].id' 2>/dev/null || echo "")

# Function to check if model is available
model_available() {
    echo "$AVAILABLE_MODELS" | grep -q "^$1$"
}

# Function to test a model if available
test_model() {
    local model="$1"
    local prompt="$2"
    local description="$3"
    
    if model_available "$model"; then
        echo -e "${BLUE}Testing $description...${NC}"
        BASE="$BASE" KEY="$KEY" MODEL="$model" PROMPT="$prompt" \
            "$SCRIPT_DIR/probe.sh"
        echo ""
        return 0
    else
        echo -e "${YELLOW}⚠ Skipping $description (model not available: $model)${NC}"
        echo "  To enable, ensure DEEPSEEK_API_KEY is set in ~/.env"
        echo ""
        return 1
    fi
}

# Track if any tests succeeded
TESTS_RUN=0

# Test DeepSeek Chat
if test_model "deepseek-chat" \
    "Write a SQL query to find duplicate rows in a table" \
    "DeepSeek Chat"; then
    ((TESTS_RUN++))
fi

# Test DeepSeek Coder
if test_model "deepseek-coder" \
    "Implement binary search in C++" \
    "DeepSeek Coder"; then
    ((TESTS_RUN++))
fi

# Test local DeepSeek via llama.cpp
if test_model "local-deepseek" \
    "Write a bash function to check if a port is open" \
    "local DeepSeek (llama.cpp)"; then
    ((TESTS_RUN++))
fi

# Test local DeepSeek via Ollama
if test_model "ollama-deepseek" \
    "Create a Python decorator for retry logic" \
    "local DeepSeek (Ollama)"; then
    ((TESTS_RUN++))
fi

# Fallback test with a local model if no DeepSeek models available
if [ $TESTS_RUN -eq 0 ]; then
    echo -e "${YELLOW}No DeepSeek models available. Testing with fallback local model...${NC}"
    if model_available "llama3.2:latest"; then
        BASE="$BASE" KEY="$KEY" MODEL="llama3.2:latest" \
            PROMPT="Write a Python function to calculate factorial" \
            "$SCRIPT_DIR/probe.sh"
    else
        echo -e "${RED}✗ No models available for testing${NC}"
        echo "  Please configure DEEPSEEK_API_KEY or install local models"
        exit 1
    fi
fi

if [ $TESTS_RUN -gt 0 ]; then
    echo -e "${GREEN}✓ DeepSeek tests complete ($TESTS_RUN models tested)${NC}"
else
    echo -e "${YELLOW}⚠ Tests completed with fallback model${NC}"
fi