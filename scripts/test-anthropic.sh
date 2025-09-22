#!/usr/bin/env bash
# Test Anthropic Claude models via LiteLLM gateway

set -euo pipefail

# Help function
show_help() {
    cat << EOF
PURPOSE:
    Test Anthropic Claude AI models through the LiteLLM gateway.
    Verifies connectivity to Anthropic API for all Claude model variants.

USAGE:
    $0 [OPTIONS]
    $0 -h | --help

OPTIONS:
    -h, --help    Show this help message

MODELS TESTED:
    - claude-3-5-sonnet: Latest Sonnet model (most capable)
    - claude-3-5-haiku: Fast, efficient model
    - claude-3-opus: Previous generation high-capability model

ENVIRONMENT:
    BASE                  API base URL (default: http://localhost:4000/v1)
    KEY or LITELLM_MASTER_KEY    API key for authentication
    LITELLM_VIRTUAL_KEY   Virtual key (alternative to master key)
    ANTHROPIC_API_KEY     Anthropic API key (must be configured in LiteLLM)

REQUIREMENTS:
    - LiteLLM running at localhost:4000
    - Anthropic API credentials configured
    - Docker containers up (cd llm-gateway && docker compose up -d)

EXAMPLES:
    # Test with default settings
    $0

    # Test with custom API key
    KEY=sk-my-virtual-key $0

    # Test against different endpoint
    BASE=http://192.168.1.100:4000/v1 $0

TROUBLESHOOTING:
    - Check LiteLLM status: ./scripts/health.sh
    - Verify Anthropic config in litellm_config.yaml
    - Check API key: echo \$ANTHROPIC_API_KEY
    - View logs: docker compose logs litellm
    - Test models list: curl -H "Authorization: Bearer \$KEY" \$BASE/models | grep claude

NOTES:
    - Claude models require a valid Anthropic API key
    - Rate limits apply based on your Anthropic plan
    - Costs are tracked in LiteLLM's spend logs
    - Claude 3.5 Sonnet is recommended for complex tasks
    - Claude 3.5 Haiku is faster and more cost-effective for simple tasks

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

echo -e "${BLUE}Testing Anthropic Claude Models via LiteLLM${NC}"
echo "==========================================="
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
        echo "  To enable, ensure ANTHROPIC_API_KEY is set in ~/.env"
        echo ""
        return 1
    fi
}

# Track if any tests succeeded
TESTS_RUN=0

# Test Claude 3.5 Sonnet
if test_model "claude-3-5-sonnet" \
    "Write a Python context manager for database connections" \
    "Claude 3.5 Sonnet"; then
    ((TESTS_RUN++))
fi

# Test Claude 3.5 Haiku
if test_model "claude-3-5-haiku" \
    "Explain async/await in JavaScript briefly" \
    "Claude 3.5 Haiku"; then
    ((TESTS_RUN++))
fi

# Test Claude 3 Opus
if test_model "claude-3-opus" \
    "Design a simple rate limiter in Go" \
    "Claude 3 Opus"; then
    ((TESTS_RUN++))
fi

# Fallback test with a local model if no Claude models available
if [ $TESTS_RUN -eq 0 ]; then
    echo -e "${YELLOW}No Claude models available. Testing with fallback local model...${NC}"
    if model_available "llama3.2:latest"; then
        BASE="$BASE" KEY="$KEY" MODEL="llama3.2:latest" \
            PROMPT="Write a simple HTTP server in Python" \
            "$SCRIPT_DIR/probe.sh"
    else
        echo -e "${RED}✗ No models available for testing${NC}"
        echo "  Please configure ANTHROPIC_API_KEY or install local models"
        exit 1
    fi
fi

if [ $TESTS_RUN -gt 0 ]; then
    echo -e "${GREEN}✓ Anthropic tests complete ($TESTS_RUN models tested)${NC}"
else
    echo -e "${YELLOW}⚠ Tests completed with fallback model${NC}"
fi