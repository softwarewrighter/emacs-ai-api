#!/usr/bin/env bash
# Test Google Gemini models via LiteLLM gateway

set -euo pipefail

# Help function
show_help() {
    cat << EOF
PURPOSE:
    Test Google Gemini AI models through the LiteLLM gateway.
    Verifies connectivity to Gemini API including Pro and Flash variants.

USAGE:
    $0 [OPTIONS]
    $0 -h | --help

OPTIONS:
    -h, --help    Show this help message

MODELS TESTED:
    - gemini-1.5-pro: Gemini's most capable model
    - gemini-1.5-flash: Fast, efficient model for high-volume tasks
    - gemini-2.0-flash-exp: Experimental next-gen Flash model

ENVIRONMENT:
    BASE                  API base URL (default: http://localhost:4000/v1)
    KEY or LITELLM_MASTER_KEY    API key for authentication
    LITELLM_VIRTUAL_KEY   Virtual key (alternative to master key)
    GEMINI_API_KEY        Google API key (must be configured in LiteLLM)

REQUIREMENTS:
    - LiteLLM running at localhost:4000
    - Google API credentials configured
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
    - Verify Gemini config in litellm_config.yaml
    - Check API key: echo \$GEMINI_API_KEY
    - View logs: docker compose logs litellm
    - Test models list: curl -H "Authorization: Bearer \$KEY" \$BASE/models

NOTES:
    - Gemini models require a valid Google API key
    - Rate limits apply based on your Google Cloud project
    - Costs are tracked in LiteLLM's spend logs

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

echo -e "${BLUE}Testing Google Gemini Models via LiteLLM${NC}"
echo "========================================"
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
        echo "  To enable, ensure GEMINI_API_KEY or Google Cloud credentials are configured"
        echo ""
        return 1
    fi
}

# Track if any tests succeeded
TESTS_RUN=0

# Test Gemini 1.5 Pro
if test_model "gemini-1.5-pro" \
    "Write a TypeScript generic function for array filtering" \
    "Gemini 1.5 Pro"; then
    ((TESTS_RUN++))
fi

# Test Gemini 1.5 Flash
if test_model "gemini-1.5-flash" \
    "Explain REST API principles in 3 bullet points" \
    "Gemini 1.5 Flash"; then
    ((TESTS_RUN++))
fi

# Test Gemini 1.5 Flash 8B (alternative model)
if test_model "gemini-1.5-flash-8b" \
    "Create a simple TODO list in HTML" \
    "Gemini 1.5 Flash 8B"; then
    ((TESTS_RUN++))
fi

# Test Gemini 2.0 Flash Experimental (may not be available)
if test_model "gemini-2.0-flash-exp" \
    "Write a React hook for debounced input" \
    "Gemini 2.0 Flash Experimental"; then
    ((TESTS_RUN++))
fi

# Fallback test with a local model if no Gemini models available
if [ $TESTS_RUN -eq 0 ]; then
    echo -e "${YELLOW}No Gemini models available. Testing with fallback local model...${NC}"
    if model_available "llama3.2:latest"; then
        BASE="$BASE" KEY="$KEY" MODEL="llama3.2:latest" \
            PROMPT="Explain cloud computing in one sentence" \
            "$SCRIPT_DIR/probe.sh"
    else
        echo -e "${RED}✗ No models available for testing${NC}"
        echo "  Please configure GEMINI_API_KEY or install local models"
        exit 1
    fi
fi

if [ $TESTS_RUN -gt 0 ]; then
    echo -e "${GREEN}✓ Gemini tests complete ($TESTS_RUN models tested)${NC}"
else
    echo -e "${YELLOW}⚠ Tests completed with fallback model${NC}"
fi