#!/usr/bin/env bash
# Test local llama.cpp server models directly and via LiteLLM

set -euo pipefail

# Help function
show_help() {
    cat << EOF
PURPOSE:
    Test llama.cpp server models both directly and through LiteLLM gateway.
    Verifies connectivity to local or remote llama.cpp servers and model routing.

USAGE:
    $0 [OPTIONS] [HOST:PORT]
    $0 -h | --help
    $0 --big72    # Test big72 server

ARGUMENTS:
    HOST:PORT     Optional llama.cpp server location (e.g., big72:8090)

OPTIONS:
    -h, --help    Show this help message
    --big72       Test big72.local server (shortcut for big72:8090)
    --local       Test localhost server (default)

TESTS PERFORMED:
    1. Direct llama.cpp server access at localhost:8080
    2. llama.cpp DeepSeek model via LiteLLM
    3. llama.cpp Kimi-K2 model via LiteLLM
    4. Server information retrieval

ENVIRONMENT:
    LLAMA_CPP_HOST        llama.cpp server host (default: localhost)
    LLAMA_CPP_PORT        llama.cpp server port (default: 8080)
    LITELLM_MASTER_KEY    Admin key for LiteLLM
    LITELLM_VIRTUAL_KEY   Virtual key (alternative to master key)

REQUIREMENTS:
    - llama.cpp server running at localhost:8080
    - LiteLLM running at localhost:4000
    - Docker containers up (cd llm-gateway && docker compose up -d)
    - Model loaded in llama.cpp server

STARTING LLAMA.CPP SERVER:
    # Basic server start
    ./server -m model.gguf -c 16384 --port 8080

    # With GPU acceleration
    ./server -m model.gguf -c 16384 --port 8080 -ngl 35

    # With specific model (DeepSeek example)
    ./server -m deepseek-coder-v2-lite-instruct-Q4_K_M.gguf -c 16384 --port 8080

EXAMPLES:
    # Test default localhost:8080
    $0

    # Test big72 server on port 8090
    $0 --big72
    # or
    $0 big72:8090
    
    # Test custom server
    $0 192.168.1.100:8081

    # Using environment variables
    LLAMA_CPP_HOST=big72 LLAMA_CPP_PORT=8090 $0

    # Test with custom LiteLLM key
    LITELLM_MASTER_KEY=sk-my-key $0

TROUBLESHOOTING:
    - Check llama.cpp server: curl http://localhost:8080/health
    - View llama.cpp logs: Check terminal where server was started
    - Check LiteLLM status: ./scripts/health.sh
    - Verify models: curl -H "Authorization: Bearer \$KEY" http://localhost:4000/v1/models

NOTES:
    - llama.cpp server must be started manually before running tests
    - Models in LiteLLM config must point to correct llama.cpp endpoint
    - Performance depends on hardware (CPU/GPU) and model quantization

EOF
    exit 0
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            show_help
            ;;
        --big72)
            LLAMA_CPP_HOST="big72"
            LLAMA_CPP_PORT="8090"
            shift
            ;;
        --local)
            LLAMA_CPP_HOST="localhost"
            LLAMA_CPP_PORT="8080"
            shift
            ;;
        *:*)
            # Parse HOST:PORT format
            LLAMA_CPP_HOST="${1%%:*}"
            LLAMA_CPP_PORT="${1##*:}"
            shift
            ;;
        *)
            echo "Unknown option: $1"
            echo "Use -h or --help for usage information"
            exit 1
            ;;
    esac
done

# Get script directory for relative paths
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# Configuration (use environment variables or defaults)
LLAMA_CPP_HOST="${LLAMA_CPP_HOST:-localhost}"
LLAMA_CPP_PORT="${LLAMA_CPP_PORT:-8080}"
LLAMA_CPP_BASE="http://$LLAMA_CPP_HOST:$LLAMA_CPP_PORT/v1"

# Color output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

echo -e "${BLUE}Testing llama.cpp Server Models${NC}"
echo "==============================="
echo "Target: $LLAMA_CPP_HOST:$LLAMA_CPP_PORT"
echo ""

# Check if llama.cpp server is running (check for props endpoint which is llama.cpp specific)
LLAMA_CPP_AVAILABLE=false
if curl -s "http://$LLAMA_CPP_HOST:$LLAMA_CPP_PORT/props" 2>/dev/null | jq -e '.default_generation_settings' >/dev/null 2>&1; then
    LLAMA_CPP_AVAILABLE=true
    echo -e "${GREEN}✓ llama.cpp server detected at $LLAMA_CPP_HOST:$LLAMA_CPP_PORT${NC}"
elif curl -sf "http://$LLAMA_CPP_HOST:$LLAMA_CPP_PORT/health" >/dev/null 2>&1; then
    echo -e "${YELLOW}⚠ Server running on $LLAMA_CPP_HOST:$LLAMA_CPP_PORT but not llama.cpp${NC}"
    echo "  (Another service is using this port)"
    echo "  To test llama.cpp, start with:"
    echo "  ./server -m model.gguf -c 16384 --port 8081"
else
    echo -e "${YELLOW}⚠ llama.cpp server not running on $LLAMA_CPP_HOST:$LLAMA_CPP_PORT${NC}"
    echo "  To test direct access, start with:"
    echo "  ./server -m model.gguf -c 16384 --port $LLAMA_CPP_PORT"
fi
echo ""

# Test direct llama.cpp access only if server is running
if [ "$LLAMA_CPP_AVAILABLE" = true ]; then
    echo -e "${BLUE}Testing direct llama.cpp server access...${NC}"
    
    # Use different prompts for different servers
    if [[ "$LLAMA_CPP_HOST" == "big72" ]]; then
        # DeepSeek model on big72
        TEST_PROMPT="Which cargo run flag runs code that is not under ./src dir in a Rust project?"
    else
        # Generic prompt for other servers
        TEST_PROMPT="Write a shell script to monitor disk usage"
    fi
    
    # Note: llama.cpp servers typically don't require auth, using dummy key
    BASE="$LLAMA_CPP_BASE" KEY="sk-llama" MODEL="deepseek" \
        PROMPT="$TEST_PROMPT" \
        "$SCRIPT_DIR/probe.sh"
    echo ""
else
    echo -e "${YELLOW}Skipping direct llama.cpp test (server not running)${NC}"
    echo ""
fi

# Check LiteLLM availability
echo -e "${BLUE}Checking LiteLLM models...${NC}"
LITELLM_KEY="${LITELLM_MASTER_KEY:-${LITELLM_VIRTUAL_KEY:-sk-local-test-key-123}}"
AVAILABLE_MODELS=$(curl -s -H "Authorization: Bearer $LITELLM_KEY" "http://localhost:4000/v1/models" 2>/dev/null | jq -r '.data[].id' 2>/dev/null || echo "")

# Function to check if model is available
model_available() {
    echo "$AVAILABLE_MODELS" | grep -q "^$1$"
}

# Track successful tests
TESTS_RUN=0

# Test llama.cpp through LiteLLM (DeepSeek model) if available
if model_available "local-deepseek"; then
    echo -e "${BLUE}Testing llama.cpp DeepSeek via LiteLLM...${NC}"
    BASE="http://localhost:4000/v1" KEY="$LITELLM_KEY" \
        MODEL="local-deepseek" \
        PROMPT="Implement a LRU cache in Python" \
        "$SCRIPT_DIR/probe.sh"
    echo ""
    ((TESTS_RUN++))
else
    echo -e "${YELLOW}⚠ Skipping local-deepseek (model not configured in LiteLLM)${NC}"
    echo ""
fi

# Test llama.cpp through LiteLLM (Kimi model) if available
if model_available "local-kimi"; then
    echo -e "${BLUE}Testing llama.cpp Kimi-K2 via LiteLLM...${NC}"
    BASE="http://localhost:4000/v1" KEY="$LITELLM_KEY" \
        MODEL="local-kimi" \
        PROMPT="Explain memory management in Rust" \
        "$SCRIPT_DIR/probe.sh"
    echo ""
    ((TESTS_RUN++))
else
    echo -e "${YELLOW}⚠ Skipping local-kimi (model not configured in LiteLLM)${NC}"
    echo ""
fi

# If no llama.cpp models configured, test with a local Ollama model
if [ $TESTS_RUN -eq 0 ] && [ "$LLAMA_CPP_AVAILABLE" = false ]; then
    echo -e "${YELLOW}No llama.cpp models available. Testing with local Ollama model...${NC}"
    if model_available "llama3.2:latest"; then
        BASE="http://localhost:4000/v1" KEY="$LITELLM_KEY" \
            MODEL="llama3.2:latest" \
            PROMPT="Write a function to calculate Fibonacci numbers" \
            "$SCRIPT_DIR/probe.sh"
        echo ""
    else
        echo -e "${RED}✗ No models available for testing${NC}"
        echo "  Please start llama.cpp server or configure models in LiteLLM"
    fi
fi

# Get server info only if available
if [ "$LLAMA_CPP_AVAILABLE" = true ]; then
    echo -e "${BLUE}llama.cpp server info:${NC}"
    curl -s "http://$LLAMA_CPP_HOST:$LLAMA_CPP_PORT/props" 2>/dev/null | jq '.' || echo "Unable to get server info"
    echo ""
fi

# Summary
if [ "$LLAMA_CPP_AVAILABLE" = true ] || [ $TESTS_RUN -gt 0 ]; then
    echo -e "${GREEN}✓ Tests complete${NC}"
else
    echo -e "${YELLOW}⚠ Tests completed with fallback models${NC}"
    echo "  To test llama.cpp models:"
    echo "  1. Start llama.cpp server: ./server -m model.gguf --port 8080"
    echo "  2. Configure models in LiteLLM config"
fi