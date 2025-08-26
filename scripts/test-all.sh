#!/usr/bin/env bash
# Test all configured providers and models
# Usage: ./test-all.sh [quick|full]

set -euo pipefail

# Configuration
MODE="${1:-quick}"
BASE="${BASE:-http://localhost:4000/v1}"
KEY="${KEY:-${LITELLM_MASTER_KEY:-${LITELLM_VIRTUAL_KEY}}}"

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Counters
TOTAL=0
SUCCESS=0
FAILED=0
SKIPPED=0

# Arrays to track results
declare -a FAILED_MODELS=()
declare -a SUCCESS_MODELS=()

# Function to print section header
print_header() {
    echo ""
    echo -e "${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${CYAN}$1${NC}"
    echo -e "${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
}

# Function to test a model
test_model() {
    local model=$1
    local prompt="${2:-Write a one-line Python hello world.}"
    local provider_type="${3:-unknown}"
    
    ((TOTAL++))
    
    printf "  %-30s " "$model:"
    
    # Skip local models if in quick mode and they're not running
    if [ "$MODE" = "quick" ] && [[ "$model" == local-* || "$model" == ollama-* ]]; then
        # Quick check if local services are running
        if [[ "$model" == ollama-* ]] && ! curl -s http://localhost:11434/api/tags >/dev/null 2>&1; then
            echo -e "${YELLOW}[SKIPPED]${NC} - Ollama not running"
            ((SKIPPED++))
            return
        fi
        if [[ "$model" == local-* ]] && ! curl -s http://localhost:8080/health >/dev/null 2>&1; then
            echo -e "${YELLOW}[SKIPPED]${NC} - llama.cpp not running"
            ((SKIPPED++))
            return
        fi
    fi
    
    # Make the API call
    RESPONSE=$(curl -sS "$BASE/chat/completions" \
        -H "Authorization: Bearer $KEY" \
        -H "Content-Type: application/json" \
        -d "{\"model\":\"$model\",\"messages\":[{\"role\":\"user\",\"content\":\"$prompt\"}],\"max_tokens\":50}" \
        --max-time 10 2>&1) || RESPONSE=""
    
    # Check response
    if [ -z "$RESPONSE" ]; then
        echo -e "${RED}[TIMEOUT]${NC}"
        ((FAILED++))
        FAILED_MODELS+=("$model (timeout)")
    elif echo "$RESPONSE" | jq -e '.choices[0].message.content' >/dev/null 2>&1; then
        echo -e "${GREEN}[OK]${NC}"
        ((SUCCESS++))
        SUCCESS_MODELS+=("$model")
    elif echo "$RESPONSE" | jq -e '.error' >/dev/null 2>&1; then
        ERROR=$(echo "$RESPONSE" | jq -r '.error.message // .error' | head -c 50)
        echo -e "${RED}[FAILED]${NC} - $ERROR"
        ((FAILED++))
        FAILED_MODELS+=("$model ($ERROR)")
    else
        echo -e "${RED}[INVALID]${NC}"
        ((FAILED++))
        FAILED_MODELS+=("$model (invalid response)")
    fi
}

# Main testing
echo -e "${BLUE}LiteLLM Comprehensive Test Suite${NC}"
echo -e "${BLUE}Mode: $MODE${NC}"
echo "================================"

# Check LiteLLM is running
if ! curl -sf "$BASE/../health/readiness" >/dev/null 2>&1; then
    echo -e "${RED}Error: LiteLLM doesn't appear to be running at $BASE${NC}"
    echo "Start it with: cd llm-gateway && docker compose up -d"
    exit 1
fi

# Test routing aliases
print_header "Testing Routing Aliases"
test_model "coding-best" "Write a recursive factorial function" "alias"
test_model "coding-balanced" "Explain closures in JavaScript" "alias"
test_model "coding-cheap" "Write a bash loop to count files" "alias"
test_model "coding-auto" "Implement a stack in Python" "alias"
test_model "reasoning" "Find the bug: if (x = 5) { return true }" "alias"

# Test OpenAI models
if [ "$MODE" = "full" ] || [ -n "${OPENAI_API_KEY:-}" ]; then
    print_header "Testing OpenAI Models"
    test_model "gpt-4o" "Write a Python decorator" "openai"
    test_model "gpt-4o-mini" "Explain async/await" "openai"
    test_model "gpt-4-turbo" "Write a SQL JOIN query" "openai"
    if [ "$MODE" = "full" ]; then
        test_model "o1-preview" "Optimize this: O(n²) nested loops" "openai"
        test_model "o1-mini" "What's the complexity of quicksort?" "openai"
    fi
fi

# Test Anthropic models
if [ "$MODE" = "full" ] || [ -n "${ANTHROPIC_API_KEY:-}" ]; then
    print_header "Testing Anthropic Models"
    test_model "claude-3-5-sonnet" "Write a context manager in Python" "anthropic"
    test_model "claude-3-5-haiku" "Explain REST APIs briefly" "anthropic"
    if [ "$MODE" = "full" ]; then
        test_model "claude-3-opus" "Design a rate limiter" "anthropic"
    fi
fi

# Test DeepSeek models
if [ "$MODE" = "full" ] || [ -n "${DEEPSEEK_API_KEY:-}" ]; then
    print_header "Testing DeepSeek Models"
    test_model "deepseek-chat" "Write a regex for email validation" "deepseek"
    test_model "deepseek-coder" "Implement binary search" "deepseek"
fi

# Test Google Gemini models
if [ "$MODE" = "full" ] || [ -n "${GOOGLE_API_KEY:-}" ]; then
    print_header "Testing Google Gemini Models"
    test_model "gemini-1.5-pro" "Write a TypeScript interface" "gemini"
    test_model "gemini-1.5-flash" "List Python data types" "gemini"
    if [ "$MODE" = "full" ]; then
        test_model "gemini-2.0-flash-exp" "Explain React hooks" "gemini"
    fi
fi

# Test local models
print_header "Testing Local Models"
test_model "local-deepseek" "Write a hello world in C" "local"
test_model "local-kimi" "Explain pointers in C++" "local"
test_model "ollama-deepseek" "Write a Python list comprehension" "local"
test_model "ollama-qwen" "Explain Docker containers" "local"
test_model "ollama-llama" "Write a bash script header" "local"

# Summary
print_header "Test Summary"
echo ""
echo -e "  Total Tests:    $TOTAL"
echo -e "  ${GREEN}Successful:     $SUCCESS${NC}"
echo -e "  ${RED}Failed:         $FAILED${NC}"
echo -e "  ${YELLOW}Skipped:        $SKIPPED${NC}"
echo ""

if [ ${#SUCCESS_MODELS[@]} -gt 0 ]; then
    echo -e "${GREEN}Working Models:${NC}"
    for model in "${SUCCESS_MODELS[@]}"; do
        echo "  ✓ $model"
    done
    echo ""
fi

if [ ${#FAILED_MODELS[@]} -gt 0 ]; then
    echo -e "${RED}Failed Models:${NC}"
    for model in "${FAILED_MODELS[@]}"; do
        echo "  ✗ $model"
    done
    echo ""
fi

# Exit code based on results
if [ $FAILED -eq 0 ] && [ $SUCCESS -gt 0 ]; then
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
elif [ $SUCCESS -eq 0 ]; then
    echo -e "${RED}All tests failed!${NC}"
    exit 1
else
    echo -e "${YELLOW}Some tests failed. Check configuration.${NC}"
    exit 1
fi