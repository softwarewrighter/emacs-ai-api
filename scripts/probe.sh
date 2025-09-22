#!/usr/bin/env bash
# Generic probe script for testing any OpenAI-compatible API endpoint

set -euo pipefail

# Help function
show_help() {
    cat << EOF
PURPOSE:
    Test any OpenAI-compatible API endpoint with a simple prompt.
    Useful for verifying LiteLLM gateway connectivity and model availability.

USAGE:
    $0 [MODEL_NAME]
    $0 -h | --help

ARGUMENTS:
    MODEL_NAME    Optional model name (default: llama3.2:latest)

ENVIRONMENT VARIABLES:
    BASE          API base URL (default: http://localhost:4000/v1)
    KEY           API key (default: sk-local-test-key-123)
    MODEL         Model name (overrides positional argument)
    PROMPT        Custom prompt to send
    MAX_TOKENS    Maximum tokens in response (default: 256)
    TEMPERATURE   Temperature for generation (default: 0.7)
    STREAM        Enable streaming (default: false)
    VERBOSE       Show detailed output (default: false)

EXAMPLES:
    # Test default model
    $0

    # Test specific model
    $0 gpt-4o-mini

    # Test with custom endpoint
    BASE=http://api.openai.com/v1 KEY=sk-... $0 gpt-4

    # Verbose mode with custom prompt
    VERBOSE=true PROMPT="Hello" $0 qwen2.5:7b

EOF
    exit 0
}

# Check for help flag
if [[ "${1:-}" == "-h" ]] || [[ "${1:-}" == "--help" ]]; then
    show_help
fi

# Accept model as first argument, or use environment variable, or default
if [[ -n "${1:-}" ]]; then
    MODEL_ARG="$1"
else
    MODEL_ARG=""
fi

# Configuration with defaults
BASE="${BASE:-http://localhost:4000/v1}"
KEY="${KEY:-${LITELLM_MASTER_KEY:-${LITELLM_VIRTUAL_KEY:-sk-local-test-key-123}}}"
MODEL="${MODEL:-${MODEL_ARG:-llama3.2:latest}}"
PROMPT="${PROMPT:-Write a one-line Python function to calculate factorial.}"
MAX_TOKENS="${MAX_TOKENS:-256}"
TEMPERATURE="${TEMPERATURE:-0.7}"
STREAM="${STREAM:-false}"
VERBOSE="${VERBOSE:-false}"

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored output
print_info() { echo -e "${BLUE}ℹ ${NC}$1"; }
print_success() { echo -e "${GREEN}✓${NC} $1"; }
print_error() { echo -e "${RED}✗${NC} $1"; }
print_warning() { echo -e "${YELLOW}⚠${NC} $1"; }

# Display configuration
if [ "$VERBOSE" = "true" ]; then
    print_info "Configuration:"
    echo "  BASE: $BASE"
    echo "  MODEL: $MODEL"
    echo "  KEY: ${KEY:0:10}..."
    echo "  MAX_TOKENS: $MAX_TOKENS"
    echo "  TEMPERATURE: $TEMPERATURE"
    echo ""
fi

print_info "Testing $MODEL at $BASE"

# Create the request JSON
REQUEST_JSON=$(jq -nc \
    --arg model "$MODEL" \
    --arg prompt "$PROMPT" \
    --arg max_tokens "$MAX_TOKENS" \
    --arg temperature "$TEMPERATURE" \
    --argjson stream "$STREAM" \
    '{
        model: $model,
        messages: [{role: "user", content: $prompt}],
        max_tokens: ($max_tokens | tonumber),
        temperature: ($temperature | tonumber),
        stream: $stream
    }')

if [ "$VERBOSE" = "true" ]; then
    print_info "Request JSON:"
    echo "$REQUEST_JSON" | jq .
    echo ""
fi

# Make the API call
RESPONSE=$(curl -sS "$BASE/chat/completions" \
    -H "Authorization: Bearer $KEY" \
    -H "Content-Type: application/json" \
    -d "$REQUEST_JSON" 2>&1) || {
    print_error "Failed to connect to $BASE"
    exit 1
}

# Check if response is valid JSON
if ! echo "$RESPONSE" | jq empty 2>/dev/null; then
    print_error "Invalid JSON response:"
    echo "$RESPONSE"
    exit 1
fi

# Check for error in response
if echo "$RESPONSE" | jq -e '.error' >/dev/null 2>&1; then
    ERROR_MSG=$(echo "$RESPONSE" | jq -r '.error.message // .error')
    print_error "API Error: $ERROR_MSG"
    
    if [ "$VERBOSE" = "true" ]; then
        echo "Full response:"
        echo "$RESPONSE" | jq .
    fi
    exit 1
fi

# Extract the response content
CONTENT=$(echo "$RESPONSE" | jq -r '.choices[0].message.content // empty')
if [ -z "$CONTENT" ]; then
    print_error "No content in response"
    if [ "$VERBOSE" = "true" ]; then
        echo "Full response:"
        echo "$RESPONSE" | jq .
    fi
    exit 1
fi

# Extract metadata
MODEL_USED=$(echo "$RESPONSE" | jq -r '.model // "unknown"')
USAGE=$(echo "$RESPONSE" | jq -r '.usage // {}')
PROMPT_TOKENS=$(echo "$USAGE" | jq -r '.prompt_tokens // 0')
COMPLETION_TOKENS=$(echo "$USAGE" | jq -r '.completion_tokens // 0')
TOTAL_TOKENS=$(echo "$USAGE" | jq -r '.total_tokens // 0')

# Display results
print_success "Response received from $MODEL_USED"
echo ""
echo "Response:"
echo "----------------------------------------"
echo "$CONTENT"
echo "----------------------------------------"

if [ "$VERBOSE" = "true" ]; then
    echo ""
    print_info "Token Usage:"
    echo "  Prompt: $PROMPT_TOKENS"
    echo "  Completion: $COMPLETION_TOKENS"
    echo "  Total: $TOTAL_TOKENS"
fi