#!/usr/bin/env bash
# Generic probe script for testing any OpenAI-compatible API endpoint
# Usage: BASE=http://host:port/v1 MODEL=model-name ./probe.sh

set -euo pipefail

# Configuration with defaults
BASE="${BASE:-http://localhost:4000/v1}"
KEY="${KEY:-${LITELLM_MASTER_KEY:-${LITELLM_VIRTUAL_KEY:-sk-local}}}"
MODEL="${MODEL:-coding-auto}"
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