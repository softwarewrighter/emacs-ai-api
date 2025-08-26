#!/usr/bin/env bash
# Test local llama.cpp server models directly and via LiteLLM

set -euo pipefail

# Color output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${BLUE}Testing llama.cpp Server Models${NC}"
echo "==============================="
echo ""

# Check if llama.cpp server is running
if ! curl -s http://localhost:8080/health >/dev/null 2>&1; then
    echo -e "${YELLOW}Warning: llama.cpp server doesn't appear to be running on localhost:8080${NC}"
    echo "Start with: ./server -m model.gguf -c 16384 --port 8080"
    echo ""
fi

# Test direct llama.cpp access
echo -e "${BLUE}Testing direct llama.cpp server access...${NC}"
BASE="http://localhost:8080/v1" KEY="sk-local" MODEL="model" \
    PROMPT="Write a shell script to monitor disk usage" \
    VERBOSE=true \
    ./probe.sh
echo ""

# Test llama.cpp through LiteLLM (DeepSeek model)
echo -e "${BLUE}Testing llama.cpp DeepSeek via LiteLLM...${NC}"
BASE="http://localhost:4000/v1" KEY="${LITELLM_MASTER_KEY:-${LITELLM_VIRTUAL_KEY}}" \
    MODEL="local-deepseek" \
    PROMPT="Implement a LRU cache in Python" \
    ./probe.sh
echo ""

# Test llama.cpp through LiteLLM (Kimi model)
echo -e "${BLUE}Testing llama.cpp Kimi-K2 via LiteLLM...${NC}"
BASE="http://localhost:4000/v1" KEY="${LITELLM_MASTER_KEY:-${LITELLM_VIRTUAL_KEY}}" \
    MODEL="local-kimi" \
    PROMPT="Explain memory management in Rust" \
    ./probe.sh
echo ""

# Get server info
echo -e "${BLUE}llama.cpp server info:${NC}"
curl -s http://localhost:8080/props 2>/dev/null | jq '.' || echo "Unable to get server info"
echo ""

echo -e "${GREEN}✓ llama.cpp tests complete${NC}"