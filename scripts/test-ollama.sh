#!/usr/bin/env bash
# Test local Ollama models directly and via LiteLLM

set -euo pipefail

# Color output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${BLUE}Testing Ollama Models${NC}"
echo "====================="
echo ""

# Check if Ollama is running
if ! curl -s http://localhost:11434/api/tags >/dev/null 2>&1; then
    echo -e "${YELLOW}Warning: Ollama doesn't appear to be running on localhost:11434${NC}"
    echo "Start Ollama with: ollama serve"
    echo ""
fi

# Test direct Ollama access
echo -e "${BLUE}Testing direct Ollama access...${NC}"
BASE="http://localhost:11434/v1" KEY="sk-local" MODEL="deepseek-coder:latest" \
    PROMPT="Write a function to check if a string is a palindrome" \
    ./probe.sh
echo ""

# Test Ollama through LiteLLM
echo -e "${BLUE}Testing Ollama via LiteLLM gateway...${NC}"
BASE="http://localhost:4000/v1" KEY="${LITELLM_MASTER_KEY:-${LITELLM_VIRTUAL_KEY}}" \
    MODEL="ollama-deepseek" \
    PROMPT="Implement a stack using a linked list" \
    ./probe.sh
echo ""

# Test Qwen model
echo -e "${BLUE}Testing Qwen 2.5 Coder via LiteLLM...${NC}"
BASE="http://localhost:4000/v1" KEY="${LITELLM_MASTER_KEY:-${LITELLM_VIRTUAL_KEY}}" \
    MODEL="ollama-qwen" \
    PROMPT="Write a regex to validate email addresses" \
    ./probe.sh
echo ""

# Test Llama model
echo -e "${BLUE}Testing Llama 3.2 via LiteLLM...${NC}"
BASE="http://localhost:4000/v1" KEY="${LITELLM_MASTER_KEY:-${LITELLM_VIRTUAL_KEY}}" \
    MODEL="ollama-llama" \
    PROMPT="Explain the difference between TCP and UDP" \
    ./probe.sh
echo ""

# List available Ollama models
echo -e "${BLUE}Available Ollama models:${NC}"
curl -s http://localhost:11434/api/tags 2>/dev/null | jq -r '.models[]?.name' || echo "Unable to list models"
echo ""

echo -e "${GREEN}✓ Ollama tests complete${NC}"