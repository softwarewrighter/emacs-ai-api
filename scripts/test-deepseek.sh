#!/usr/bin/env bash
# Test DeepSeek models via LiteLLM gateway

set -euo pipefail

# Source environment or use defaults
BASE="${BASE:-http://localhost:4000/v1}"
KEY="${KEY:-${LITELLM_MASTER_KEY:-${LITELLM_VIRTUAL_KEY}}}"

# Color output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}Testing DeepSeek Models via LiteLLM${NC}"
echo "==================================="
echo ""

# Test DeepSeek Chat
echo -e "${BLUE}Testing DeepSeek Chat...${NC}"
BASE="$BASE" KEY="$KEY" MODEL="deepseek-chat" \
    PROMPT="Write a SQL query to find duplicate rows in a table" \
    ./probe.sh
echo ""

# Test DeepSeek Coder
echo -e "${BLUE}Testing DeepSeek Coder...${NC}"
BASE="$BASE" KEY="$KEY" MODEL="deepseek-coder" \
    PROMPT="Implement binary search in C++" \
    ./probe.sh
echo ""

# Test local DeepSeek via llama.cpp
echo -e "${BLUE}Testing local DeepSeek (llama.cpp)...${NC}"
BASE="$BASE" KEY="$KEY" MODEL="local-deepseek" \
    PROMPT="Write a bash function to check if a port is open" \
    ./probe.sh
echo ""

# Test local DeepSeek via Ollama
echo -e "${BLUE}Testing local DeepSeek (Ollama)...${NC}"
BASE="$BASE" KEY="$KEY" MODEL="ollama-deepseek" \
    PROMPT="Create a Python decorator for retry logic" \
    ./probe.sh
echo ""

echo -e "${GREEN}✓ DeepSeek tests complete${NC}"