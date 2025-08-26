#!/usr/bin/env bash
# Test Google Gemini models via LiteLLM gateway

set -euo pipefail

# Source environment or use defaults
BASE="${BASE:-http://localhost:4000/v1}"
KEY="${KEY:-${LITELLM_MASTER_KEY:-${LITELLM_VIRTUAL_KEY}}}"

# Color output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}Testing Google Gemini Models via LiteLLM${NC}"
echo "========================================"
echo ""

# Test Gemini 1.5 Pro
echo -e "${BLUE}Testing Gemini 1.5 Pro...${NC}"
BASE="$BASE" KEY="$KEY" MODEL="gemini-1.5-pro" \
    PROMPT="Write a TypeScript generic function for array filtering" \
    ./probe.sh
echo ""

# Test Gemini 1.5 Flash
echo -e "${BLUE}Testing Gemini 1.5 Flash...${NC}"
BASE="$BASE" KEY="$KEY" MODEL="gemini-1.5-flash" \
    PROMPT="Explain REST API principles in 3 bullet points" \
    ./probe.sh
echo ""

# Test Gemini 2.0 Flash Experimental
echo -e "${BLUE}Testing Gemini 2.0 Flash Experimental...${NC}"
BASE="$BASE" KEY="$KEY" MODEL="gemini-2.0-flash-exp" \
    PROMPT="Write a React hook for debounced input" \
    ./probe.sh
echo ""

echo -e "${GREEN}✓ Gemini tests complete${NC}"