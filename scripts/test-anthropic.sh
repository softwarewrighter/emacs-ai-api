#!/usr/bin/env bash
# Test Anthropic Claude models via LiteLLM gateway

set -euo pipefail

# Source environment or use defaults
BASE="${BASE:-http://localhost:4000/v1}"
KEY="${KEY:-${LITELLM_MASTER_KEY:-${LITELLM_VIRTUAL_KEY}}}"

# Color output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}Testing Anthropic Claude Models via LiteLLM${NC}"
echo "==========================================="
echo ""

# Test Claude 3.5 Sonnet
echo -e "${BLUE}Testing Claude 3.5 Sonnet...${NC}"
BASE="$BASE" KEY="$KEY" MODEL="claude-3-5-sonnet" \
    PROMPT="Write a Python context manager for database connections" \
    ./probe.sh
echo ""

# Test Claude 3.5 Haiku
echo -e "${BLUE}Testing Claude 3.5 Haiku...${NC}"
BASE="$BASE" KEY="$KEY" MODEL="claude-3-5-haiku" \
    PROMPT="Explain async/await in JavaScript briefly" \
    ./probe.sh
echo ""

# Test Claude 3 Opus
echo -e "${BLUE}Testing Claude 3 Opus...${NC}"
BASE="$BASE" KEY="$KEY" MODEL="claude-3-opus" \
    PROMPT="Design a simple rate limiter in Go" \
    ./probe.sh
echo ""

echo -e "${GREEN}✓ Anthropic tests complete${NC}"