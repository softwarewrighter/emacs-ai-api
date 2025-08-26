#!/usr/bin/env bash
# Test OpenAI GPT models via LiteLLM gateway

set -euo pipefail

# Source environment or use defaults
BASE="${BASE:-http://localhost:4000/v1}"
KEY="${KEY:-${LITELLM_MASTER_KEY:-${LITELLM_VIRTUAL_KEY}}}"

# Color output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}Testing OpenAI Models via LiteLLM${NC}"
echo "================================="
echo ""

# Test GPT-4o
echo -e "${BLUE}Testing GPT-4o...${NC}"
BASE="$BASE" KEY="$KEY" MODEL="gpt-4o" \
    PROMPT="Write a Python list comprehension to get squares of even numbers from 1-10" \
    ./probe.sh
echo ""

# Test GPT-4o-mini
echo -e "${BLUE}Testing GPT-4o-mini...${NC}"
BASE="$BASE" KEY="$KEY" MODEL="gpt-4o-mini" \
    PROMPT="Explain Python decorators in one sentence" \
    ./probe.sh
echo ""

# Test o1-preview (reasoning model)
echo -e "${BLUE}Testing o1-preview (reasoning)...${NC}"
BASE="$BASE" KEY="$KEY" MODEL="o1-preview" \
    PROMPT="Find the bug: def factorial(n): return n * factorial(n) if n > 0 else 1" \
    ./probe.sh
echo ""

# Test o1-mini
echo -e "${BLUE}Testing o1-mini...${NC}"
BASE="$BASE" KEY="$KEY" MODEL="o1-mini" \
    PROMPT="What's the time complexity of binary search?" \
    ./probe.sh
echo ""

# Test coding-best alias (should route to GPT-4o or Claude)
echo -e "${BLUE}Testing coding-best alias...${NC}"
BASE="$BASE" KEY="$KEY" MODEL="coding-best" \
    PROMPT="Write a Rust function to reverse a linked list" \
    ./probe.sh
echo ""

echo -e "${GREEN}✓ OpenAI tests complete${NC}"