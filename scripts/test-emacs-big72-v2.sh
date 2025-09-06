#!/usr/bin/env bash
# Test Emacs with big72.local Ollama model in batch mode
# Version 2: Uses external elisp file for better maintainability

set -euo pipefail

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m'

echo -e "${BLUE}Testing Emacs with big72.local Ollama (v2)${NC}"
echo "==========================================="
echo ""

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
ELISP_FILE="$SCRIPT_DIR/elisp/test-big72-simple.el"

# Check if elisp file exists
if [ ! -f "$ELISP_FILE" ]; then
    echo -e "${RED}Error: Elisp file not found: $ELISP_FILE${NC}"
    echo "Please ensure elisp/test-big72-simple.el exists"
    exit 1
fi

# Validate elisp file first (if validator exists)
if [ -f "$SCRIPT_DIR/validate_elisp.sh" ]; then
    echo "Validating elisp file..."
    if "$SCRIPT_DIR/validate_elisp.sh" "$ELISP_FILE" > /dev/null 2>&1; then
        echo -e "${GREEN}✓ Elisp validation passed${NC}"
    else
        echo -e "${RED}✗ Elisp validation failed${NC}"
        echo "Running validator with output:"
        "$SCRIPT_DIR/validate_elisp.sh" "$ELISP_FILE"
        exit 1
    fi
    echo ""
fi

# Check if LiteLLM is running
echo "Checking LiteLLM status..."
if curl -s http://localhost:4000/health > /dev/null 2>&1; then
    echo -e "${GREEN}✓ LiteLLM is running${NC}"
else
    echo -e "${RED}✗ LiteLLM is not running${NC}"
    echo "Please start it with: cd llm-gateway && docker compose up -d"
    exit 1
fi
echo ""

# Check if big72 model is available
echo "Checking model availability..."
if curl -s -H "Authorization: Bearer sk-local-test-key-123" \
     http://localhost:4000/v1/models | grep -q "qwen2.5:7b"; then
    echo -e "${GREEN}✓ qwen2.5:7b model is available${NC}"
else
    echo -e "${RED}✗ qwen2.5:7b model not found${NC}"
    echo "Available models:"
    curl -s -H "Authorization: Bearer sk-local-test-key-123" \
         http://localhost:4000/v1/models | jq -r '.data[].id' | head -10
    exit 1
fi
echo ""

# Run Emacs in batch mode
echo -e "${BLUE}Running Emacs batch test...${NC}"
echo "----------------------------------------"

emacs -Q --batch \
      --eval "(setq default-directory \"$PROJECT_ROOT/\")" \
      -l "$ELISP_FILE" 2>&1

EXIT_CODE=$?

echo "----------------------------------------"

if [ $EXIT_CODE -eq 0 ]; then
    echo -e "${GREEN}✓ Test completed successfully${NC}"
    echo ""
    echo "Summary:"
    echo "  - Model: qwen2.5:7b (on big72.local)"
    echo "  - Endpoint: http://localhost:4000/v1"
    echo "  - Backend: http://big72.local:11434"
else
    echo -e "${RED}✗ Test failed (exit code: $EXIT_CODE)${NC}"
    echo ""
    echo "Troubleshooting:"
    echo "  1. Check if big72.local is reachable: ping big72.local"
    echo "  2. Check if Ollama is running on big72: curl http://big72.local:11434/api/tags"
    echo "  3. Check LiteLLM logs: docker compose logs litellm"
fi

exit $EXIT_CODE