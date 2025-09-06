#!/usr/bin/env bash
# Test Emacs with big72.local Ollama model in batch mode

set -euo pipefail

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m'

echo -e "${BLUE}Testing Emacs with big72.local Ollama${NC}"
echo "======================================"
echo ""

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
ELISP_FILE="$SCRIPT_DIR/elisp/test-big72-ollama.el"

# Validate elisp file first
if [ -f "$SCRIPT_DIR/validate_elisp.sh" ]; then
    echo "Validating elisp file..."
    if "$SCRIPT_DIR/validate_elisp.sh" "$ELISP_FILE" > /dev/null 2>&1; then
        echo -e "${GREEN}✓ Elisp validation passed${NC}"
    else
        echo -e "${RED}✗ Elisp validation failed${NC}"
        exit 1
    fi
fi

# Check if elisp file exists
if [ ! -f "$ELISP_FILE" ]; then
    echo -e "${RED}Error: Elisp file not found: $ELISP_FILE${NC}"
    exit 1
fi

# Run Emacs in batch mode
echo -e "${BLUE}Running Emacs batch test...${NC}"
echo ""

emacs -Q --batch \
      --eval "(setq default-directory \"$PROJECT_ROOT/\")" \
      -l "$ELISP_FILE" 2>&1

EXIT_CODE=$?

if [ $EXIT_CODE -eq 0 ]; then
    echo ""
    echo -e "${GREEN}✓ big72.local test completed successfully${NC}"
else
    echo ""
    echo -e "${RED}✗ big72.local test failed (exit code: $EXIT_CODE)${NC}"
fi

exit $EXIT_CODE