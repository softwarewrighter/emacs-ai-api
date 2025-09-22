#!/usr/bin/env bash
# Purpose: Verify that Emacs can connect to localhost Ollama models through LiteLLM

set -euo pipefail

# Help function
show_help() {
    cat << EOF
PURPOSE:
    Test Emacs connectivity to localhost Ollama models through LiteLLM gateway.
    Verifies the complete pipeline: Emacs -> LiteLLM -> localhost Ollama -> Model

USAGE:
    $0
    $0 -h | --help

DESCRIPTION:
    This script performs a comprehensive test of the localhost Ollama integration:
    1. Validates the elisp test code for structural correctness
    2. Checks that LiteLLM gateway is running
    3. Verifies localhost Ollama is accessible
    4. Confirms the llama3.2:latest model is available
    5. Runs an Emacs batch test to verify the full pipeline

REQUIREMENTS:
    - Docker containers running (cd llm-gateway && docker compose up -d)
    - Ollama running on localhost:11434
    - llama3.2:latest model pulled (ollama pull llama3.2)
    - Emacs installed

FILES USED:
    - elisp/test-localhost-simple.el (test implementation)
    - validate_elisp.sh (optional elisp validator)

EXIT CODES:
    0  Success - all tests passed
    1  Failure - check error messages for details

EOF
    exit 0
}

# Check for help flag
if [[ "${1:-}" == "-h" ]] || [[ "${1:-}" == "--help" ]]; then
    show_help
fi

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m'

echo -e "${BLUE}Testing Emacs with Localhost Ollama${NC}"
echo "===================================="
echo ""

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
ELISP_FILE="$SCRIPT_DIR/elisp/test-localhost-simple.el"

# Check if elisp file exists
if [ ! -f "$ELISP_FILE" ]; then
    echo -e "${RED}Error: Elisp file not found: $ELISP_FILE${NC}"
    echo "Please ensure elisp/test-localhost-simple.el exists"
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

# Check if localhost Ollama is running
echo "Checking localhost Ollama..."
if curl -s http://localhost:11434/api/tags > /dev/null 2>&1; then
    echo -e "${GREEN}✓ Ollama is running on localhost${NC}"
else
    echo -e "${RED}✗ Ollama is not running on localhost${NC}"
    echo "Please start Ollama locally"
    exit 1
fi
echo ""

# Check if llama3.2:latest model is available
echo "Checking model availability..."
if curl -s -H "Authorization: Bearer sk-local-test-key-123" \
     http://localhost:4000/v1/models | grep -q "llama3.2:latest"; then
    echo -e "${GREEN}✓ llama3.2:latest model is available${NC}"
else
    echo -e "${RED}✗ llama3.2:latest model not found${NC}"
    echo "Available models:"
    curl -s -H "Authorization: Bearer sk-local-test-key-123" \
         http://localhost:4000/v1/models | jq -r '.data[].id' | head -10
    echo ""
    echo "You may need to pull the model: ollama pull llama3.2"
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
    echo "  - Model: llama3.2:latest (on localhost)"
    echo "  - Endpoint: http://localhost:4000/v1"
    echo "  - Backend: http://localhost:11434"
else
    echo -e "${RED}✗ Test failed (exit code: $EXIT_CODE)${NC}"
    echo ""
    echo "Troubleshooting:"
    echo "  1. Check if Ollama is running: ollama list"
    echo "  2. Check if model exists: ollama pull llama3.2"
    echo "  3. Check LiteLLM logs: docker compose logs litellm"
fi

exit $EXIT_CODE