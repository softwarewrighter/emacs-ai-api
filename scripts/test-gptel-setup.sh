#!/usr/bin/env bash
# Verify gptel setup with local LLMs

set -euo pipefail

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

echo -e "${BLUE}═══════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}     gptel Setup Verification${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════${NC}"
echo ""

# Step 1: Check if LiteLLM is running
echo -e "${BLUE}Step 1: Checking LiteLLM status...${NC}"
if curl -s http://localhost:4000/health/readiness >/dev/null 2>&1; then
    echo -e "${GREEN}✓${NC} LiteLLM is running on port 4000"
else
    echo -e "${YELLOW}⚠${NC} LiteLLM not running. Start it with:"
    echo "  cd scripts && ./start-litellm-local.sh"
    echo ""
fi

# Step 2: Check Ollama instances
echo -e "${BLUE}Step 2: Checking Ollama instances...${NC}"

# Check localhost
if curl -s http://localhost:11434/api/tags >/dev/null 2>&1; then
    echo -e "${GREEN}✓${NC} Ollama running on localhost"
    MODELS=$(curl -s http://localhost:11434/api/tags | jq -r '.models[]?.name' | head -3 | paste -sd ', ' -)
    echo "  Models: $MODELS"
else
    echo -e "${YELLOW}⚠${NC} Ollama not running on localhost"
fi

# Check big72.local
if curl -s --connect-timeout 2 http://big72.local:11434/api/tags >/dev/null 2>&1; then
    echo -e "${GREEN}✓${NC} Ollama running on big72.local"
    MODELS=$(curl -s http://big72.local:11434/api/tags | jq -r '.models[]?.name' | head -3 | paste -sd ', ' -)
    echo "  Models: $MODELS"
else
    echo -e "${YELLOW}⚠${NC} Ollama not accessible on big72.local"
fi

echo ""
echo -e "${BLUE}Step 3: Testing with Emacs...${NC}"

# Create test elisp file
TEMP_EL=$(mktemp /tmp/gptel-test-XXXX.el)

cat > "$TEMP_EL" <<'EOF'
;; Test gptel installation
(require 'package)
(package-initialize)

(if (require 'gptel nil t)
    (progn
      (message "✓ gptel is installed")
      
      ;; Try to create a backend
      (condition-case err
          (let ((backend (gptel-make-openai "Test"
                           :host "localhost:4000"
                           :endpoint "/v1/chat/completions"
                           :protocol "http"
                           :key "sk-local-test-key-123"
                           :models '("localhost-llama3"))))
            (message "✓ Backend created successfully")
            (kill-emacs 0))
        (error
         (message "✗ Failed to create backend: %s" err)
         (kill-emacs 1))))
  (message "✗ gptel not installed")
  (message "Install with: M-x package-install RET gptel RET")
  (kill-emacs 1))
EOF

# Change to project directory
cd "$(dirname "$0")/.."

# Run Emacs test
echo "Testing gptel in Emacs..."
if emacs -Q --batch -l "$TEMP_EL" 2>&1; then
    echo -e "${GREEN}✓${NC} gptel is properly configured"
else
    echo -e "${RED}✗${NC} gptel configuration issue"
    echo ""
    echo "To install gptel in Emacs:"
    echo "1. Open Emacs"
    echo "2. M-x package-refresh-contents"
    echo "3. M-x package-install RET gptel RET"
fi

# Clean up
rm -f "$TEMP_EL"

echo ""
echo -e "${BLUE}═══════════════════════════════════════════════════════${NC}"
echo -e "${GREEN}     Setup Instructions${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════${NC}"
echo ""
echo "1. Start LiteLLM (if not running):"
echo "   cd scripts && ./start-litellm-local.sh"
echo ""
echo "2. In Emacs, load the configuration:"
echo "   M-x load-file RET"
echo "   ~/github/softwarewrighter/emacs-ai-api/llm-gateway/emacs/gptel-local-test.el"
echo ""
echo "   Or for simple testing:"
echo "   ~/github/softwarewrighter/emacs-ai-api/llm-gateway/emacs/gptel-simple-test.el"
echo ""
echo "3. Test with key bindings:"
echo "   Full config:   C-c g 1 (localhost), C-c g 3 (big72), C-c g s (send)"
echo "   Simple config: C-c t l (LiteLLM), C-c t o (Ollama), C-c t s (send)"
echo ""
echo "4. Or use commands:"
echo "   M-x gptel-send           ; Send current buffer"
echo "   M-x gptel-send-region    ; Send selected region"
echo "   M-x gptel-menu           ; Open gptel menu"
echo ""