#!/bin/bash
# Test script for unified gptel configuration
# Tests various models (local and cloud) via LiteLLM

set -e

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "================================================"
echo "Testing Unified LLM Configuration"
echo "================================================"

# Test configuration
TEST_PROMPT="What is 2+2? Reply with just the number."
EMACS_CONFIG="/Users/mike/github/softwarewrighter/emacs-ai-api/llm-gateway/emacs/gptel-unified.el"
TIMEOUT=30

# Function to test a model
test_model() {
    local model=$1
    local description=$2
    
    echo -e "\n${YELLOW}Testing: $description ($model)${NC}"
    
    # Create temporary file for output
    TMPFILE=$(mktemp /tmp/emacs-unified-test-XXXXXX)
    
    # Create Emacs Lisp test script
    cat > /tmp/test-unified-model.el << EOF
;; Load the unified configuration
(load-file "$EMACS_CONFIG")

;; Select the specific model
(setq gptel-model "$model")

;; Create a test buffer with the prompt
(with-temp-buffer
  (insert "$TEST_PROMPT")
  (goto-char (point-min))
  
  ;; Send to GPT
  (condition-case err
      (progn
        (gptel-send)
        ;; Wait for response (max $TIMEOUT seconds)
        (let ((start-time (current-time))
              (timeout-seconds $TIMEOUT))
          (while (and (< (float-time (time-subtract (current-time) start-time)) timeout-seconds)
                      (not (save-excursion
                             (goto-char (point-min))
                             (re-search-forward "^[0-9]+" nil t))))
            (sit-for 1))
          
          ;; Check if we got a response
          (if (save-excursion
                (goto-char (point-min))
                (re-search-forward "^[0-9]+" nil t))
              (progn
                (write-region (point-min) (point-max) "$TMPFILE")
                (message "SUCCESS: Response received from %s" gptel-model)
                (kill-emacs 0))
            (progn
              (message "TIMEOUT: No response from %s within %s seconds" gptel-model timeout-seconds)
              (kill-emacs 1)))))
    (error
     (message "ERROR: %s" (error-message-string err))
     (kill-emacs 2))))
EOF
    
    # Run Emacs in batch mode
    if emacs --batch --no-init-file \
             --eval "(require 'package)" \
             --eval "(package-initialize)" \
             --eval "(require 'gptel)" \
             --load /tmp/test-unified-model.el 2>/dev/null; then
        
        # Check the response
        if [ -f "$TMPFILE" ]; then
            RESPONSE=$(cat "$TMPFILE")
            echo -e "${GREEN}✓ Success: $description responded${NC}"
            echo "  Response: $RESPONSE"
            
            # Verify it's actually the number 4
            if echo "$RESPONSE" | grep -q "4"; then
                echo -e "  ${GREEN}✓ Correct answer received${NC}"
            else
                echo -e "  ${RED}✗ Unexpected answer${NC}"
            fi
        fi
    else
        EXIT_CODE=$?
        echo -e "${RED}✗ Failed: $description (exit code: $EXIT_CODE)${NC}"
        
        # Try to diagnose the issue
        case $EXIT_CODE in
            1) echo "  Timeout: No response within $TIMEOUT seconds" ;;
            2) echo "  Error during API call" ;;
            *) echo "  Unknown error" ;;
        esac
    fi
    
    # Clean up
    rm -f "$TMPFILE" /tmp/test-unified-model.el
}

# Check if LiteLLM is running
if ! curl -s http://localhost:4000/health > /dev/null 2>&1; then
    echo -e "${RED}Error: LiteLLM doesn't appear to be running at localhost:4000${NC}"
    echo "Please ensure Docker containers are running: cd llm-gateway && docker compose up -d"
    exit 1
fi

# Show available models
echo "Available models:"
curl -s http://localhost:4000/v1/models -H "Authorization: Bearer sk-local-test-key-123" | \
    jq -r '.data[].id' | head -10 | sed 's/^/  - /'

echo ""
echo "Running tests..."

# Test a local model (Ollama)
test_model "llama3.2:latest" "Ollama Local"

# Test OpenAI if available
if curl -s -H "Authorization: Bearer sk-local-test-key-123" \
     http://localhost:4000/v1/models | grep -q "gpt-4o-mini"; then
    test_model "gpt-4o-mini" "OpenAI GPT-4o-mini"
else
    echo -e "${YELLOW}Skipping OpenAI tests (models not available)${NC}"
fi

echo -e "\n================================================"
echo "Test Complete"
echo "================================================"
echo ""
echo "Key bindings for the unified configuration:"
echo "  C-c C-h    : Comprehensive help"
echo "  C-c C-m    : Select any model"
echo "  C-c C-1    : Quick select best quality"
echo "  C-c C-2    : Quick select fast/cheap"
echo "  C-c C-3    : Quick select local"
echo "  C-c RET    : Send text"
echo "  C-c C-u    : View usage/costs"
echo ""
echo "To test interactively:"
echo "1. Open Emacs"
echo "2. Load: M-x load-file RET $EMACS_CONFIG"
echo "3. Press C-c C-h for help"