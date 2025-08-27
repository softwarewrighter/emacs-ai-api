#!/bin/bash
# Batch Emacs test script for OpenAI models via LiteLLM
# Tests GPT-4o and GPT-4o-mini models end-to-end

set -e

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "================================================"
echo "Testing OpenAI Models via LiteLLM with Emacs"
echo "================================================"

# Test configuration
TEST_PROMPT="What is 2+2? Reply with just the number."
EMACS_CONFIG="/Users/mike/github/softwarewrighter/emacs-ai-api/llm-gateway/emacs/gptel-openai.el"
TIMEOUT=30

# Function to test a model
test_model() {
    local model=$1
    local select_key=$2
    
    echo -e "\n${YELLOW}Testing model: $model${NC}"
    
    # Create temporary file for output
    TMPFILE=$(mktemp /tmp/emacs-openai-test-XXXXXX)
    
    # Create Emacs Lisp test script
    cat > /tmp/test-openai-model.el << EOF
;; Load the OpenAI configuration
(load-file "$EMACS_CONFIG")

;; Select the model
$select_key

;; Create a test buffer with the prompt
(with-temp-buffer
  (insert "$TEST_PROMPT")
  (goto-char (point-min))
  
  ;; Send to GPT
  (condition-case err
      (progn
        (gptel-safe-send)
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
             --load /tmp/test-openai-model.el 2>/dev/null; then
        
        # Check the response
        if [ -f "$TMPFILE" ]; then
            RESPONSE=$(cat "$TMPFILE")
            echo -e "${GREEN}✓ Success: Model $model responded${NC}"
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
        echo -e "${RED}✗ Failed: Model $model (exit code: $EXIT_CODE)${NC}"
        
        # Try to diagnose the issue
        case $EXIT_CODE in
            1) echo "  Timeout: No response within $TIMEOUT seconds" ;;
            2) echo "  Error during API call" ;;
            *) echo "  Unknown error" ;;
        esac
    fi
    
    # Clean up
    rm -f "$TMPFILE" /tmp/test-openai-model.el
}

# Check if LiteLLM is running
if ! curl -s http://localhost:4000/health > /dev/null 2>&1; then
    echo -e "${RED}Error: LiteLLM doesn't appear to be running at localhost:4000${NC}"
    echo "Please ensure Docker containers are running: cd llm-gateway && docker compose up -d"
    exit 1
fi

# Check if OpenAI API key is configured
if ! curl -s -H "Authorization: Bearer sk-local-test-key-123" \
     http://localhost:4000/v1/models | grep -q "gpt-4o"; then
    echo -e "${RED}Warning: OpenAI models not found. Is OPENAI_API_KEY configured?${NC}"
    echo "Check llm-gateway/.env.openai file"
fi

# Test GPT-4o-mini (cheaper, faster)
test_model "gpt-4o-mini" "(gptel-use-gpt4o-mini)"

# Test GPT-4o (more capable)
test_model "gpt-4o" "(gptel-use-gpt4o)"

echo -e "\n================================================"
echo "Test Complete"
echo "================================================"
echo ""
echo "To test interactively in Emacs:"
echo "1. Open Emacs"
echo "2. Load the configuration: M-x load-file RET $EMACS_CONFIG"
echo "3. Select a model:"
echo "   - C-c o m  : GPT-4o-mini (cheaper)"
echo "   - C-c o g  : GPT-4o (more capable)"
echo "4. Check available models: C-c o c"
echo "5. Type your prompt and send: C-c o s (safe send with error handling)"
echo "6. View current config: C-c o ?"