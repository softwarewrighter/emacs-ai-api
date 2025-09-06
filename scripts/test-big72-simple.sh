#!/usr/bin/env bash
# Simple test for big72.local Ollama via LiteLLM

set -euo pipefail

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m'

echo -e "${BLUE}Testing big72.local Ollama via LiteLLM${NC}"
echo "======================================"
echo ""

# First, test direct API access
echo "1. Testing direct API to LiteLLM..."
RESPONSE=$(curl -s -X POST http://localhost:4000/v1/chat/completions \
  -H "Authorization: Bearer sk-local-test-key-123" \
  -H "Content-Type: application/json" \
  -d '{
    "model": "qwen2.5:7b",
    "messages": [{"role": "user", "content": "Say hello in 3 words"}],
    "max_tokens": 20
  }' | jq -r '.choices[0].message.content' 2>/dev/null || echo "FAILED")

if [ "$RESPONSE" != "FAILED" ] && [ -n "$RESPONSE" ]; then
    echo -e "${GREEN}✓ Direct API test passed${NC}"
    echo "  Response: $RESPONSE"
else
    echo -e "${RED}✗ Direct API test failed${NC}"
    echo "  Check if LiteLLM is running and big72.local is accessible"
    exit 1
fi

echo ""
echo "2. Testing with Emacs batch mode..."

# Create a simple test that just makes the API call
cat > /tmp/test-big72.el << 'ELISP'
;;; Simple test for big72 via LiteLLM

(require 'url)
(require 'json)

(defun test-litellm-api ()
  "Test LiteLLM API directly."
  (let* ((url "http://localhost:4000/v1/chat/completions")
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")
            ("Authorization" . "Bearer sk-local-test-key-123")))
         (url-request-data
          (json-encode
           '((model . "qwen2.5:7b")
             (messages . [((role . "user") (content . "Reply with: test passed"))])
             (max_tokens . 20)
             (temperature . 0.1))))
         (response-buffer (url-retrieve-synchronously url nil t 10)))
    (if response-buffer
        (with-current-buffer response-buffer
          (goto-char (point-min))
          (re-search-forward "^$" nil t)
          (forward-char)
          (condition-case nil
              (let* ((json-response (json-read))
                     (choices (cdr (assoc 'choices json-response)))
                     (first-choice (aref choices 0))
                     (message-obj (cdr (assoc 'message first-choice)))
                     (content (cdr (assoc 'content message-obj))))
                (kill-buffer response-buffer)
                (message "Response: %s" content)
                (message "✓ Emacs test PASSED")
                0)
            (error
             (message "✗ Failed to parse response")
             1)))
      (progn
        (message "✗ No response from API")
        1))))

;; Run the test
(let ((result (test-litellm-api)))
  (kill-emacs result))
ELISP

# Run Emacs test
if emacs -Q --batch -l /tmp/test-big72.el 2>&1; then
    echo -e "${GREEN}✓ Emacs batch test passed${NC}"
else
    echo -e "${RED}✗ Emacs batch test failed${NC}"
fi

# Clean up
rm -f /tmp/test-big72.el

echo ""
echo -e "${BLUE}Test Summary:${NC}"
echo "- Model tested: qwen2.5:7b (on big72.local)"
echo "- API endpoint: http://localhost:4000/v1"
echo "- Authentication: sk-local-test-key-123"
echo ""
echo "To use in interactive Emacs:"
echo "1. Load unified config: M-x load-file RET ~/path/to/gptel-unified.el"
echo "2. Press C-c C-m to select qwen2.5:7b"
echo "3. Press C-c C-n to open new chat"
echo "4. Type message and press C-c RET to send"