#!/usr/bin/env bash
# Test Emacs with big72.local Ollama model in batch mode

set -euo pipefail

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${BLUE}Testing Emacs with big72.local Ollama${NC}"
echo "======================================"
echo ""

# Create temporary elisp test file
TEMP_EL=$(mktemp /tmp/test-big72-XXXX.el)

cat > "$TEMP_EL" <<'EOF'
;;; Test big72.local Ollama via LiteLLM

;; Add paths
(add-to-list 'load-path (expand-file-name "llm-gateway/emacs" default-directory))

;; Minimal gptel setup
(require 'package)
(package-initialize)

;; Load gptel if available, otherwise define minimal version
(unless (require 'gptel nil t)
  ;; Minimal gptel mock for testing
  (defvar gptel-model nil)
  (defvar gptel-api-key "sk-local-test-key-123")
  
  (defun gptel-request (prompt &rest args)
    "Make a test request."
    (let* ((url "http://localhost:4000/v1/chat/completions")
           (url-request-method "POST")
           (url-request-extra-headers
            `(("Content-Type" . "application/json")
              ("Authorization" . ,(concat "Bearer " gptel-api-key))))
           (url-request-data
            (json-encode
             `((model . ,gptel-model)
               (messages . [((role . "user") (content . ,prompt))])
               (max_tokens . 100)
               (temperature . 0.7))))
           (response-buffer (url-retrieve-synchronously url nil t 10)))
      (when response-buffer
        (with-current-buffer response-buffer
          (goto-char (point-min))
          (re-search-forward "^$" nil t)
          (forward-char)
          (let* ((json-response (json-read))
                 (choices (cdr (assoc 'choices json-response)))
                 (first-choice (aref choices 0))
                 (message-obj (cdr (assoc 'message first-choice)))
                 (content (cdr (assoc 'content message-obj))))
            (kill-buffer response-buffer)
            content))))))

;; Configure for big72.local
(setq gptel-model "big72-default")

;; Test function
(defun test-big72-ollama ()
  "Test big72.local Ollama model."
  (message "Testing model: %s" gptel-model)
  (message "API endpoint: http://localhost:4000/v1")
  (message "Actual backend: http://big72.local:11434")
  (message "")
  
  (let ((prompt "Write a one-line bash command to list files."))
    (message "Prompt: %s" prompt)
    (message "")
    (condition-case err
        (let ((response (gptel-request prompt)))
          (if response
              (progn
                (message "Response received:")
                (message "----------------------------------------")
                (message "%s" response)
                (message "----------------------------------------")
                (message "✓ Test PASSED for big72.local Ollama")
                (kill-emacs 0))
            (message "✗ No response received")
            (kill-emacs 1)))
      (error
       (message "✗ Test FAILED: %s" (error-message-string err))
       (kill-emacs 1)))))

;; Run test
(test-big72-ollama)
EOF

# Change to project directory
cd "$(dirname "$0")/.."

# Run Emacs in batch mode
echo -e "${BLUE}Running Emacs batch test...${NC}"
echo ""

emacs -Q --batch \
      --eval "(setq default-directory \"$(pwd)/\")" \
      -l "$TEMP_EL" 2>&1

EXIT_CODE=$?

# Clean up
rm -f "$TEMP_EL"

if [ $EXIT_CODE -eq 0 ]; then
    echo ""
    echo -e "${GREEN}✓ big72.local test completed successfully${NC}"
else
    echo ""
    echo -e "✗ big72.local test failed"
fi

exit $EXIT_CODE