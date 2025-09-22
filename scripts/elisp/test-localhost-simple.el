;;; test-localhost-simple.el --- Simple test for localhost Ollama via LiteLLM -*- lexical-binding: t -*-

;;; Commentary:
;; Purpose: Verify that localhost Ollama models are accessible through LiteLLM
;; This test makes a simple synchronous API call to ensure the pipeline works:
;;   Emacs -> LiteLLM (localhost:4000) -> Ollama (localhost:11434) -> Model response

;;; Code:

(require 'url)
(require 'json)

(defun test-litellm-api ()
  "Test LiteLLM API directly with localhost model."
  (let* ((test-prompt "Write a one-line Python function to add two numbers.")
         (url "http://localhost:4000/v1/chat/completions")
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")
            ("Authorization" . "Bearer sk-local-test-key-123")))
         (request-data
          `((model . "llama3.2:latest")  ; Use actual model name
            (messages . [((role . "user") (content . ,test-prompt))])
            (max_tokens . 100)
            (temperature . 0.7)))
         (url-request-data (json-encode request-data))
         (response-buffer (url-retrieve-synchronously url nil t 10)))
    (if response-buffer
        (with-current-buffer response-buffer
          (goto-char (point-min))
          ;; Skip HTTP headers
          (re-search-forward "^$" nil t)
          (forward-char)
          (condition-case err
              (let* ((json-response (json-read))
                     (choices (cdr (assoc 'choices json-response)))
                     (first-choice (aref choices 0))
                     (message-obj (cdr (assoc 'message first-choice)))
                     (content (cdr (assoc 'content message-obj))))
                (kill-buffer response-buffer)
                ;; Show the request
                (message "")
                (message "Request sent:")
                (message "----------------------------------------")
                (message "Model: llama3.2:latest")
                (message "Prompt: %s" test-prompt)
                (message "----------------------------------------")
                (message "")
                (message "Response received:")
                (message "----------------------------------------")
                (message "%s" content)
                (message "----------------------------------------")
                (message "")
                (message "✓ Test PASSED for localhost Ollama (llama3.2:latest)")
                0)
            (error
             (message "✗ Failed to parse response: %s" (error-message-string err))
             (kill-buffer response-buffer)
             1)))
      (progn
        (message "✗ No response from API")
        1))))

;; Run the test
(message "========================================")
(message "Purpose: Verify localhost Ollama connectivity through LiteLLM")
(message "========================================")
(message "")
(message "Testing pipeline:")
(message "  Emacs -> LiteLLM (localhost:4000) -> Ollama (localhost:11434)")
(message "")

(let ((result (test-litellm-api)))
  (kill-emacs result))

(provide 'test-localhost-simple)
;;; test-localhost-simple.el ends here