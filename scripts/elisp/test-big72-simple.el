;;; test-big72-simple.el --- Simple test for big72 via LiteLLM -*- lexical-binding: t -*-

;;; Commentary:
;; Simple synchronous test for big72.local Ollama via LiteLLM
;; This bypasses gptel's async machinery for easier batch testing

;;; Code:

(require 'url)
(require 'json)

(defun test-litellm-api ()
  "Test LiteLLM API directly with big72 model."
  (let* ((url "http://localhost:4000/v1/chat/completions")
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")
            ("Authorization" . "Bearer sk-local-test-key-123")))
         (url-request-data
          (json-encode
           '((model . "qwen2.5:7b")
             (messages . [((role . "user") (content . "Write a one-line bash command to list files."))])
             (max_tokens . 100)
             (temperature . 0.7))))
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
                (message "Response: %s" content)
                (message "✓ Test PASSED for big72.local (qwen2.5:7b)")
                0)
            (error
             (message "✗ Failed to parse response: %s" (error-message-string err))
             (kill-buffer response-buffer)
             1)))
      (progn
        (message "✗ No response from API")
        1))))

;; Run the test
(message "Testing big72.local model via LiteLLM...")
(message "API endpoint: http://localhost:4000/v1")
(message "Model: qwen2.5:7b")
(message "")

(let ((result (test-litellm-api)))
  (kill-emacs result))

(provide 'test-big72-simple)
;;; test-big72-simple.el ends here