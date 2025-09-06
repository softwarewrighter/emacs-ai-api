;;; test-big72-ollama.el --- Test big72.local Ollama via LiteLLM -*- lexical-binding: t -*-

;;; Commentary:
;; Test script for verifying big72.local Ollama models work through LiteLLM

;;; Code:

;; Add paths
(add-to-list 'load-path (expand-file-name "llm-gateway/emacs" default-directory))

;; Minimal gptel setup
(require 'package)
(package-initialize)

;; Load gptel and configure for LiteLLM
(if (require 'gptel nil t)
    ;; Use real gptel with LiteLLM backend
    (progn
      (require 'gptel-openai nil t)
      
      ;; Create LiteLLM backend
      (defvar gptel-litellm
        (gptel-make-openai "LiteLLM"
          :host "localhost:4000"
          :protocol "http"
          :endpoint "/v1/chat/completions"
          :key "sk-local-test-key-123"
          :models '("qwen2.5:7b")))
      
      ;; Set as default
      (setq gptel-backend gptel-litellm)
      (setq gptel-model "qwen2.5:7b"))
  
  ;; Fallback: minimal implementation for testing
  (progn
    (defvar gptel-model "qwen2.5:7b")
    (defvar gptel-api-key "sk-local-test-key-123")
    
    (defun gptel-request (prompt &rest args)
      "Make a test request to LiteLLM."
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
              content)))))))

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
        (let ((response 
               (if (fboundp 'gptel-request)
                   ;; Use our function
                   (gptel-request prompt)
                 ;; Use direct API call
                 (let* ((url "http://localhost:4000/v1/chat/completions")
                        (url-request-method "POST")
                        (url-request-extra-headers
                         `(("Content-Type" . "application/json")
                           ("Authorization" . "Bearer sk-local-test-key-123")))
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
                         content)))))))
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

(provide 'test-big72-ollama)
;;; test-big72-ollama.el ends here