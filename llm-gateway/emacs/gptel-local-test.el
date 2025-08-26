;;; gptel-local-test.el --- Test configuration for local LLMs via LiteLLM -*- lexical-binding: t; -*-

;; Simple configuration for testing localhost and big72.local models

;;; Code:

(require 'gptel)

;; ============================================================================
;; Backend Configuration
;; ============================================================================

;; Store backends in variables for easy access
(defvar gptel-backend-litellm nil "LiteLLM backend")
(defvar gptel-backend-ollama-local nil "Local Ollama backend")
(defvar gptel-backend-ollama-big72 nil "Big72 Ollama backend")

;; LiteLLM Gateway backend (routes to all local models)
(setq gptel-backend-litellm
      (gptel-make-openai "LiteLLM-Local"
        :host "localhost:4000"
        :endpoint "/v1/chat/completions"
        :protocol "http"
        :key "sk-local-test-key-123"  ; Our test master key
        :stream t
        :models '("localhost-llama3"        ; Llama on your Mac
                  "localhost-qwen-coder"     ; Qwen Coder on your Mac  
                  "localhost-llama-vision"   ; Llama Vision on your Mac
                  "big72-default"            ; Model on big72.local
                  "coding-local"             ; Routes to fastest available
                  "chat-local")))            ; Chat routing alias

;; Direct Ollama backend for localhost (bypass LiteLLM)
(setq gptel-backend-ollama-local
      (gptel-make-ollama "Ollama-Localhost"
        :host "localhost:11434"
        :stream t
        :models '("llama3.2:latest"
                  "qwen2.5-coder:14b"
                  "llama3.2-vision:11b")))

;; Direct Ollama backend for big72.local (bypass LiteLLM)
(setq gptel-backend-ollama-big72
      (gptel-make-ollama "Ollama-Big72"
        :host "big72.local:11434"
        :stream t
        :models '("llama3.2:latest"
                  "mistral:latest"
                  "qwen2.5-coder:14b"
                  "deepseek-r1:14b")))

;; ============================================================================
;; Quick Switching Functions
;; ============================================================================

(defun use-localhost-llama ()
  "Use Llama model on localhost via LiteLLM."
  (interactive)
  (setq gptel-backend gptel-backend-litellm)
  (setq gptel-model "localhost-llama3")
  (message "Using localhost Llama via LiteLLM"))

(defun use-localhost-qwen ()
  "Use Qwen Coder on localhost via LiteLLM."
  (interactive)
  (setq gptel-backend gptel-backend-litellm)
  (setq gptel-model "localhost-qwen-coder")
  (message "Using localhost Qwen Coder via LiteLLM"))

(defun use-big72-model ()
  "Use model on big72.local via LiteLLM."
  (interactive)
  (setq gptel-backend gptel-backend-litellm)
  (setq gptel-model "big72-default")
  (message "Using big72.local model via LiteLLM"))

(defun use-localhost-direct ()
  "Use Ollama on localhost directly (no LiteLLM)."
  (interactive)
  (setq gptel-backend gptel-backend-ollama-local)
  (setq gptel-model "llama3.2:latest")
  (message "Using localhost Ollama directly"))

(defun use-big72-direct ()
  "Use Ollama on big72.local directly (no LiteLLM)."
  (interactive)
  (setq gptel-backend gptel-backend-ollama-big72)
  (setq gptel-model "llama3.2:latest")
  (message "Using big72.local Ollama directly"))

(defun show-current-llm ()
  "Show current LLM backend and model."
  (interactive)
  (if (and (boundp 'gptel-backend) gptel-backend)
      (message "Backend: %s | Model: %s | Stream: %s"
               (if (fboundp 'gptel-backend-name)
                   (gptel-backend-name gptel-backend)
                 "Unknown")
               (or gptel-model "None")
               (if gptel-stream "enabled" "disabled"))
    (message "No backend configured yet")))

;; ============================================================================
;; Test Functions
;; ============================================================================

(defun test-current-llm ()
  "Test the current LLM with a simple prompt."
  (interactive)
  (let ((buf (get-buffer-create "*LLM Test*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "Testing %s/%s\n\n" 
                      (if (and (boundp 'gptel-backend) gptel-backend
                               (fboundp 'gptel-backend-name))
                          (gptel-backend-name gptel-backend)
                        "Unknown backend")
                      (or gptel-model "No model")))
      (insert "Prompt: Write a haiku about coding\n\n")
      (goto-char (point-max)))
    (display-buffer buf)
    (with-current-buffer buf
      (gptel-send))))

(defun test-all-models ()
  "Test all configured models."
  (interactive)
  (let ((buf (get-buffer-create "*LLM Test All*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "Testing All Models\n")
      (insert "==================\n\n"))
    (display-buffer buf)
    
    ;; Test each model
    (dolist (test '((use-localhost-llama "Localhost Llama")
                    (use-localhost-qwen "Localhost Qwen")
                    (use-big72-model "Big72 Model")))
      (funcall (car test))
      (with-current-buffer buf
        (goto-char (point-max))
        (insert (format "\n--- %s ---\n" (cadr test)))
        (insert "Say 'Hello from " (cadr test) "':\n")
        (gptel-send)
        (sit-for 3)))))  ; Wait 3 seconds between tests

;; ============================================================================
;; Key Bindings
;; ============================================================================

;; Define a prefix map for local LLM commands
(defvar local-llm-map (make-sparse-keymap)
  "Keymap for local LLM commands.")

;; Model selection
(define-key local-llm-map (kbd "1") #'use-localhost-llama)
(define-key local-llm-map (kbd "2") #'use-localhost-qwen)
(define-key local-llm-map (kbd "3") #'use-big72-model)
(define-key local-llm-map (kbd "d") #'use-localhost-direct)
(define-key local-llm-map (kbd "D") #'use-big72-direct)

;; Actions
(define-key local-llm-map (kbd "s") #'gptel-send)          ; Send current buffer
(define-key local-llm-map (kbd "S") #'gptel-send-region)   ; Send region
(define-key local-llm-map (kbd "m") #'gptel-menu)          ; Open menu
(define-key local-llm-map (kbd "?") #'show-current-llm)    ; Show status
(define-key local-llm-map (kbd "t") #'test-current-llm)    ; Test current
(define-key local-llm-map (kbd "T") #'test-all-models)     ; Test all

;; Bind the prefix map to C-c g (you can change this)
(global-set-key (kbd "C-c g") local-llm-map)

;; ============================================================================
;; Set Defaults
;; ============================================================================

;; Start with localhost via LiteLLM
(setq gptel-backend gptel-backend-litellm)
(setq gptel-model "localhost-llama3")
(setq gptel-stream t)  ; Enable streaming

(message "Local LLM configuration loaded! Use C-c g ? to see current settings.")
(message "Quick keys: C-c g 1 (localhost), C-c g 3 (big72), C-c g s (send)")

(provide 'gptel-local-test)
;;; gptel-local-test.el ends here