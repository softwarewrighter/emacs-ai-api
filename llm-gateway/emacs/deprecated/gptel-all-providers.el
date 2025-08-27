;;; gptel-all-providers.el --- Dynamic multi-provider configuration via LiteLLM -*- lexical-binding: t; -*-

;;; Commentary:
;; Dynamically discovers and configures all available models from LiteLLM
;; No hardcoded model lists - adapts to whatever is configured

;;; Code:

(require 'gptel)
(require 'url)
(require 'json)

;; ============================================================================
;; Dynamic Model Discovery
;; ============================================================================

(defvar gptel-litellm-models nil
  "Dynamically discovered models from LiteLLM.")

(defvar gptel-litellm-backend nil
  "The LiteLLM backend instance.")

(defun gptel-discover-models ()
  "Discover all available models from LiteLLM."
  (interactive)
  (let ((url "http://localhost:4000/v1/models")
        (url-request-method "GET")
        (url-request-extra-headers 
         '(("Authorization" . "Bearer sk-local-test-key-123"))))
    (condition-case err
        (with-current-buffer (url-retrieve-synchronously url nil t 5)
          (goto-char (point-min))
          (when (re-search-forward "\n\n" nil t)
            (let* ((json (json-read))
                   (models (mapcar (lambda (m) (cdr (assoc 'id m)))
                                 (cdr (assoc 'data json)))))
              (setq gptel-litellm-models models)
              (message "Discovered %d models from LiteLLM" (length models))
              models)))
      (error
       (message "Failed to discover models: %s" err)
       nil))))

(defun gptel-refresh-backend ()
  "Refresh the LiteLLM backend with discovered models."
  (interactive)
  (let ((models (or (gptel-discover-models) 
                    '("gpt-4o-mini"))))  ; Fallback
    (setq gptel-litellm-backend
          (gptel-make-openai "LiteLLM-Dynamic"
            :host "localhost:4000"
            :endpoint "/v1/chat/completions"
            :protocol "http"
            :key "sk-local-test-key-123"
            :stream t
            :models models))
    (setq gptel-backend gptel-litellm-backend)
    (message "Backend refreshed with %d models" (length models))))

;; ============================================================================
;; Interactive Model Selection
;; ============================================================================

(defun gptel-select-any-model ()
  "Interactively select from all available models."
  (interactive)
  (unless gptel-litellm-models
    (gptel-discover-models))
  (if gptel-litellm-models
      (let ((model (completing-read "Select model: " 
                                   gptel-litellm-models 
                                   nil t)))
        (setq gptel-model model)
        (message "Selected: %s" model))
    (message "No models available. Check LiteLLM connection.")))

(defun gptel-select-by-provider (provider)
  "Select a model from a specific PROVIDER."
  (interactive "sProvider (openai/anthropic/gemini/ollama): ")
  (unless gptel-litellm-models
    (gptel-discover-models))
  (let ((filtered-models 
         (seq-filter (lambda (m) 
                      (cond
                       ((string= provider "openai") 
                        (string-match-p "^gpt\\|^o1" m))
                       ((string= provider "anthropic") 
                        (string-match-p "^claude" m))
                       ((string= provider "gemini") 
                        (string-match-p "^gemini" m))
                       ((string= provider "ollama") 
                        (string-match-p "^llama\\|^qwen\\|^mistral\\|^deepseek" m))
                       (t nil)))
                    gptel-litellm-models)))
    (if filtered-models
        (let ((model (completing-read 
                     (format "Select %s model: " provider)
                     filtered-models nil t)))
          (setq gptel-model model)
          (message "Selected: %s" model))
      (message "No %s models available" provider))))

;; ============================================================================
;; Model Categories (Smart Selection)
;; ============================================================================

(defun gptel-select-best-available ()
  "Select the best available model (Claude Sonnet > GPT-4o > Gemini Pro)."
  (interactive)
  (unless gptel-litellm-models
    (gptel-discover-models))
  (let ((model (or (seq-find (lambda (m) (string-match-p "claude-3-5-sonnet" m)) 
                            gptel-litellm-models)
                   (seq-find (lambda (m) (string-match-p "^gpt-4o$" m)) 
                            gptel-litellm-models)
                   (seq-find (lambda (m) (string-match-p "gemini-1.5-pro" m)) 
                            gptel-litellm-models)
                   (car gptel-litellm-models))))
    (if model
        (progn
          (setq gptel-model model)
          (message "Selected best available: %s" model))
      (message "No models available"))))

(defun gptel-select-fast-model ()
  "Select a fast, cheap model (Haiku > GPT-4o-mini > Gemini Flash > Ollama)."
  (interactive)
  (unless gptel-litellm-models
    (gptel-discover-models))
  (let ((model (or (seq-find (lambda (m) (string-match-p "haiku" m)) 
                            gptel-litellm-models)
                   (seq-find (lambda (m) (string-match-p "gpt-4o-mini" m)) 
                            gptel-litellm-models)
                   (seq-find (lambda (m) (string-match-p "flash" m)) 
                            gptel-litellm-models)
                   (seq-find (lambda (m) (string-match-p "llama\\|qwen" m)) 
                            gptel-litellm-models))))
    (if model
        (progn
          (setq gptel-model model)
          (message "Selected fast model: %s" model))
      (message "No fast models available"))))

(defun gptel-select-coding-model ()
  "Select a model optimized for coding."
  (interactive)
  (unless gptel-litellm-models
    (gptel-discover-models))
  (let ((model (or (seq-find (lambda (m) (string-match-p "sonnet\\|gpt-4o\\|opus" m)) 
                            gptel-litellm-models)
                   (seq-find (lambda (m) (string-match-p "coder\\|deepseek" m)) 
                            gptel-litellm-models)
                   (car gptel-litellm-models))))
    (if model
        (progn
          (setq gptel-model model)
          (message "Selected coding model: %s" model))
      (message "No coding models available"))))

;; ============================================================================
;; Model Information
;; ============================================================================

(defun gptel-list-all-models ()
  "List all available models with provider info."
  (interactive)
  (unless gptel-litellm-models
    (gptel-discover-models))
  (if gptel-litellm-models
      (let ((buffer (get-buffer-create "*Available LLM Models*")))
        (with-current-buffer buffer
          (read-only-mode -1)
          (erase-buffer)
          (insert "Available Models via LiteLLM\n")
          (insert "============================\n\n")
          
          ;; Group by provider
          (let ((openai-models (seq-filter (lambda (m) (string-match-p "^gpt\\|^o1" m)) 
                                          gptel-litellm-models))
                (anthropic-models (seq-filter (lambda (m) (string-match-p "^claude" m)) 
                                             gptel-litellm-models))
                (gemini-models (seq-filter (lambda (m) (string-match-p "^gemini" m)) 
                                          gptel-litellm-models))
                (ollama-models (seq-filter (lambda (m) 
                                            (not (or (string-match-p "^gpt\\|^o1\\|^claude\\|^gemini" m))))
                                          gptel-litellm-models)))
            
            (when openai-models
              (insert "OpenAI Models:\n")
              (dolist (model openai-models)
                (insert (format "  • %s\n" model)))
              (insert "\n"))
            
            (when anthropic-models
              (insert "Anthropic Models:\n")
              (dolist (model anthropic-models)
                (insert (format "  • %s\n" model)))
              (insert "\n"))
            
            (when gemini-models
              (insert "Google Gemini Models:\n")
              (dolist (model gemini-models)
                (insert (format "  • %s\n" model)))
              (insert "\n"))
            
            (when ollama-models
              (insert "Local/Ollama Models:\n")
              (dolist (model ollama-models)
                (insert (format "  • %s\n" model)))
              (insert "\n")))
          
          (insert "\nCommands:\n")
          (insert "  C-c l m - Select any model\n")
          (insert "  C-c l b - Select best available\n")
          (insert "  C-c l f - Select fast model\n")
          (insert "  C-c l c - Select coding model\n")
          (insert "  q - Close this buffer\n")
          
          (read-only-mode 1)
          (local-set-key (kbd "q") #'quit-window)
          (goto-char (point-min)))
        (display-buffer buffer))
    (message "No models discovered. Check LiteLLM connection.")))

;; ============================================================================
;; Usage and Cost Tracking (reuse from gptel-openai.el)
;; ============================================================================

(load-file (expand-file-name "gptel-openai.el" 
                            (file-name-directory (or load-file-name buffer-file-name))))

;; ============================================================================
;; Initialization
;; ============================================================================

;; Discover models on load
(gptel-refresh-backend)

;; Key bindings - Model Selection
(global-set-key (kbd "C-c l r") #'gptel-refresh-backend)
(global-set-key (kbd "C-c l l") #'gptel-list-all-models)
(global-set-key (kbd "C-c l m") #'gptel-select-any-model)
(global-set-key (kbd "C-c l p") #'gptel-select-by-provider)
(global-set-key (kbd "C-c l b") #'gptel-select-best-available)
(global-set-key (kbd "C-c l f") #'gptel-select-fast-model)
(global-set-key (kbd "C-c l c") #'gptel-select-coding-model)

;; Key bindings - Sending Text
(global-set-key (kbd "C-c l s") #'gptel-safe-send)
(global-set-key (kbd "C-c l RET") #'gptel-send)
(global-set-key (kbd "C-c RET") #'gptel-send)  ; Standard gptel binding
(global-set-key (kbd "C-c l SPC") #'gptel-send-region)  ; Send selected region

;; Key bindings - Utilities
(global-set-key (kbd "C-c l u") #'gptel-check-usage)
(global-set-key (kbd "C-c l ?") #'gptel-show-current-config)
(global-set-key (kbd "C-c l n") #'gptel)  ; Open new gptel buffer

(message "Dynamic multi-provider configuration loaded!")
(message "Commands: C-c l l (list), C-c l m (select), C-c l b (best), C-c l f (fast)")
(message "Discovered %d models. Use C-c l r to refresh." (length gptel-litellm-models))

(provide 'gptel-all-providers)
;;; gptel-all-providers.el ends here