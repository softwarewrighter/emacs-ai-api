;;; gptel-unified.el --- Single unified LLM configuration with all providers -*- lexical-binding: t; -*-

;;; Commentary:
;; ONE configuration file for all LLM providers via LiteLLM
;; Dynamic model discovery, usage tracking, and comprehensive help
;; 
;; LOAD THIS FILE ONLY - replaces all other gptel-*.el files

;;; Code:

(require 'gptel)
(require 'url)
(require 'json)

;; ============================================================================
;; Environment Variable Support
;; ============================================================================

(defun gptel-unified-load-env-file (&optional env-file)
  "Load environment variables from ENV-FILE (defaults to ~/.env).
Returns an alist of (VAR . VALUE) pairs."
  (let ((file (expand-file-name (or env-file "~/.env")))
        (env-vars '()))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (not (eobp))
          ;; Skip comments and empty lines
          (cond
           ((looking-at "^\\s-*#.*$") nil)  ; Comment line
           ((looking-at "^\\s-*$") nil)      ; Empty line
           ((looking-at "^\\([A-Za-z_][A-Za-z0-9_]*\\)=\\(.*\\)$")
            (let ((var (match-string 1))
                  (val (match-string 2)))
              ;; Trim whitespace
              (setq val (string-trim val))
              ;; Remove surrounding quotes if present
              (when (string-match "^[\"']\\(.*\\)[\"']$" val)
                (setq val (match-string 1 val)))
              (push (cons var val) env-vars))))
          (forward-line 1))))
    (nreverse env-vars)))

(defun gptel-unified-get-env (var &optional default)
  "Get environment variable VAR from ~/.env or system env.
Falls back to DEFAULT if not found."
  (or
   ;; First try ~/.env file
   (cdr (assoc var (gptel-unified-load-env-file)))
   ;; Then try system environment
   (getenv var)
   ;; Finally use default
   default))

;; ============================================================================
;; Core Variables
;; ============================================================================

(defvar gptel-unified-models nil
  "All available models from LiteLLM.")

(defvar gptel-unified-backend nil
  "The unified LiteLLM backend.")

(defvar gptel-unified-litellm-key nil
  "API key for LiteLLM, loaded from environment.")

(defvar gptel-unified-litellm-host nil
  "Host for LiteLLM, loaded from environment.")

;; ============================================================================
;; Configuration Loading
;; ============================================================================

(defun gptel-unified-load-config ()
  "Load configuration from environment variables."
  (setq gptel-unified-litellm-key
        (gptel-unified-get-env "LITELLM_KEY" "sk-local-test-key-123"))
  (setq gptel-unified-litellm-host
        (gptel-unified-get-env "LITELLM_HOST" "localhost:4000")))

(defun gptel-unified-debug-env ()
  "Debug environment variable loading."
  (interactive)
  (let ((env-vars (gptel-unified-load-env-file)))
    (with-output-to-temp-buffer "*ENV Debug*"
      (princ "Environment variables from ~/.env:\n")
      (princ "===================================\n\n")
      (if env-vars
          (dolist (pair env-vars)
            (princ (format "%s = %s\n" (car pair) (cdr pair))))
        (princ "No environment variables found in ~/.env\n"))
      (princ "\n\nCurrent configuration:\n")
      (princ "======================\n")
      (princ (format "LITELLM_HOST: %s\n" gptel-unified-litellm-host))
      (princ (format "LITELLM_KEY: %s\n" gptel-unified-litellm-key))
      (princ "\nNote: Check for typos or extra characters in your ~/.env file"))))

;; ============================================================================
;; Model Discovery
;; ============================================================================

(defun gptel-unified-discover-models ()
  "Discover all available models from LiteLLM."
  (gptel-unified-load-config)  ;; Ensure config is loaded
  (let ((url (format "http://%s/v1/models" gptel-unified-litellm-host))
        (url-request-method "GET")
        (url-request-extra-headers
         `(("Authorization" . ,(format "Bearer %s" gptel-unified-litellm-key)))))
    (condition-case err
        (with-current-buffer (url-retrieve-synchronously url nil t 5)
          (goto-char (point-min))
          (when (re-search-forward "\n\n" nil t)
            (let* ((json (json-read))
                   (models (mapcar (lambda (m) (cdr (assoc 'id m)))
                                 (cdr (assoc 'data json)))))
              (setq gptel-unified-models models)
              models)))
      (error nil))))

(defun gptel-unified-refresh ()
  "Refresh the backend with discovered models."
  (interactive)
  (gptel-unified-load-config)  ;; Reload config in case ~/.env changed
  (let ((models (gptel-unified-discover-models)))
    (if models
        (progn
          (setq gptel-unified-backend
                (gptel-make-openai "LiteLLM"
                  :host gptel-unified-litellm-host
                  :endpoint "/v1/chat/completions"
                  :protocol "http"
                  :key gptel-unified-litellm-key
                  :stream t
                  :models models))
          (setq gptel-backend gptel-unified-backend)
          (message "Discovered %d models (host: %s)"
                   (length models) gptel-unified-litellm-host))
      (message "Failed to discover models. Check LiteLLM connection at %s"
               gptel-unified-litellm-host))))

;; ============================================================================
;; Model Selection
;; ============================================================================

(defun gptel-unified-select-model ()
  "Select any available model."
  (interactive)
  (unless gptel-unified-models
    (gptel-unified-discover-models))
  (if gptel-unified-models
      (let ((model (completing-read "Select model: " 
                                   gptel-unified-models 
                                   nil t)))
        (setq gptel-model model)
        (message "Selected: %s" model))
    (message "No models available. Try M-x gptel-unified-refresh")))

(defun gptel-unified-quick-select (pattern name)
  "Quickly select first model matching PATTERN with NAME."
  (unless gptel-unified-models
    (gptel-unified-discover-models))
  (let ((model (seq-find (lambda (m) (string-match-p pattern m))
                        gptel-unified-models)))
    (if model
        (progn
          (setq gptel-model model)
          (message "Selected %s: %s" name model))
      (message "No %s model available" name))))

;; ============================================================================
;; Usage Tracking
;; ============================================================================

(defun gptel-unified-fetch-usage ()
  "Fetch usage data from LiteLLM."
  (gptel-unified-load-config)  ;; Ensure config is loaded
  (let ((url (format "http://%s/spend/logs" gptel-unified-litellm-host))
        (url-request-method "GET")
        (url-request-extra-headers
         `(("Authorization" . ,(format "Bearer %s" gptel-unified-litellm-key)))))
    (condition-case nil
        (with-current-buffer (url-retrieve-synchronously url nil t 10)
          (goto-char (point-min))
          (when (re-search-forward "\n\n" nil t)
            (let ((json-array-type 'list)
                  (json-object-type 'alist))
              (json-read))))
      (error nil))))

(defun gptel-unified-usage ()
  "Display usage report."
  (interactive)
  (let ((data (gptel-unified-fetch-usage))
        (buffer (get-buffer-create "*LLM Usage*")))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert "LLM Usage Report\n")
      (insert "================\n\n")
      
      (if data
          (let ((total-cost 0.0)
                (total-tokens 0)
                (entries-with-usage
                 (seq-filter (lambda (entry)
                              (and (cdr (assoc 'model entry))
                                   (> (or (cdr (assoc 'total_tokens entry)) 0) 0)))
                            data)))
            
            ;; Recent requests
            (insert "Recent Requests:\n\n")
            (dolist (entry (seq-take entries-with-usage 10))
              (let* ((model (cdr (assoc 'model entry)))
                     (tokens (cdr (assoc 'total_tokens entry)))
                     (cost (cdr (assoc 'spend entry)))
                     (api-base (cdr (assoc 'api_base entry)))
                     (location (cond
                               ((string-match "docker\\.internal" (or api-base "")) "@localhost")
                               ((string-match "big72" (or api-base "")) "@big72")
                               ((string-match "openai" (or api-base "")) "@openai")
                               ((string-match "anthropic" (or api-base "")) "@anthropic")
                               ((string-match "google" (or api-base "")) "@google")
                               (t ""))))
                (insert (format "• %s%s\n" model location))
                (insert (format "  Tokens: %d | Cost: $%.6f\n\n"
                              (or tokens 0) (or cost 0.0)))
                (when tokens (setq total-tokens (+ total-tokens tokens)))
                (when cost (setq total-cost (+ total-cost cost)))))
            
            ;; Summary
            (insert (format "\nTotal: %d tokens, $%.6f\n" total-tokens total-cost)))
        (insert "No usage data available.\n"))
      
      (insert "\nPress 'q' to close, 'g' to refresh\n")
      (read-only-mode 1)
      (local-set-key (kbd "q") #'quit-window)
      (local-set-key (kbd "g") #'gptel-unified-usage)
      (goto-char (point-min)))
    (display-buffer buffer)))

;; ============================================================================
;; Help System
;; ============================================================================

(defun gptel-unified-help ()
  "Show comprehensive help for LLM commands."
  (interactive)
  (let ((buffer (get-buffer-create "*LLM Help*")))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert "LLM Unified Configuration - Help\n")
      (insert "=================================\n\n")
      
      (insert "CURRENT STATUS:\n")
      (insert (format "  Backend: %s\n" (if gptel-backend "Connected" "Not connected")))
      (insert (format "  Model: %s\n" (or gptel-model "None selected")))
      (insert (format "  Models available: %d\n\n" (length gptel-unified-models)))
      
      (insert "KEY BINDINGS:\n\n")
      (insert "Model Selection:\n")
      (insert "  C-c m      - Select any model (with completion)\n")
      (insert "  C-c 1      - Quick: Best quality (Claude/GPT-4o)\n")
      (insert "  C-c 2      - Quick: Fast/cheap (Haiku/GPT-4o-mini)\n")
      (insert "  C-c 3      - Quick: Local (Ollama models)\n\n")

      (insert "Sending Text:\n")
      (insert "  C-c RET    - Send buffer/paragraph at point\n")
      (insert "  C-c C-SPC  - Send selected region\n")
      (insert "  C-c n      - Open new chat buffer\n\n")
      
      (insert "Utilities:\n")
      (insert "  C-c C-h    - This help\n")
      (insert "  C-c C-u    - Show usage/costs\n")
      (insert "  C-c C-r    - Refresh models from LiteLLM\n")
      (insert "  C-c C-l    - List all available models\n")
      (insert "  C-c C-?    - Show current configuration\n\n")
      
      (insert "AVAILABLE MODELS:\n")
      (if gptel-unified-models
          (let ((openai (seq-filter (lambda (m) (string-match-p "^gpt\\|^o1" m)) gptel-unified-models))
                (anthropic (seq-filter (lambda (m) (string-match-p "^claude" m)) gptel-unified-models))
                (google (seq-filter (lambda (m) (string-match-p "^gemini" m)) gptel-unified-models))
                (local (seq-filter (lambda (m) (not (string-match-p "^gpt\\|^o1\\|^claude\\|^gemini" m))) 
                                  gptel-unified-models)))
            (when openai
              (insert "\nOpenAI:\n")
              (dolist (m openai) (insert (format "  • %s\n" m))))
            (when anthropic
              (insert "\nAnthropic:\n")
              (dolist (m anthropic) (insert (format "  • %s\n" m))))
            (when google
              (insert "\nGoogle:\n")
              (dolist (m google) (insert (format "  • %s\n" m))))
            (when local
              (insert "\nLocal/Ollama:\n")
              (dolist (m local) (insert (format "  • %s\n" m)))))
        (insert "  No models discovered. Press C-c C-r to refresh.\n"))
      
      (insert "\nPress 'q' to close\n")
      (read-only-mode 1)
      (local-set-key (kbd "q") #'quit-window)
      (goto-char (point-min)))
    (display-buffer buffer)))

(defun gptel-unified-list-models ()
  "List all available models."
  (interactive)
  (unless gptel-unified-models
    (gptel-unified-discover-models))
  (if gptel-unified-models
      (message "Available models: %s" (string-join gptel-unified-models ", "))
    (message "No models available")))

(defun gptel-unified-status ()
  "Show current configuration."
  (interactive)
  (message "Model: %s | Backend: %s | Total models: %d"
           (or gptel-model "none")
           (if gptel-backend "LiteLLM" "disconnected")
           (length gptel-unified-models)))

;; ============================================================================
;; Initialization
;; ============================================================================

;; Discover and setup on load
(gptel-unified-refresh)

;; ============================================================================
;; Key Bindings - ALL commands use C-c C-<key> pattern
;; ============================================================================

;; Model selection - use C-c m instead of C-c C-m to avoid conflicts
(global-set-key (kbd "C-c m") #'gptel-unified-select-model)
(global-set-key (kbd "C-c 1") (lambda () (interactive)
                                  (gptel-unified-quick-select "claude-3-5-sonnet\\|gpt-4o$" "best")))
(global-set-key (kbd "C-c 2") (lambda () (interactive)
                                  (gptel-unified-quick-select "haiku\\|gpt-4o-mini\\|flash" "fast")))
(global-set-key (kbd "C-c 3") (lambda () (interactive)
                                  (gptel-unified-quick-select "llama\\|qwen\\|mistral" "local")))

;; Sending
(global-set-key (kbd "C-c RET") #'gptel-send)
(global-set-key (kbd "C-c C-SPC") #'gptel-send-region)
(global-set-key (kbd "C-c n") #'gptel)

;; Utilities
(global-set-key (kbd "C-c C-h") #'gptel-unified-help)
(global-set-key (kbd "C-c C-u") #'gptel-unified-usage)
(global-set-key (kbd "C-c C-r") #'gptel-unified-refresh)
(global-set-key (kbd "C-c C-l") #'gptel-unified-list-models)
(global-set-key (kbd "C-c C-?") #'gptel-unified-status)

;; ============================================================================
;; Startup Message
;; ============================================================================

(message "gptel-unified loaded: %d models available. Press C-c C-h for help."
         (length gptel-unified-models))

(provide 'gptel-unified)
;;; gptel-unified.el ends here