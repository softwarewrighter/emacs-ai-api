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
;; Core Variables
;; ============================================================================

(defvar gptel-unified-models nil
  "All available models from LiteLLM.")

(defvar gptel-unified-backend nil
  "The unified LiteLLM backend.")

;; ============================================================================
;; Model Discovery
;; ============================================================================

(defun gptel-unified-discover-models ()
  "Discover all available models from LiteLLM."
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
              (setq gptel-unified-models models)
              models)))
      (error nil))))

(defun gptel-unified-refresh ()
  "Refresh the backend with discovered models."
  (interactive)
  (let ((models (gptel-unified-discover-models)))
    (if models
        (progn
          (setq gptel-unified-backend
                (gptel-make-openai "LiteLLM"
                  :host "localhost:4000"
                  :endpoint "/v1/chat/completions"
                  :protocol "http"
                  :key "sk-local-test-key-123"
                  :stream t
                  :models models))
          (setq gptel-backend gptel-unified-backend)
          (message "Discovered %d models" (length models)))
      (message "Failed to discover models. Check LiteLLM connection."))))

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
  (let ((url "http://localhost:4000/spend/logs")
        (url-request-method "GET")
        (url-request-extra-headers 
         '(("Authorization" . "Bearer sk-local-test-key-123"))))
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
      (insert "  C-c C-m    - Select any model (with completion)\n")
      (insert "  C-c C-1    - Quick: Best quality (Claude/GPT-4o)\n")
      (insert "  C-c C-2    - Quick: Fast/cheap (Haiku/GPT-4o-mini)\n")
      (insert "  C-c C-3    - Quick: Local (Ollama models)\n\n")
      
      (insert "Sending Text:\n")
      (insert "  C-c RET    - Send buffer/paragraph at point\n")
      (insert "  C-c C-SPC  - Send selected region\n")
      (insert "  C-c C-n    - Open new chat buffer\n\n")
      
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

;; Model selection
(global-set-key (kbd "C-c C-m") #'gptel-unified-select-model)
(global-set-key (kbd "C-c C-1") (lambda () (interactive) 
                                  (gptel-unified-quick-select "claude-3-5-sonnet\\|gpt-4o$" "best")))
(global-set-key (kbd "C-c C-2") (lambda () (interactive) 
                                  (gptel-unified-quick-select "haiku\\|gpt-4o-mini\\|flash" "fast")))
(global-set-key (kbd "C-c C-3") (lambda () (interactive) 
                                  (gptel-unified-quick-select "llama\\|qwen\\|mistral" "local")))

;; Sending
(global-set-key (kbd "C-c RET") #'gptel-send)
(global-set-key (kbd "C-c C-SPC") #'gptel-send-region)
(global-set-key (kbd "C-c C-n") #'gptel)

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