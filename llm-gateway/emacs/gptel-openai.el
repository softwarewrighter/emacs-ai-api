;;; gptel-openai.el --- Configuration for OpenAI models via LiteLLM -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for using OpenAI models through LiteLLM gateway
;; Handles both local and cloud models

;;; Code:

(require 'gptel)

;; ============================================================================
;; Backend Configuration
;; ============================================================================

(defvar gptel-litellm-openai
  (gptel-make-openai "LiteLLM-OpenAI"
    :host "localhost:4000"
    :endpoint "/v1/chat/completions"
    :protocol "http"
    :key "sk-local-test-key-123"
    :stream t  ; Enable streaming for all models
    :models '("llama3.2:latest"
              "qwen2.5-coder:14b"
              "qwen2.5:7b"
              "mistral:latest"
              "gpt-4o"           ; Available with standard API
              "gpt-4o-mini"      ; Available with standard API
              "gpt-4-turbo")))   ; Available with standard API

;; ============================================================================
;; Model Selection Functions
;; ============================================================================

(defun gptel-use-gpt4o ()
  "Select GPT-4o model."
  (interactive)
  (setq gptel-backend gptel-litellm-openai)
  (setq gptel-model "gpt-4o")
  (setq gptel-stream t)
  (message "Selected: GPT-4o via LiteLLM"))

(defun gptel-use-gpt4o-mini ()
  "Select GPT-4o-mini model."
  (interactive)
  (setq gptel-backend gptel-litellm-openai)
  (setq gptel-model "gpt-4o-mini")
  (setq gptel-stream t)
  (message "Selected: GPT-4o-mini via LiteLLM"))

;; ============================================================================
;; Model Availability Check
;; ============================================================================

(defun gptel-check-openai-models ()
  "Check which OpenAI models are actually available."
  (interactive)
  (let ((url "http://localhost:4000/v1/models")
        (url-request-method "GET")
        (url-request-extra-headers 
         '(("Authorization" . "Bearer sk-local-test-key-123"))))
    (with-current-buffer (url-retrieve-synchronously url nil t 5)
      (goto-char (point-min))
      (when (re-search-forward "\n\n" nil t)
        (let* ((json (json-read))
               (models (mapcar (lambda (m) (cdr (assoc 'id m)))
                             (cdr (assoc 'data json))))
               (openai-models (seq-filter (lambda (m) 
                                          (string-match-p "^gpt\\|^o1" m))
                                        models)))
          (if openai-models
              (message "Available OpenAI models: %s" 
                       (string-join openai-models ", "))
            (message "No OpenAI models found. Check your API key.")))))))

;; ============================================================================
;; Safe Send with Error Handling
;; ============================================================================

(defun gptel-safe-send ()
  "Send to LLM with error handling."
  (interactive)
  (condition-case err
      (gptel-send)
    (error
     (message "Error sending to LLM: %s" (error-message-string err))
     (when (string-match-p ":null\\|null" (error-message-string err))
       (message "Retrying without streaming...")
       (let ((gptel-stream nil))
         (gptel-send))))))

;; ============================================================================
;; Usage Tracking Functions
;; ============================================================================

(defun gptel-fetch-usage-data ()
  "Fetch usage data from LiteLLM spend logs endpoint.
Returns the parsed JSON data or nil on error."
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

(defvar gptel-deprecated-model-names
  '("big72-default" "big72-qwen" "big72-mistral" 
    "localhost-llama3" "localhost-qwen-coder" "coding-local"
    "o1-preview" "o1-mini")
  "List of deprecated or renamed model names to filter out or mark.")

(defun gptel-calculate-usage-stats (data)
  "Calculate usage statistics from DATA.
Returns an alist with total-cost, total-tokens, and model-stats."
  (let ((total-cost 0.0)
        (total-tokens 0)
        (model-stats (make-hash-table :test 'equal)))
    (dolist (entry data)
      (let* ((model (cdr (assoc 'model entry)))
             (tokens (or (cdr (assoc 'total_tokens entry)) 0))
             (cost (or (cdr (assoc 'spend entry)) 0))
             (current (gethash model model-stats '(0 0 0.0))))
        (when model
          ;; Track all costs and tokens for totals
          (setq total-cost (+ total-cost cost))
          (setq total-tokens (+ total-tokens tokens))
          ;; Only include in model stats if it has actual usage (tokens > 0)
          ;; or if it's not a failed request
          (when (or (> tokens 0) (> cost 0))
            (puthash model 
                    (list (1+ (car current))
                          (+ (cadr current) tokens)
                          (+ (caddr current) cost))
                    model-stats)))))
    (list (cons 'total-cost total-cost)
          (cons 'total-tokens total-tokens)
          (cons 'model-stats model-stats))))

(defun gptel-format-usage-entry (entry)
  "Format a single usage ENTRY for display.
Returns a formatted string."
  (let* ((model (cdr (assoc 'model entry)))
         (provider (cdr (assoc 'custom_llm_provider entry)))
         (api-base (cdr (assoc 'api_base entry)))
         (tokens (cdr (assoc 'total_tokens entry)))
         (prompt-tokens (cdr (assoc 'prompt_tokens entry)))
         (completion-tokens (cdr (assoc 'completion_tokens entry)))
         (cost (cdr (assoc 'spend entry)))
         (time (cdr (assoc 'startTime entry)))
         ;; Determine location from api-base
         (location (cond
                    ((string-match "host\\.docker\\.internal" (or api-base ""))
                     "@localhost")
                    ((string-match "big72" (or api-base ""))
                     "@big72")
                    ((string-match "openai\\.com" (or api-base ""))
                     "@openai")
                    (t ""))))
    (concat
     (format "• Model: %s%s\n" model location)
     (format "  Provider: %s" (or provider "unknown"))
     (when api-base
       (format " (%s)\n" api-base))
     (unless api-base "\n")
     (when time
       (format "  Time: %s\n" (substring time 0 19)))
     (format "  Tokens: %d (prompt: %d, completion: %d)\n"
             (or tokens 0)
             (or prompt-tokens 0)
             (or completion-tokens 0))
     (format "  Cost: $%.8f\n" (or cost 0.0))
     "\n")))

(defun gptel-display-usage-report (data limit)
  "Display usage report with DATA showing up to LIMIT entries."
  (let ((buffer (get-buffer-create "*LLM Usage*"))
        (stats (gptel-calculate-usage-stats data)))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      
      ;; Header
      (insert "═══════════════════════════════════════════\n")
      (insert "          LiteLLM Usage Report\n")
      (insert "═══════════════════════════════════════════\n\n")
      
      (if (and data (listp data) (not (assoc 'detail data)))
          (progn
            ;; Filter out 0 token/0 cost entries first
            (let* ((entries-with-usage
                    (seq-filter (lambda (entry)
                                 (and (cdr (assoc 'model entry))
                                      (or (> (or (cdr (assoc 'total_tokens entry)) 0) 0)
                                          (> (or (cdr (assoc 'spend entry)) 0) 0))))
                               data))
                   (total-requests (length data))
                   (requests-with-usage (length entries-with-usage)))
              
              ;; Summary info
              (insert (format "Showing %d most recent (of %d with usage, %d total requests)\n\n" 
                            (min limit requests-with-usage)
                            requests-with-usage
                            total-requests))
              
              ;; Recent requests
              (insert "Recent Requests (with actual usage):\n")
              (insert "────────────────────────────────────────\n")
              (dolist (entry (seq-take entries-with-usage limit))
                (insert (gptel-format-usage-entry entry))))
            
            ;; Overall summary
            (insert "═══════════════════════════════════════════\n")
            (insert "Overall Summary (All Records):\n")
            (insert "───────────────────────────────────────────\n")
            (insert (format "Total Requests: %d\n" (length data)))
            (insert (format "Total Tokens Used: %d\n" 
                          (cdr (assoc 'total-tokens stats))))
            (insert (format "Total Cost: $%.8f\n" 
                          (cdr (assoc 'total-cost stats))))
            (when (> (length data) 0)
              (insert (format "Average Cost per Request: $%.8f\n" 
                            (/ (cdr (assoc 'total-cost stats)) 
                               (length data)))))
            
            ;; Model breakdown
            (gptel-display-model-breakdown (cdr (assoc 'model-stats stats))))
        
        ;; No data available
        (if (assoc 'detail data)
            (insert (format "Error: %s\n\nNo usage data available yet.\n" 
                          (cdr (assoc 'detail data))))
          (insert "No usage data available.\n")))
      
      ;; Footer with help
      (gptel-insert-usage-footer)
      
      ;; Setup buffer
      (read-only-mode 1)
      (local-set-key (kbd "q") #'quit-window)
      (local-set-key (kbd "g") #'gptel-check-usage)
      (goto-char (point-min)))
    
    (display-buffer buffer)))

(defun gptel-display-model-breakdown (model-stats)
  "Display breakdown of usage by model from MODEL-STATS hash table."
  (when (> (hash-table-count model-stats) 0)
    ;; Separate current and deprecated models
    (let ((current-models '())
          (deprecated-models '()))
      (maphash (lambda (model stats)
                 (if (member model gptel-deprecated-model-names)
                     (push (cons model stats) deprecated-models)
                   (push (cons model stats) current-models)))
               model-stats)
      
      ;; Display current models
      (when current-models
        (insert "\n───────────────────────────────────────────\n")
        (insert "Usage by Model (Current Names):\n")
        (insert "───────────────────────────────────────────\n")
        (let ((sorted-current 
               (sort current-models
                     (lambda (a b)
                       (> (caddr (cdr a)) (caddr (cdr b)))))))
          (dolist (model-entry sorted-current)
            (let ((model (car model-entry))
                  (stats (cdr model-entry)))
              (insert (format "• %s:\n" model))
              (insert (format "  Requests: %d | Tokens: %d | Cost: $%.8f\n"
                            (car stats) (cadr stats) (caddr stats)))))))
      
      ;; Display deprecated models only if they have significant usage
      (let ((deprecated-with-usage 
             (seq-filter (lambda (entry) 
                          (and (> (cadr (cdr entry)) 10)  ; tokens > 10 (not just failed requests)
                               (> (caddr (cdr entry)) 0))) ; cost > 0
                        deprecated-models)))
        (when deprecated-with-usage
          (insert "\n───────────────────────────────────────────\n")
          (insert "Legacy Model Names (before renaming):\n")
          (insert "───────────────────────────────────────────\n")
          (let ((sorted-deprecated 
                 (sort deprecated-with-usage
                       (lambda (a b)
                         (> (caddr (cdr a)) (caddr (cdr b)))))))
            (dolist (model-entry sorted-deprecated)
              (let ((model (car model-entry))
                    (stats (cdr model-entry)))
                (insert (format "• %s → %s:\n" 
                              model
                              (cond
                               ((string-match "localhost-llama" model) "llama3.2:latest")
                               ((string-match "localhost-qwen" model) "qwen2.5-coder:14b")
                               ((string-match "big72-qwen" model) "qwen2.5:7b")
                               ((string-match "big72-mistral" model) "mistral:latest")
                               (t "renamed"))))
                (insert (format "  Requests: %d | Tokens: %d | Cost: $%.8f\n"
                              (car stats) (cadr stats) (caddr stats)))))))))))

(defun gptel-insert-usage-footer ()
  "Insert footer with cost reference and commands."
  (insert "\n═══════════════════════════════════════════\n")
  (insert "Cost Reference (per 1M tokens):\n")
  (insert "───────────────────────────────────────────\n")
  (insert "• GPT-4o-mini:  Input: $0.15  Output: $0.60\n")
  (insert "• GPT-4o:       Input: $5.00  Output: $15.00\n")
  (insert "• GPT-4-turbo:  Input: $10.00 Output: $30.00\n")
  (insert "• Ollama models: Free (local compute)\n")
  
  (insert "\n═══════════════════════════════════════════\n")
  (insert "Commands:\n")
  (insert "───────────────────────────────────────────\n")
  (insert "• C-c o u or g : Refresh this usage report\n")
  (insert "• C-c o ?       : Show current model config\n")
  (insert "• q             : Close this buffer\n"))

(defun gptel-check-usage (&optional limit)
  "Check recent LLM usage and costs via LiteLLM.
Optional LIMIT argument specifies how many records to show (default 20)."
  (interactive "P")
  (let ((record-limit (or limit 20))
        (data (gptel-fetch-usage-data)))
    (if data
        (gptel-display-usage-report data record-limit)
      (message "Error fetching usage data. Is LiteLLM running?"))))

;; ============================================================================
;; Configuration Display
;; ============================================================================

(defun gptel-show-current-config ()
  "Show current configuration."
  (interactive)
  (message "Backend: %s | Model: %s | Stream: %s"
           (if gptel-backend
               (gptel-backend-name gptel-backend)
             "None")
           (or gptel-model "None")
           (if gptel-stream "enabled" "disabled")))

;; ============================================================================
;; Initialization
;; ============================================================================

;; Set defaults
(setq gptel-backend gptel-litellm-openai)
(setq gptel-model "gpt-4o-mini")  ; Start with mini as it's cheaper
(setq gptel-stream t)

;; Key bindings
(global-set-key (kbd "C-c o g") #'gptel-use-gpt4o)
(global-set-key (kbd "C-c o m") #'gptel-use-gpt4o-mini)
(global-set-key (kbd "C-c o c") #'gptel-check-openai-models)
(global-set-key (kbd "C-c o s") #'gptel-safe-send)
(global-set-key (kbd "C-c o ?") #'gptel-show-current-config)
(global-set-key (kbd "C-c o u") #'gptel-check-usage)

(message "OpenAI configuration loaded!")
(message "Commands: C-c o g (GPT-4o), C-c o m (GPT-4o-mini), C-c o s (send)")
(message "Check: C-c o c (models), C-c o u (usage), C-c o ? (config)")

(provide 'gptel-openai)
;;; gptel-openai.el ends here