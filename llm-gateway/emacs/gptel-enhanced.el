;;; gptel-enhanced.el --- Enhanced gptel config with model selection -*- lexical-binding: t; -*-

;;; Commentary:
;; Enhanced configuration for gptel with interactive model selection
;; Shows available models and allows easy switching

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
        :key "sk-local-test-key-123"
        :stream t
        :models '("localhost-llama3"        ; Llama on your Mac
                  "localhost-qwen-coder"     ; Qwen Coder on your Mac  
                  "localhost-llama-vision"   ; Llama Vision on your Mac
                  "big72-default"            ; Default model on big72.local
                  "big72-llama3"             ; Specific Llama on big72
                  "big72-mistral"            ; Mistral on big72
                  "big72-qwen"               ; Qwen on big72
                  "big72-deepseek"           ; DeepSeek on big72
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
;; Model Listing and Selection
;; ============================================================================

(defun gptel-show-available-models ()
  "Show all available models from all backends."
  (interactive)
  (let ((buffer (get-buffer-create "*Available LLM Models*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "═══════════════════════════════════════════════════════\n")
      (insert "     Available LLM Models\n")
      (insert "═══════════════════════════════════════════════════════\n\n")
      
      ;; LiteLLM models
      (insert "📡 LiteLLM Gateway (via localhost:4000)\n")
      (insert "─────────────────────────────────────\n")
      (insert "Localhost models:\n")
      (insert "  • localhost-llama3      - Llama 3.2 on your Mac\n")
      (insert "  • localhost-qwen-coder  - Qwen 2.5 Coder on your Mac\n")
      (insert "  • localhost-llama-vision - Llama Vision on your Mac\n")
      (insert "\n")
      (insert "Big72 models:\n")
      (insert "  • big72-default         - Default model on big72.local\n")
      (insert "  • big72-llama3          - Llama 3.2 on big72\n")
      (insert "  • big72-mistral         - Mistral on big72\n")
      (insert "  • big72-qwen            - Qwen Coder on big72\n")
      (insert "  • big72-deepseek        - DeepSeek on big72\n")
      (insert "\n")
      (insert "Routing aliases:\n")
      (insert "  • coding-local          - Routes to best coding model\n")
      (insert "  • chat-local            - Routes to best chat model\n")
      (insert "\n\n")
      
      ;; Direct Ollama localhost
      (insert "💻 Direct Ollama (localhost:11434)\n")
      (insert "─────────────────────────────────────\n")
      (dolist (model (plist-get gptel-backend-ollama-local :models))
        (insert (format "  • %s\n" model)))
      (insert "\n")
      
      ;; Direct Ollama big72
      (insert "🖥️  Direct Ollama (big72.local:11434)\n")
      (insert "─────────────────────────────────────\n")
      (dolist (model (plist-get gptel-backend-ollama-big72 :models))
        (insert (format "  • %s\n" model)))
      (insert "\n\n")
      
      ;; Current selection
      (insert "═══════════════════════════════════════════════════════\n")
      (insert (format "Current Backend: %s\n" 
                      (if (and (boundp 'gptel-backend) gptel-backend
                               (fboundp 'gptel-backend-name))
                          (gptel-backend-name gptel-backend)
                        "None")))
      (insert (format "Current Model: %s\n" (or gptel-model "None")))
      (insert "═══════════════════════════════════════════════════════\n")
      
      (read-only-mode 1)
      (goto-char (point-min)))
    (display-buffer buffer)))

(defun gptel-select-model-interactive ()
  "Interactively select a backend and model."
  (interactive)
  (let* ((backend-choice 
          (completing-read "Select backend: "
                           '("LiteLLM Gateway" 
                             "Ollama Localhost (direct)" 
                             "Ollama Big72 (direct)")
                           nil t))
         (backend (cond
                   ((string-match "LiteLLM" backend-choice) gptel-backend-litellm)
                   ((string-match "Localhost" backend-choice) gptel-backend-ollama-local)
                   ((string-match "Big72" backend-choice) gptel-backend-ollama-big72)))
         (models (plist-get backend :models))
         (model (completing-read "Select model: " models nil t)))
    (setq gptel-backend backend)
    (setq gptel-model model)
    (message "Selected: %s / %s" 
             (gptel-backend-name backend) 
             model)))

;; ============================================================================
;; Quick Switch Functions for Big72
;; ============================================================================

(defun gptel-use-big72-default ()
  "Switch to big72 default model via LiteLLM."
  (interactive)
  (setq gptel-backend gptel-backend-litellm)
  (setq gptel-model "big72-default")
  (message "Using big72-default via LiteLLM"))

(defun gptel-use-big72-llama ()
  "Switch to big72 Llama model via LiteLLM."
  (interactive)
  (setq gptel-backend gptel-backend-litellm)
  (setq gptel-model "big72-llama3")
  (message "Using big72-llama3 via LiteLLM"))

(defun gptel-use-big72-mistral ()
  "Switch to big72 Mistral model via LiteLLM."
  (interactive)
  (setq gptel-backend gptel-backend-litellm)
  (setq gptel-model "big72-mistral")
  (message "Using big72-mistral via LiteLLM"))

(defun gptel-use-big72-qwen ()
  "Switch to big72 Qwen model via LiteLLM."
  (interactive)
  (setq gptel-backend gptel-backend-litellm)
  (setq gptel-model "big72-qwen")
  (message "Using big72-qwen via LiteLLM"))

(defun gptel-use-big72-deepseek ()
  "Switch to big72 DeepSeek model via LiteLLM."
  (interactive)
  (setq gptel-backend gptel-backend-litellm)
  (setq gptel-model "big72-deepseek")
  (message "Using big72-deepseek via LiteLLM"))

(defun gptel-use-big72-direct ()
  "Switch to big72 via direct Ollama and select model."
  (interactive)
  (setq gptel-backend gptel-backend-ollama-big72)
  (let ((models (plist-get gptel-backend-ollama-big72 :models)))
    (setq gptel-model (completing-read "Select big72 model: " models nil t)))
  (message "Using %s on big72.local (direct Ollama)" gptel-model))

;; ============================================================================
;; Quick Switch Functions for Localhost
;; ============================================================================

(defun gptel-use-localhost-llama ()
  "Switch to localhost Llama via LiteLLM."
  (interactive)
  (setq gptel-backend gptel-backend-litellm)
  (setq gptel-model "localhost-llama3")
  (message "Using localhost-llama3 via LiteLLM"))

(defun gptel-use-localhost-qwen ()
  "Switch to localhost Qwen via LiteLLM."
  (interactive)
  (setq gptel-backend gptel-backend-litellm)
  (setq gptel-model "localhost-qwen-coder")
  (message "Using localhost-qwen-coder via LiteLLM"))

;; ============================================================================
;; Status Display
;; ============================================================================

(defun gptel-show-current-config ()
  "Show current gptel configuration in minibuffer."
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
;; Hydra Menu (if hydra is installed)
;; ============================================================================

(with-eval-after-load 'hydra
  (defhydra gptel-model-hydra (:color blue :hint nil)
    "
LLM Model Selection
═══════════════════════════════════════════════════════
_l_: Show all models     _s_: Select interactively
_?_: Current config      _m_: gptel menu

Localhost Models:        Big72 Models:
_1_: localhost-llama3    _4_: big72-default
_2_: localhost-qwen      _5_: big72-llama3
_3_: localhost-vision    _6_: big72-mistral
                        _7_: big72-qwen
                        _8_: big72-deepseek

Direct Access:
_d_: Ollama localhost    _D_: Ollama big72

_q_: quit
"
    ("l" gptel-show-available-models)
    ("s" gptel-select-model-interactive)
    ("?" gptel-show-current-config)
    ("m" gptel-menu)
    
    ("1" gptel-use-localhost-llama)
    ("2" gptel-use-localhost-qwen)
    ("3" (progn (setq gptel-backend gptel-backend-litellm)
                (setq gptel-model "localhost-llama-vision")
                (message "Using localhost-llama-vision")))
    
    ("4" gptel-use-big72-default)
    ("5" gptel-use-big72-llama)
    ("6" gptel-use-big72-mistral)
    ("7" gptel-use-big72-qwen)
    ("8" gptel-use-big72-deepseek)
    
    ("d" (progn (setq gptel-backend gptel-backend-ollama-local)
                (message "Switched to direct Ollama localhost")))
    ("D" gptel-use-big72-direct)
    
    ("q" nil)))

;; ============================================================================
;; Key Bindings
;; ============================================================================

(defvar gptel-enhanced-map (make-sparse-keymap)
  "Keymap for enhanced gptel commands.")

;; Model selection
(define-key gptel-enhanced-map (kbd "l") #'gptel-show-available-models)
(define-key gptel-enhanced-map (kbd "s") #'gptel-select-model-interactive)
(define-key gptel-enhanced-map (kbd "?") #'gptel-show-current-config)

;; Quick switch to big72 models
(define-key gptel-enhanced-map (kbd "b d") #'gptel-use-big72-default)
(define-key gptel-enhanced-map (kbd "b l") #'gptel-use-big72-llama)
(define-key gptel-enhanced-map (kbd "b m") #'gptel-use-big72-mistral)
(define-key gptel-enhanced-map (kbd "b q") #'gptel-use-big72-qwen)
(define-key gptel-enhanced-map (kbd "b s") #'gptel-use-big72-deepseek)
(define-key gptel-enhanced-map (kbd "b b") #'gptel-use-big72-direct)

;; Quick switch to localhost models  
(define-key gptel-enhanced-map (kbd "1") #'gptel-use-localhost-llama)
(define-key gptel-enhanced-map (kbd "2") #'gptel-use-localhost-qwen)

;; gptel commands
(define-key gptel-enhanced-map (kbd "RET") #'gptel-send)
(define-key gptel-enhanced-map (kbd "r") #'gptel-send-region)
(define-key gptel-enhanced-map (kbd "m") #'gptel-menu)

;; Hydra menu (if available)
(when (fboundp 'gptel-model-hydra/body)
  (define-key gptel-enhanced-map (kbd "h") #'gptel-model-hydra/body))

;; Bind the enhanced map to C-c g
(global-set-key (kbd "C-c g") gptel-enhanced-map)

;; ============================================================================
;; Set Defaults
;; ============================================================================

;; Start with localhost via LiteLLM
(setq gptel-backend gptel-backend-litellm)
(setq gptel-model "localhost-llama3")
(setq gptel-stream t)

;; ============================================================================
;; Startup Message
;; ============================================================================

(message "Enhanced gptel loaded! Quick help:")
(message "  C-c g l    - List all available models")
(message "  C-c g s    - Select model interactively")
(message "  C-c g b d  - Use big72-default")
(message "  C-c g b l  - Use big72-llama3")
(message "  C-c g ?    - Show current config")
(message "  C-c g RET  - Send buffer to LLM")

(provide 'gptel-enhanced)
;;; gptel-enhanced.el ends here