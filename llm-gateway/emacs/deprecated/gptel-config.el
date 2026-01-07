;;; gptel-config.el --- Multi-backend LLM configuration for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Michael A. Wright
;;
;; Author: Michael A. Wright
;; URL: https://github.com/softwarewrighter/emacs-ai-api
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is part of emacs-ai-api.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This configuration provides multiple LLM backends accessible through gptel
;; Supports: LiteLLM gateway, direct Ollama, direct llama.cpp, and cloud providers

;;; Code:

(require 'gptel)

;; ============================================================================
;; Configuration Variables
;; ============================================================================

(defgroup llm-config nil
  "Configuration for LLM integration."
  :group 'gptel)

(defcustom llm-litellm-base-url 
  (or (getenv "LITELLM_BASE_URL") "http://localhost:4000/v1")
  "Base URL for LiteLLM gateway."
  :type 'string
  :group 'llm-config)

(defcustom llm-litellm-key
  (or (getenv "LITELLM_VIRTUAL_KEY") 
      (getenv "LITELLM_MASTER_KEY")
      "sk-admin-changeme")
  "API key for LiteLLM gateway."
  :type 'string
  :group 'llm-config)

(defcustom llm-default-backend 'litellm
  "Default LLM backend to use."
  :type 'symbol
  :group 'llm-config)

(defcustom llm-default-model "coding-balanced"
  "Default model to use."
  :type 'string
  :group 'llm-config)

;; ============================================================================
;; Backend Definitions
;; ============================================================================

;; 1. LiteLLM Gateway (primary, routes to all providers)
(gptel-make-openai "LiteLLM"
  :host "localhost:4000"
  :endpoint "/v1/chat/completions"
  :protocol "http"
  :key llm-litellm-key
  :stream t
  :models '(;; Routing aliases
            "coding-best"      ; High quality (GPT-4o, Claude Sonnet)
            "coding-balanced"  ; Good quality, moderate cost
            "coding-cheap"     ; Local first, then cheap cloud
            "coding-auto"      ; Auto-routing based on availability
            "reasoning"        ; o1 models for complex reasoning
            
            ;; Direct cloud models
            "gpt-4o" "gpt-4o-mini" "gpt-4-turbo"
            "o1-preview" "o1-mini"
            "claude-3-5-sonnet" "claude-3-5-haiku" "claude-3-opus"
            "deepseek-chat" "deepseek-coder"
            "gemini-1.5-pro" "gemini-1.5-flash" "gemini-2.0-flash-exp"
            
            ;; Local models via gateway
            "local-deepseek" "local-kimi"
            "ollama-deepseek" "ollama-qwen" "ollama-llama"))

;; 2. Direct Ollama access (fallback if gateway is down)
(when (executable-find "ollama")
  (gptel-make-ollama "Ollama-Direct"
    :host "localhost:11434"
    :stream t
    :models '("deepseek-coder:latest"
              "qwen2.5-coder:7b"
              "qwen2.5-coder:14b"
              "llama3.2:latest"
              "mistral:latest"
              "codellama:latest")))

;; 3. Direct llama.cpp access (for custom models)
(gptel-make-openai "LlamaCpp-Direct"
  :host "localhost:8080"
  :endpoint "/v1/chat/completions"
  :protocol "http"
  :key "sk-local"
  :stream t
  :models '("model"))  ; llama.cpp uses a single model at a time

;; 4. Direct OpenAI (if you want to bypass LiteLLM)
(when-let ((key (getenv "OPENAI_API_KEY")))
  (gptel-make-openai "OpenAI-Direct"
    :key key
    :stream t
    :models '("gpt-4o" "gpt-4o-mini" "gpt-4-turbo" "o1-preview" "o1-mini")))

;; 5. Direct Anthropic (if you want to bypass LiteLLM)
(when-let ((key (getenv "ANTHROPIC_API_KEY")))
  (gptel-make-anthropic "Anthropic-Direct"
    :key key
    :stream t
    :models '("claude-3-5-sonnet-20241022" 
              "claude-3-5-haiku-20241022" 
              "claude-3-opus-20240229")))

;; ============================================================================
;; Helper Functions
;; ============================================================================

(defun llm-switch-backend ()
  "Interactively switch between LLM backends."
  (interactive)
  (let* ((backends (mapcar #'car gptel--known-backends))
         (choice (completing-read "Select backend: " backends nil t)))
    (setq gptel-backend (gptel-backend choice))
    (message "Switched to backend: %s" choice)))

(defun llm-switch-model ()
  "Interactively switch model for current backend."
  (interactive)
  (let* ((models (gptel-backend-models gptel-backend))
         (choice (completing-read "Select model: " models nil t)))
    (setq gptel-model choice)
    (message "Switched to model: %s" choice)))

(defun llm-quick-switch (backend-name model-name)
  "Quickly switch to a specific BACKEND-NAME and MODEL-NAME."
  (setq gptel-backend (gptel-backend backend-name))
  (setq gptel-model model-name)
  (message "Switched to %s/%s" backend-name model-name))

(defun llm-use-best ()
  "Switch to best quality models (GPT-4o/Claude)."
  (interactive)
  (llm-quick-switch "LiteLLM" "coding-best"))

(defun llm-use-balanced ()
  "Switch to balanced quality/cost models."
  (interactive)
  (llm-quick-switch "LiteLLM" "coding-balanced"))

(defun llm-use-cheap ()
  "Switch to cheapest models (local first)."
  (interactive)
  (llm-quick-switch "LiteLLM" "coding-cheap"))

(defun llm-use-local ()
  "Switch to local Ollama models."
  (interactive)
  (if (gptel-backend "Ollama-Direct")
      (llm-quick-switch "Ollama-Direct" "deepseek-coder:latest")
    (message "Ollama backend not available")))

(defun llm-show-status ()
  "Show current LLM backend and model status."
  (interactive)
  (message "Backend: %s | Model: %s | Stream: %s"
           (gptel-backend-name gptel-backend)
           gptel-model
           (if gptel-stream "enabled" "disabled")))

(defun llm-test-current ()
  "Test current backend and model with a simple prompt."
  (interactive)
  (let ((buffer (get-buffer-create "*LLM Test*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Testing: " (gptel-backend-name gptel-backend) "/" gptel-model "\n")
      (insert "Prompt: Write 'Hello from [your name]'\n\n")
      (goto-char (point-max)))
    (switch-to-buffer buffer)
    (gptel-send)))

;; ============================================================================
;; Project-specific Configuration
;; ============================================================================

(defun llm-set-project-key (project-name key)
  "Set a virtual key for PROJECT-NAME."
  (let ((keys-file (expand-file-name 
                    (format "keys/%s.env" project-name)
                    (file-name-directory (or load-file-name buffer-file-name)))))
    (when (file-exists-p keys-file)
      (with-temp-buffer
        (insert-file-contents keys-file)
        (when (re-search-forward "LITELLM_VIRTUAL_KEY=\\(.+\\)" nil t)
          (setq llm-litellm-key (match-string 1))
          (message "Loaded key for project: %s" project-name))))))

;; ============================================================================
;; Key Bindings
;; ============================================================================

(defvar llm-map (make-sparse-keymap)
  "Keymap for LLM commands.")

(define-key llm-map (kbd "b") #'llm-switch-backend)
(define-key llm-map (kbd "m") #'llm-switch-model)
(define-key llm-map (kbd "1") #'llm-use-best)
(define-key llm-map (kbd "2") #'llm-use-balanced)
(define-key llm-map (kbd "3") #'llm-use-cheap)
(define-key llm-map (kbd "l") #'llm-use-local)
(define-key llm-map (kbd "s") #'llm-show-status)
(define-key llm-map (kbd "t") #'llm-test-current)
(define-key llm-map (kbd "c") #'gptel-send)
(define-key llm-map (kbd "C") #'gptel-menu)

;; Bind to C-c l (you can change this)
(global-set-key (kbd "C-c l") llm-map)

;; ============================================================================
;; Initialize
;; ============================================================================

(defun llm-initialize ()
  "Initialize LLM configuration with defaults."
  (setq gptel-backend (gptel-backend (symbol-name llm-default-backend)))
  (setq gptel-model llm-default-model)
  (setq gptel-stream t)
  (message "LLM initialized: %s/%s" 
           (gptel-backend-name gptel-backend) 
           gptel-model))

;; Initialize on load
(llm-initialize)

;; ============================================================================
;; Provide
;; ============================================================================

(provide 'gptel-config)

;;; gptel-config.el ends here