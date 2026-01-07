;;; gptel-ollama-selector.el --- Select Ollama endpoints and models dynamically -*- lexical-binding: t; -*-

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

;;; Commentary:
;; Configuration for selecting Ollama endpoints and models
;; Queries actual Ollama instances for available models

;;; Code:

(require 'gptel)
(require 'url)
(require 'json)

;; ============================================================================
;; Configuration
;; ============================================================================

(defvar gptel-ollama-endpoints
  '(("localhost" . "http://localhost:11434")
    ("big72" . "http://big72.local:11434")
    ("litellm" . "http://localhost:4000"))
  "Available Ollama endpoints.")

(defvar gptel-current-endpoint nil
  "Currently selected endpoint.")

(defvar gptel-current-endpoint-name nil
  "Name of currently selected endpoint.")

(defvar gptel-available-models nil
  "Models available at current endpoint.")

;; ============================================================================
;; Model Discovery Functions
;; ============================================================================

(defun gptel-fetch-ollama-models (endpoint)
  "Fetch available models from ENDPOINT."
  (let ((url (concat endpoint "/api/tags"))
        (url-request-method "GET")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        models)
    (condition-case err
        (with-current-buffer (url-retrieve-synchronously url nil t 5)
          (goto-char (point-min))
          (re-search-forward "\n\n")
          (let ((json-data (json-read)))
            (mapcar (lambda (model)
                      (cdr (assoc 'name model)))
                    (cdr (assoc 'models json-data)))))
      (error
       (message "Failed to fetch models from %s: %s" endpoint err)
       nil))))

(defun gptel-fetch-litellm-models ()
  "Fetch available models from LiteLLM."
  (let ((url "http://localhost:4000/v1/models")
        (url-request-method "GET")
        (url-request-extra-headers 
         '(("Content-Type" . "application/json")
           ("Authorization" . "Bearer sk-local-test-key-123"))))
    (condition-case err
        (with-current-buffer (url-retrieve-synchronously url nil t 5)
          (goto-char (point-min))
          (re-search-forward "\n\n")
          (let ((json-data (json-read)))
            (mapcar (lambda (model)
                      (cdr (assoc 'id model)))
                    (cdr (assoc 'data json-data)))))
      (error
       (message "Failed to fetch LiteLLM models: %s" err)
       nil))))

;; ============================================================================
;; Selection Functions
;; ============================================================================

(defun gptel-list-endpoints ()
  "List all available endpoints."
  (interactive)
  (let ((buffer (get-buffer-create "*Ollama Endpoints*")))
    (with-current-buffer buffer
      (read-only-mode -1)  ; Disable read-only first
      (erase-buffer)
      (insert "Available Endpoints\n")
      (insert "═══════════════════\n\n")
      (dolist (endpoint gptel-ollama-endpoints)
        (insert (format "• %s: %s\n" (car endpoint) (cdr endpoint))))
      (insert "\n")
      (insert (format "Current: %s\n" (or gptel-current-endpoint-name "none")))
      (insert "\nUse C-c g S <key> to select:\n")
      (insert "  C-c g S l - localhost\n")
      (insert "  C-c g S b - big72\n")
      (insert "  C-c g S L - LiteLLM\n")
      (read-only-mode 1)
      (goto-char (point-min)))
    (display-buffer buffer)))

(defun gptel-select-localhost ()
  "Select localhost Ollama endpoint."
  (interactive)
  (gptel-select-endpoint "localhost"))

(defun gptel-select-big72 ()
  "Select big72 Ollama endpoint."
  (interactive)
  (gptel-select-endpoint "big72"))

(defun gptel-select-litellm ()
  "Select LiteLLM endpoint."
  (interactive)
  (gptel-select-endpoint "litellm"))

(defun gptel-select-endpoint (name)
  "Select endpoint by NAME."
  (let ((endpoint (assoc name gptel-ollama-endpoints)))
    (if endpoint
        (progn
          (setq gptel-current-endpoint (cdr endpoint))
          (setq gptel-current-endpoint-name name)
          ;; Fetch models for this endpoint
          (message "Fetching models from %s..." name)
          (setq gptel-available-models
                (if (string= name "litellm")
                    (gptel-fetch-litellm-models)
                  (gptel-fetch-ollama-models gptel-current-endpoint)))
          (message "Selected %s (%d models available)" 
                   name (length gptel-available-models)))
      (message "Unknown endpoint: %s" name))))

(defun gptel-list-models ()
  "List models available at current endpoint."
  (interactive)
  (if (not gptel-current-endpoint)
      (message "No endpoint selected. Use C-c g L to list endpoints.")
    (if (not gptel-available-models)
        (message "No models found at %s" gptel-current-endpoint-name)
      (let ((buffer (get-buffer-create "*Available Models*")))
        (with-current-buffer buffer
          (read-only-mode -1)  ; Disable read-only first
          (erase-buffer)
          (insert (format "Models at %s\n" gptel-current-endpoint-name))
          (insert "══════════════════════════\n\n")
          (dolist (model gptel-available-models)
            (insert (format "• %s\n" model)))
          (insert "\n")
          (insert (format "Current model: %s\n" (or gptel-model "none")))
          (insert "\nUse C-c g s <key> to select:\n")
          ;; Show shortcuts for common models
          (when (member "qwen2.5-coder:14b" gptel-available-models)
            (insert "  C-c g s q - qwen2.5-coder:14b\n"))
          (when (member "llama3.2:latest" gptel-available-models)
            (insert "  C-c g s l - llama3.2:latest\n"))
          (when (member "mistral:latest" gptel-available-models)
            (insert "  C-c g s m - mistral:latest\n"))
          (when (member "deepseek-r1:14b" gptel-available-models)
            (insert "  C-c g s d - deepseek-r1:14b\n"))
          (insert "  C-c g s s - select from list\n")
          (read-only-mode 1)  ; Re-enable read-only
          (goto-char (point-min)))
        (display-buffer buffer)))))

(defun gptel-select-qwen ()
  "Select Qwen model."
  (interactive)
  (gptel-select-model-by-name "qwen2.5-coder:14b"))

(defun gptel-select-llama ()
  "Select Llama model."
  (interactive)
  (gptel-select-model-by-name "llama3.2:latest"))

(defun gptel-select-mistral ()
  "Select Mistral model."
  (interactive)
  (gptel-select-model-by-name "mistral:latest"))

(defun gptel-select-deepseek ()
  "Select DeepSeek model."
  (interactive)
  (gptel-select-model-by-name "deepseek-r1:14b"))

(defun gptel-select-model-interactive ()
  "Select a model from available models."
  (interactive)
  (if (not gptel-available-models)
      (message "No models available. Select an endpoint first with C-c g S")
    (let ((model (completing-read "Select model: " gptel-available-models nil t)))
      (gptel-select-model-by-name model))))

(defun gptel-select-model-by-name (model-name)
  "Select MODEL-NAME and configure gptel backend."
  (if (not (member model-name gptel-available-models))
      (message "Model %s not available at %s" model-name gptel-current-endpoint-name)
    (cond
     ;; LiteLLM backend
     ((string= gptel-current-endpoint-name "litellm")
      (setq gptel-backend
            (gptel-make-openai "LiteLLM"
              :host "localhost:4000"
              :endpoint "/v1/chat/completions"
              :protocol "http"
              :key "sk-local-test-key-123"
              :stream t
              :models (list model-name)))
      (setq gptel-model model-name))
     
     ;; Ollama backends
     (t
      (setq gptel-backend
            (gptel-make-ollama gptel-current-endpoint-name
              :host (if (string= gptel-current-endpoint-name "localhost")
                        "localhost:11434"
                      "big72.local:11434")
              :stream t
              :models (list model-name)))
      (setq gptel-model model-name)))
    
    (message "Selected: %s on %s" model-name gptel-current-endpoint-name)))

(defun gptel-show-status ()
  "Show current endpoint and model selection."
  (interactive)
  (message "Endpoint: %s | Model: %s | Backend: %s"
           (or gptel-current-endpoint-name "none")
           (or gptel-model "none")
           (if gptel-backend
               (gptel-backend-name gptel-backend)
             "none")))

;; ============================================================================
;; Key Bindings
;; ============================================================================

(defvar gptel-selector-map (make-sparse-keymap)
  "Keymap for gptel selector commands.")

;; Main commands
(define-key gptel-selector-map (kbd "L") #'gptel-list-endpoints)
(define-key gptel-selector-map (kbd "l") #'gptel-list-models)
(define-key gptel-selector-map (kbd "?") #'gptel-show-status)

;; Endpoint selection (C-c g S <key>)
(define-key gptel-selector-map (kbd "S l") #'gptel-select-localhost)
(define-key gptel-selector-map (kbd "S b") #'gptel-select-big72)
(define-key gptel-selector-map (kbd "S L") #'gptel-select-litellm)

;; Model selection (C-c g s <key>)
(define-key gptel-selector-map (kbd "s q") #'gptel-select-qwen)
(define-key gptel-selector-map (kbd "s l") #'gptel-select-llama)
(define-key gptel-selector-map (kbd "s m") #'gptel-select-mistral)
(define-key gptel-selector-map (kbd "s d") #'gptel-select-deepseek)
(define-key gptel-selector-map (kbd "s s") #'gptel-select-model-interactive)

;; Sending
(define-key gptel-selector-map (kbd "RET") #'gptel-send)
(define-key gptel-selector-map (kbd "r") #'gptel-send-region)
(define-key gptel-selector-map (kbd "m") #'gptel-menu)

;; Bind to C-c g
(global-set-key (kbd "C-c g") gptel-selector-map)

;; ============================================================================
;; Initialization
;; ============================================================================

(message "gptel-ollama-selector loaded!")
(message "Commands:")
(message "  C-c g L    - List endpoints")
(message "  C-c g S b  - Select big72")
(message "  C-c g l    - List models on current endpoint")
(message "  C-c g s q  - Select qwen model")
(message "  C-c g RET  - Send buffer to model")

(provide 'gptel-ollama-selector)
;;; gptel-ollama-selector.el ends here