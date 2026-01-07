;;; gptel-quick.el --- Quick working config for gptel -*- lexical-binding: t; -*-

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
;; Simplified configuration that works immediately

;;; Code:

(require 'gptel)

;; Create backends
(defvar my-litellm-backend
  (gptel-make-openai "LiteLLM"
    :host "localhost:4000"
    :endpoint "/v1/chat/completions"
    :protocol "http"
    :key "sk-local-test-key-123"
    :stream t
    :models '("localhost-llama3"
              "localhost-qwen-coder"
              "localhost-llama-vision"
              "big72-default"
              "big72-llama3"
              "big72-mistral"
              "big72-qwen"
              "big72-deepseek"
              "coding-local"
              "chat-local")))

(defvar my-ollama-big72
  (gptel-make-ollama "Big72-Direct"
    :host "big72.local:11434"
    :stream t
    :models '("llama3.2:latest"
              "mistral:latest"
              "qwen2.5-coder:14b"
              "deepseek-r1:14b")))

;; Simple selection functions that work
(defun select-big72-qwen ()
  "Select big72-qwen model."
  (interactive)
  (setq gptel-backend my-litellm-backend)
  (setq gptel-model "big72-qwen")
  (message "✓ Selected: big72-qwen via LiteLLM"))

(defun select-big72-llama ()
  "Select big72-llama3 model."
  (interactive)
  (setq gptel-backend my-litellm-backend)
  (setq gptel-model "big72-llama3")
  (message "✓ Selected: big72-llama3 via LiteLLM"))

(defun select-big72-mistral ()
  "Select big72-mistral model."
  (interactive)
  (setq gptel-backend my-litellm-backend)
  (setq gptel-model "big72-mistral")
  (message "✓ Selected: big72-mistral via LiteLLM"))

(defun select-big72-deepseek ()
  "Select big72-deepseek model."
  (interactive)
  (setq gptel-backend my-litellm-backend)
  (setq gptel-model "big72-deepseek")
  (message "✓ Selected: big72-deepseek via LiteLLM"))

(defun select-localhost-llama ()
  "Select localhost-llama3 model."
  (interactive)
  (setq gptel-backend my-litellm-backend)
  (setq gptel-model "localhost-llama3")
  (message "✓ Selected: localhost-llama3 via LiteLLM"))

(defun select-localhost-qwen ()
  "Select localhost-qwen-coder model."
  (interactive)
  (setq gptel-backend my-litellm-backend)
  (setq gptel-model "localhost-qwen-coder")
  (message "✓ Selected: localhost-qwen-coder via LiteLLM"))

(defun quick-select-model ()
  "Quickly select a model from a list."
  (interactive)
  (let* ((choices '(("big72-qwen" . select-big72-qwen)
                    ("big72-llama3" . select-big72-llama)
                    ("big72-mistral" . select-big72-mistral)
                    ("big72-deepseek" . select-big72-deepseek)
                    ("localhost-llama3" . select-localhost-llama)
                    ("localhost-qwen-coder" . select-localhost-qwen)))
         (choice (completing-read "Select model: " (mapcar #'car choices) nil t)))
    (funcall (cdr (assoc choice choices)))))

(defun show-current-model ()
  "Show current model selection."
  (interactive)
  (message "Current: %s / %s" 
           (if gptel-backend 
               (gptel-backend-name gptel-backend)
             "No backend")
           (or gptel-model "No model")))

;; Set default
(setq gptel-backend my-litellm-backend)
(setq gptel-model "localhost-llama3")
(setq gptel-stream t)

;; Simple keybindings that work
(global-set-key (kbd "M-g M-s") #'quick-select-model)     ; Quick model select
(global-set-key (kbd "M-g M-?") #'show-current-model)     ; Show current
(global-set-key (kbd "M-g M-g") #'gptel-send)             ; Send buffer
(global-set-key (kbd "M-g M-r") #'gptel-send-region)      ; Send region
(global-set-key (kbd "M-g M-m") #'gptel-menu)             ; gptel menu

;; Alternative: Function keys
(global-set-key (kbd "<f5>") #'quick-select-model)
(global-set-key (kbd "<f6>") #'gptel-send)
(global-set-key (kbd "<f7>") #'show-current-model)

(message "gptel-quick loaded!")
(message "Commands: M-g M-s (select model), M-g M-g (send), M-g M-? (show current)")
(message "Or use: F5 (select), F6 (send), F7 (show current)")

(provide 'gptel-quick)
;;; gptel-quick.el ends here