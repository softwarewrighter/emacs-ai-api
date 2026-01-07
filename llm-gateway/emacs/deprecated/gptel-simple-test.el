;;; gptel-simple-test.el --- Simple test for gptel with local LLMs -*- lexical-binding: t; -*-

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
;; Minimal configuration to test gptel with local LLMs
;; First ensure gptel is installed:
;; M-x package-refresh-contents
;; M-x package-install RET gptel RET

;;; Code:

;; Check if gptel is available
(unless (require 'gptel nil t)
  (error "gptel is not installed. Install it with: M-x package-install RET gptel RET"))

;; Simple test with LiteLLM
(defun gptel-test-litellm ()
  "Test gptel with LiteLLM backend."
  (interactive)
  
  ;; Create backend
  (let ((backend (gptel-make-openai "TestLiteLLM"
                   :host "localhost:4000"
                   :endpoint "/v1/chat/completions"
                   :protocol "http"
                   :key "sk-local-test-key-123"
                   :stream t
                   :models '("localhost-llama3"))))
    
    ;; Set as current backend
    (setq gptel-backend backend)
    (setq gptel-model "localhost-llama3")
    
    ;; Create test buffer
    (let ((buf (get-buffer-create "*gptel-test*")))
      (switch-to-buffer buf)
      (erase-buffer)
      (insert "Test prompt: Say 'Hello from LiteLLM'\n\n")
      (message "Backend configured. Use M-x gptel-send to test."))))

;; Simple test with direct Ollama
(defun gptel-test-ollama ()
  "Test gptel with direct Ollama backend."
  (interactive)
  
  ;; Create backend
  (let ((backend (gptel-make-ollama "TestOllama"
                   :host "localhost:11434"
                   :stream t
                   :models '("llama3.2:latest"))))
    
    ;; Set as current backend
    (setq gptel-backend backend)
    (setq gptel-model "llama3.2:latest")
    
    ;; Create test buffer
    (let ((buf (get-buffer-create "*gptel-test*")))
      (switch-to-buffer buf)
      (erase-buffer)
      (insert "Test prompt: Say 'Hello from Ollama'\n\n")
      (message "Backend configured. Use M-x gptel-send to test."))))

;; Check current configuration
(defun gptel-show-config ()
  "Show current gptel configuration."
  (interactive)
  (if (boundp 'gptel-backend)
      (message "Backend: %s | Model: %s | Stream: %s"
               (if gptel-backend
                   (or (and (fboundp 'gptel-backend-name)
                            (gptel-backend-name gptel-backend))
                       (format "%S" gptel-backend))
                 "None")
               (or gptel-model "None")
               (if gptel-stream "enabled" "disabled"))
    (message "gptel not configured yet")))

;; Quick test commands
(global-set-key (kbd "C-c t l") #'gptel-test-litellm)   ; Test LiteLLM
(global-set-key (kbd "C-c t o") #'gptel-test-ollama)    ; Test Ollama
(global-set-key (kbd "C-c t s") #'gptel-send)           ; Send buffer
(global-set-key (kbd "C-c t ?") #'gptel-show-config)    ; Show config

(message "gptel simple test loaded!")
(message "Commands: C-c t l (LiteLLM), C-c t o (Ollama), C-c t s (send), C-c t ? (config)")

(provide 'gptel-simple-test)
;;; gptel-simple-test.el ends here