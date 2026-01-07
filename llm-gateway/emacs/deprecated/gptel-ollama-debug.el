;;; gptel-ollama-debug.el --- Debug version with better error handling -*- lexical-binding: t; -*-

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

;;; Code:

(require 'gptel)

;; Simple function to test fetching models
(defun gptel-test-fetch-models ()
  "Test fetching models from big72."
  (interactive)
  (let ((url-request-method "GET")
        (url-request-timeout 10)
        (url "http://big72.local:11434/api/tags"))
    (message "Fetching from %s..." url)
    (condition-case err
        (with-current-buffer (url-retrieve-synchronously url nil t 10)
          (goto-char (point-min))
          ;; Skip HTTP headers
          (when (re-search-forward "\r?\n\r?\n" nil t)
            (let* ((json-str (buffer-substring (point) (point-max)))
                   (json-data (json-read-from-string json-str))
                   (models (cdr (assoc 'models json-data))))
              (if models
                  (let ((model-names (mapcar (lambda (m) (cdr (assoc 'name m))) models)))
                    (message "Found %d models: %s" 
                             (length model-names)
                             (string-join (seq-take model-names 5) ", "))
                    model-names)
                (message "No models in response")
                nil))))
      (error 
       (message "Error fetching models: %s" (error-message-string err))
       nil))))

;; Fixed function with better error handling
(defun gptel-fetch-big72-models ()
  "Fetch models from big72 with proper error handling."
  (interactive)
  (let* ((url-request-method "GET")
         (url-request-timeout 10)
         (url "http://big72.local:11434/api/tags")
         (buffer (url-retrieve-synchronously url nil t 10)))
    (if (not buffer)
        (progn
          (message "Failed to connect to big72.local:11434")
          nil)
      (with-current-buffer buffer
        (goto-char (point-min))
        (if (not (re-search-forward "\r?\n\r?\n" nil t))
            (progn
              (message "Invalid response from big72")
              nil)
          (condition-case err
              (let* ((json-object-type 'alist)
                     (json-array-type 'list)
                     (json-data (json-read))
                     (models (cdr (assoc 'models json-data))))
                (prog1
                    (mapcar (lambda (m) (cdr (assoc 'name m))) models)
                  (kill-buffer buffer)))
            (error
             (message "JSON parse error: %s" err)
             (kill-buffer buffer)
             nil)))))))

;; Simple backend configuration
(defvar gptel-big72-models nil "Cached models from big72.")

(defun gptel-setup-big72 ()
  "Setup big72 with actual models."
  (interactive)
  (message "Fetching models from big72.local...")
  (setq gptel-big72-models (gptel-fetch-big72-models))
  (if gptel-big72-models
      (progn
        (message "Found %d models on big72: %s" 
                 (length gptel-big72-models)
                 (string-join (seq-take gptel-big72-models 3) ", "))
        ;; Create backend
        (setq gptel-backend
              (gptel-make-ollama "Big72"
                :host "big72.local:11434"
                :stream t
                :models gptel-big72-models))
        ;; Set first model as default
        (setq gptel-model (car gptel-big72-models))
        (message "Ready! Using model: %s" gptel-model))
    (message "Failed to fetch models from big72")))

(defun gptel-select-big72-model ()
  "Select a model from big72."
  (interactive)
  (if (not gptel-big72-models)
      (progn
        (message "Fetching models first...")
        (gptel-setup-big72)))
  (when gptel-big72-models
    (let ((model (completing-read "Select big72 model: " gptel-big72-models nil t)))
      (setq gptel-model model)
      (message "Selected: %s" model))))

(defun gptel-list-big72-models ()
  "List all models on big72."
  (interactive)
  (if (not gptel-big72-models)
      (gptel-setup-big72))
  (if gptel-big72-models
      (let ((buffer (get-buffer-create "*Big72 Models*")))
        (with-current-buffer buffer
          (erase-buffer)
          (insert "Models on big72.local:\n")
          (insert "═════════════════════\n\n")
          (dolist (model gptel-big72-models)
            (insert (format "• %s\n" model)))
          (insert (format "\n%d models total\n" (length gptel-big72-models)))
          (insert (format "\nCurrent: %s\n" (or gptel-model "none")))
          (goto-char (point-min))
          (read-only-mode 1))
        (display-buffer buffer))
    (message "No models found on big72")))

;; Simple keybindings
(global-set-key (kbd "C-c o t") #'gptel-test-fetch-models)    ; Test connection
(global-set-key (kbd "C-c o s") #'gptel-setup-big72)          ; Setup big72
(global-set-key (kbd "C-c o l") #'gptel-list-big72-models)    ; List models
(global-set-key (kbd "C-c o m") #'gptel-select-big72-model)   ; Select model
(global-set-key (kbd "C-c o g") #'gptel-send)                 ; Send buffer

(message "Debug config loaded! Commands:")
(message "  C-c o t - Test fetching models")
(message "  C-c o s - Setup big72 backend")
(message "  C-c o l - List big72 models")
(message "  C-c o m - Select model")
(message "  C-c o g - Send to model")

(provide 'gptel-ollama-debug)
;;; gptel-ollama-debug.el ends here