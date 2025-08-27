;;; gptel-working.el --- Working config with actual model names -*- lexical-binding: t; -*-

(require 'gptel)

;; Known models on big72 (based on curl output)
;; mistral:latest, qwen2.5:7b, llama3.2:1b, etc.

;; Quick setup for big72 with qwen
(defun use-big72-qwen ()
  "Use Qwen 2.5 7B on big72."
  (interactive)
  (setq gptel-backend
        (gptel-make-ollama "Big72"
          :host "big72.local:11434"
          :stream t
          :models '("qwen2.5:7b")))
  (setq gptel-model "qwen2.5:7b")
  (message "Selected: qwen2.5:7b on big72"))

;; Use mistral on big72
(defun use-big72-mistral ()
  "Use Mistral on big72."
  (interactive)
  (setq gptel-backend
        (gptel-make-ollama "Big72"
          :host "big72.local:11434"
          :stream t
          :models '("mistral:latest")))
  (setq gptel-model "mistral:latest")
  (message "Selected: mistral:latest on big72"))

;; Use localhost qwen-coder
(defun use-localhost-qwen-coder ()
  "Use Qwen Coder on localhost."
  (interactive)
  (setq gptel-backend
        (gptel-make-ollama "Localhost"
          :host "localhost:11434"
          :stream t
          :models '("qwen2.5-coder:14b")))
  (setq gptel-model "qwen2.5-coder:14b")
  (message "Selected: qwen2.5-coder:14b on localhost"))

;; Send to current model
(defun send-to-llm ()
  "Send buffer to current LLM."
  (interactive)
  (if (not gptel-backend)
      (message "No model selected! Use M-1, M-2, or M-3")
    (gptel-send)))

;; Simple keybindings
(global-set-key (kbd "M-1") #'use-big72-qwen)
(global-set-key (kbd "M-2") #'use-big72-mistral)
(global-set-key (kbd "M-3") #'use-localhost-qwen-coder)
(global-set-key (kbd "M-0") #'send-to-llm)

(message "Quick setup loaded!")
(message "  M-1 - Use qwen2.5:7b on big72")
(message "  M-2 - Use mistral:latest on big72")
(message "  M-3 - Use qwen2.5-coder:14b on localhost")
(message "  M-0 - Send buffer to selected model")

(provide 'gptel-working)
;;; gptel-working.el ends here