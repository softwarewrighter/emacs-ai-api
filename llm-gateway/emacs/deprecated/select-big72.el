;;; select-big72.el --- Direct commands to select big72 models -*- lexical-binding: t; -*-

;; Load this file and run one of these commands to select a model:
;; M-x select-big72-qwen
;; M-x select-big72-llama  
;; M-x select-big72-mistral
;; M-x select-big72-deepseek

(require 'gptel)

;; Create the backend
(setq my-litellm
      (gptel-make-openai "LiteLLM"
        :host "localhost:4000"
        :endpoint "/v1/chat/completions"
        :protocol "http"
        :key "sk-local-test-key-123"
        :stream t
        :models '("big72-qwen" "big72-llama3" "big72-mistral" "big72-deepseek")))

(defun select-big72-qwen ()
  "Use big72 Qwen Coder model."
  (interactive)
  (setq gptel-backend my-litellm)
  (setq gptel-model "big72-qwen")
  (message "Selected: big72-qwen - Ready to use!"))

(defun select-big72-llama ()
  "Use big72 Llama model."
  (interactive)
  (setq gptel-backend my-litellm)
  (setq gptel-model "big72-llama3")
  (message "Selected: big72-llama3 - Ready to use!"))

(defun select-big72-mistral ()
  "Use big72 Mistral model."
  (interactive)
  (setq gptel-backend my-litellm)
  (setq gptel-model "big72-mistral")
  (message "Selected: big72-mistral - Ready to use!"))

(defun select-big72-deepseek ()
  "Use big72 DeepSeek model."
  (interactive)
  (setq gptel-backend my-litellm)
  (setq gptel-model "big72-deepseek")
  (message "Selected: big72-deepseek - Ready to use!"))

;; Set default to big72-qwen
(select-big72-qwen)

(message "Use M-x select-big72-qwen (or -llama, -mistral, -deepseek)")
(message "Then use M-x gptel-send to send buffer content")

(provide 'select-big72)
;;; select-big72.el ends here