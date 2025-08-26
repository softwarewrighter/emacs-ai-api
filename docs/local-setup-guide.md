# Local LLM Setup & Testing Guide

This guide covers setting up and testing local LLMs (Ollama on localhost and LAN) before moving to cloud providers.

## Table of Contents
1. [Local Ollama Setup](#local-ollama-setup)
2. [LAN Ollama Configuration](#lan-ollama-configuration)
3. [Testing Local Models](#testing-local-models)
4. [Emacs Configuration for Local Models](#emacs-configuration-for-local-models)
5. [Monitoring from Emacs](#monitoring-from-emacs)

## Local Ollama Setup

### 1. Install and Start Ollama on Localhost

```bash
# macOS
brew install ollama
ollama serve

# Linux
curl -fsSL https://ollama.com/install.sh | sh
ollama serve

# The server runs on http://localhost:11434
```

### 2. Pull Models

```bash
# Essential coding models
ollama pull deepseek-coder:latest
ollama pull qwen2.5-coder:7b
ollama pull codellama:13b

# General purpose
ollama pull llama3.2:latest
ollama pull mistral:latest

# List installed models
ollama list
```

### 3. Test Direct Access

```bash
# Test Ollama is running
curl http://localhost:11434/api/tags

# Test a model directly
curl http://localhost:11434/api/generate -d '{
  "model": "deepseek-coder:latest",
  "prompt": "Write a Python hello world"
}'
```

## LAN Ollama Configuration

### 1. Configure Ollama for LAN Access

On the machine running Ollama (e.g., 192.168.1.100):

```bash
# Linux/macOS: Set environment variable
export OLLAMA_HOST=0.0.0.0:11434
ollama serve

# Or create a systemd service (Linux)
sudo tee /etc/systemd/system/ollama.service > /dev/null <<EOF
[Unit]
Description=Ollama
After=network-online.target

[Service]
Type=simple
User=$USER
Environment="OLLAMA_HOST=0.0.0.0:11434"
ExecStart=/usr/local/bin/ollama serve
Restart=on-failure
RestartSec=5

[Install]
WantedBy=multi-user.target
EOF

sudo systemctl daemon-reload
sudo systemctl enable ollama
sudo systemctl start ollama
```

### 2. Firewall Configuration

```bash
# Allow port 11434 (Linux with ufw)
sudo ufw allow 11434

# macOS
# System Preferences → Security & Privacy → Firewall → Firewall Options
# Add ollama or allow incoming connections

# Test from another machine
curl http://192.168.1.100:11434/api/tags
```

## Testing Local Models

### 1. Update LiteLLM Configuration

Edit `llm-gateway/litellm/config.yaml` to add your LAN Ollama:

```yaml
model_list:
  # Existing localhost Ollama
  - model_name: local-ollama-deepseek
    litellm_params:
      model: ollama/deepseek-coder:latest
      api_base: http://localhost:11434
      api_key: sk-local
      input_cost_per_token: 0.0
      output_cost_per_token: 0.0

  # LAN Ollama (replace with your IP)
  - model_name: lan-ollama-deepseek
    litellm_params:
      model: ollama/deepseek-coder:latest
      api_base: http://192.168.1.100:11434
      api_key: sk-local
      input_cost_per_token: 0.0
      output_cost_per_token: 0.0
  
  - model_name: lan-ollama-qwen
    litellm_params:
      model: ollama/qwen2.5-coder:7b
      api_base: http://192.168.1.100:11434
      api_key: sk-local
      input_cost_per_token: 0.0
      output_cost_per_token: 0.0

  # Local-only routing alias
  - model_name: coding-local-only
    litellm_params:
      model: ollama/deepseek-coder:latest
      api_base: http://localhost:11434
      api_key: sk-local
  - model_name: coding-local-only
    litellm_params:
      model: ollama/deepseek-coder:latest
      api_base: http://192.168.1.100:11434
      api_key: sk-local
```

### 2. Test Without Cloud Keys

```bash
# Start LiteLLM without any cloud keys
cd llm-gateway

# Create a local-only environment
cat > .env.local <<EOF
POSTGRES_PASSWORD=localtest123
LITELLM_MASTER_KEY=sk-local-test-key
# No cloud keys needed!
EOF

# Start with local config
docker compose --env-file .env.local up -d

# Test local models
cd ../scripts

# Test localhost Ollama through LiteLLM
MODEL=local-ollama-deepseek ./probe.sh

# Test LAN Ollama through LiteLLM
MODEL=lan-ollama-deepseek ./probe.sh

# Test routing alias (will use fastest available)
MODEL=coding-local-only ./probe.sh
```

### 3. Direct Testing Scripts

Create `scripts/test-local-only.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

echo "Testing Local Ollama Models"
echo "==========================="

# Test localhost
echo "Testing localhost Ollama..."
if curl -s http://localhost:11434/api/tags >/dev/null 2>&1; then
    echo "✓ Localhost Ollama is running"
    MODEL=local-ollama-deepseek ./probe.sh
else
    echo "✗ Localhost Ollama not found"
fi

# Test LAN (update IP as needed)
LAN_IP="192.168.1.100"
echo "Testing LAN Ollama at $LAN_IP..."
if curl -s http://$LAN_IP:11434/api/tags >/dev/null 2>&1; then
    echo "✓ LAN Ollama is running"
    MODEL=lan-ollama-deepseek ./probe.sh
else
    echo "✗ LAN Ollama not found at $LAN_IP"
fi
```

## Emacs Configuration for Local Models

### 1. Local-Only Emacs Config

Create `llm-gateway/emacs/gptel-local-only.el`:

```elisp
;;; gptel-local-only.el --- Local LLM configuration for testing

(require 'gptel)

;; Direct Ollama backends (no LiteLLM needed)
(gptel-make-ollama "Ollama-Localhost"
  :host "localhost:11434"
  :stream t
  :models '("deepseek-coder:latest" 
            "qwen2.5-coder:7b"
            "codellama:13b"
            "llama3.2:latest"))

(gptel-make-ollama "Ollama-LAN"
  :host "192.168.1.100:11434"  ; Update with your LAN IP
  :stream t
  :models '("deepseek-coder:latest" 
            "qwen2.5-coder:7b"
            "codellama:13b"))

;; Through LiteLLM gateway (if running)
(when (ignore-errors 
        (url-retrieve-synchronously "http://localhost:4000/health/readiness" nil t 1))
  (gptel-make-openai "LiteLLM-Local"
    :host "localhost:4000"
    :endpoint "/v1/chat/completions"
    :protocol "http"
    :key "sk-local-test-key"  ; Your LITELLM_MASTER_KEY
    :stream t
    :models '("local-ollama-deepseek"
              "lan-ollama-deepseek"
              "lan-ollama-qwen"
              "coding-local-only")))

;; Quick switching functions
(defun use-local-ollama ()
  "Switch to localhost Ollama."
  (interactive)
  (setq gptel-backend (gptel-backend "Ollama-Localhost"))
  (setq gptel-model "deepseek-coder:latest")
  (message "Using localhost Ollama"))

(defun use-lan-ollama ()
  "Switch to LAN Ollama."
  (interactive)
  (setq gptel-backend (gptel-backend "Ollama-LAN"))
  (setq gptel-model "deepseek-coder:latest")
  (message "Using LAN Ollama"))

;; Set default to local
(setq gptel-backend (gptel-backend "Ollama-Localhost"))
(setq gptel-model "deepseek-coder:latest")

(provide 'gptel-local-only)
```

### 2. Test from Emacs

```elisp
;; Load local config
(load-file "~/path/to/llm-gateway/emacs/gptel-local-only.el")

;; Test localhost
M-x use-local-ollama
M-x gptel-send

;; Test LAN
M-x use-lan-ollama
M-x gptel-send

;; Check which backend is active
(message "Backend: %s, Model: %s" 
         (gptel-backend-name gptel-backend) 
         gptel-model)
```

## Monitoring from Emacs

### 1. Create Monitoring Functions

Add to your Emacs config:

```elisp
;;; llm-monitor.el --- Monitor LLM usage from Emacs

(defun llm-check-status ()
  "Check status of all LLM providers."
  (interactive)
  (let ((buffer (get-buffer-create "*LLM Status*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "LLM Provider Status\n")
      (insert "===================\n\n")
      
      ;; Check LiteLLM
      (insert "LiteLLM Gateway: ")
      (if (llm-service-running-p "http://localhost:4000/health/readiness")
          (insert (propertize "● Online\n" 'face '(:foreground "green")))
        (insert (propertize "● Offline\n" 'face '(:foreground "red"))))
      
      ;; Check local Ollama
      (insert "Ollama (localhost): ")
      (if (llm-service-running-p "http://localhost:11434/api/tags")
          (insert (propertize "● Online\n" 'face '(:foreground "green")))
        (insert (propertize "● Offline\n" 'face '(:foreground "red"))))
      
      ;; Check LAN Ollama
      (insert "Ollama (LAN): ")
      (if (llm-service-running-p "http://192.168.1.100:11434/api/tags")
          (insert (propertize "● Online\n" 'face '(:foreground "green")))
        (insert (propertize "● Offline\n" 'face '(:foreground "red"))))
      
      (goto-char (point-min)))
    (display-buffer buffer)))

(defun llm-service-running-p (url)
  "Check if service at URL is running."
  (condition-case nil
      (url-retrieve-synchronously url nil t 1)
    (error nil)))

(defun llm-show-models ()
  "Show available models from LiteLLM."
  (interactive)
  (let* ((url "http://localhost:4000/v1/models")
         (response (with-current-buffer 
                      (url-retrieve-synchronously url nil t 5)
                    (goto-char (point-min))
                    (re-search-forward "^$" nil 'move)
                    (buffer-substring-no-properties (point) (point-max))))
         (json (json-read-from-string response))
         (models (cdr (assoc 'data json)))
         (buffer (get-buffer-create "*LLM Models*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Available Models\n")
      (insert "================\n\n")
      (dolist (model models)
        (insert (format "• %s\n" (cdr (assoc 'id model)))))
      (goto-char (point-min)))
    (display-buffer buffer)))

(defun llm-show-budget ()
  "Show current budget status."
  (interactive)
  (shell-command "cd ~/path/to/scripts && ./show-budget.sh"))

(defun llm-test-current-model ()
  "Test the current model with a simple prompt."
  (interactive)
  (let ((buffer (get-buffer-create "*LLM Test*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "Testing: %s/%s\n\n" 
                      (gptel-backend-name gptel-backend)
                      gptel-model))
      (insert "Prompt: Write 'Hello from Emacs'\n\n")
      (goto-char (point-max))
      (gptel-send))))

;; Keybindings
(global-set-key (kbd "C-c L s") 'llm-check-status)
(global-set-key (kbd "C-c L m") 'llm-show-models)
(global-set-key (kbd "C-c L b") 'llm-show-budget)
(global-set-key (kbd "C-c L t") 'llm-test-current-model)

(provide 'llm-monitor)
```

### 2. Dashboard View

Create a simple dashboard:

```elisp
(defun llm-dashboard ()
  "Show LLM dashboard."
  (interactive)
  (let ((buffer (get-buffer-create "*LLM Dashboard*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "╔════════════════════════════════════╗\n")
      (insert "║         LLM Dashboard              ║\n")
      (insert "╚════════════════════════════════════╝\n\n")
      
      (insert "Current Configuration:\n")
      (insert (format "  Backend: %s\n" (gptel-backend-name gptel-backend)))
      (insert (format "  Model: %s\n" gptel-model))
      (insert (format "  Stream: %s\n\n" (if gptel-stream "enabled" "disabled")))
      
      (insert "Quick Actions:\n")
      (insert-button "  [Check Status]" 
                     'action (lambda (_) (llm-check-status)))
      (insert " ")
      (insert-button "[Show Models]" 
                     'action (lambda (_) (llm-show-models)))
      (insert " ")
      (insert-button "[Test Model]" 
                     'action (lambda (_) (llm-test-current-model)))
      (insert "\n\n")
      
      (insert "Switch Model:\n")
      (insert-button "  [Local]" 
                     'action (lambda (_) (use-local-ollama)))
      (insert " ")
      (insert-button "[LAN]" 
                     'action (lambda (_) (use-lan-ollama)))
      (insert "\n")
      
      (goto-char (point-min)))
    (switch-to-buffer buffer)))

;; Bind to key
(global-set-key (kbd "C-c L d") 'llm-dashboard)
```

## Summary

With this setup, you can:

1. **Test locally first** without any cloud API keys
2. **Use Ollama on localhost** and on other LAN machines
3. **Monitor everything from Emacs** with dashboard and status checks
4. **Switch between local and LAN models** easily

Start with local testing, verify everything works, then add cloud keys one at a time to test each provider.