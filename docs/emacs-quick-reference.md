# Emacs LLM Quick Reference

Quick reference for using LLMs from Emacs with monitoring and cost tracking.

## Setup (One Time)

```elisp
;; Add to init.el or .emacs
(add-to-list 'load-path "~/path/to/llm-gateway/emacs/")
(require 'gptel-config)     ; Multi-backend support
(require 'llm-monitor)       ; Monitoring functions
(require 'gptel-local-only)  ; Local-only testing
```

## Key Bindings

### Model Control (C-c l)
| Key | Command | Description |
|-----|---------|-------------|
| `C-c l b` | `llm-switch-backend` | Choose backend (LiteLLM, Ollama, etc.) |
| `C-c l m` | `llm-switch-model` | Choose model from current backend |
| `C-c l 1` | `llm-use-best` | Use best quality (GPT-4o/Claude) |
| `C-c l 2` | `llm-use-balanced` | Use balanced cost/quality |
| `C-c l 3` | `llm-use-cheap` | Use cheapest (local first) |
| `C-c l l` | `llm-use-local` | Use local Ollama only |
| `C-c l s` | `llm-show-status` | Show current backend/model |
| `C-c l t` | `llm-test-current` | Test current model |
| `C-c l c` | `gptel-send` | Send prompt in current buffer |

### Monitoring (C-c L)
| Key | Command | Description |
|-----|---------|-------------|
| `C-c L s` | `llm-check-status` | Check all services status |
| `C-c L m` | `llm-show-models` | List available models |
| `C-c L b` | `llm-show-budget` | Show budget/spending |
| `C-c L $` | `llm-check-spend` | Quick spend check |
| `C-c L d` | `llm-dashboard` | Open interactive dashboard |
| `C-c L t` | `llm-test-current-model` | Test active model |

## Quick Model Switching

```elisp
;; In any buffer
M-x llm-use-best       ; High quality (cloud)
M-x llm-use-cheap      ; Low cost (local)
M-x use-local-ollama   ; Force local only
M-x use-lan-ollama     ; Use LAN server

;; Interactive selection
M-x llm-switch-backend ; Pick from list
M-x llm-switch-model   ; Pick model
```

## Testing Models

### From Emacs
```elisp
;; Quick test in temporary buffer
M-x llm-test-current

;; Test in current buffer
Type your prompt, then:
C-c l c  ; or M-x gptel-send

;; Test specific model
(llm-quick-switch "LiteLLM" "gpt-4o-mini")
M-x llm-test-current
```

### From Command Line
```bash
# Test current default
./scripts/probe.sh

# Test specific model
MODEL=local-ollama-deepseek ./scripts/probe.sh

# Test all configured models
./scripts/test-all.sh quick
```

## Local Setup Testing

### 1. Test Without Cloud Keys
```elisp
;; Load local-only config
(load-file "~/path/to/llm-gateway/emacs/gptel-local-only.el")

;; Use local models
M-x use-local-ollama  ; localhost:11434
M-x use-lan-ollama    ; 192.168.1.x:11434
```

### 2. Check Service Status
```elisp
;; Check what's running
M-x llm-check-status

;; Output shows:
;; LiteLLM Gateway: ● Online/Offline
;; Ollama (localhost): ● Online/Offline
;; Ollama (LAN): ● Online/Offline
```

## Cost Monitoring

### Check Spending from Emacs
```elisp
;; Quick check total spend
C-c L $  ; Shows total in minibuffer

;; Detailed budget view
C-c L b  ; Opens budget report

;; Custom project check
(shell-command "cd ~/scripts && ./show-budget.sh myproject")
```

### Set Project Budgets
```bash
# Create limited key for project
cd scripts
./gen-virtual-key.sh "emacs-work" 5 "1d" "coding-cheap"

# Use in Emacs
(setenv "LITELLM_VIRTUAL_KEY" "sk-generated-key")
(setq llm-litellm-key (getenv "LITELLM_VIRTUAL_KEY"))
```

## Common Workflows

### 1. Start Local Testing
```bash
# Terminal 1: Start Ollama
ollama serve

# Terminal 2: Start LiteLLM
cd llm-gateway
docker compose up -d

# Terminal 3: Test
cd scripts
./test-local-only.sh
```

### 2. Add New Local Model
```bash
# Pull model
ollama pull codellama:13b

# Add to config.yaml
# Restart LiteLLM
docker compose restart litellm

# Test from Emacs
M-x llm-show-models  ; Should see new model
```

### 3. Switch from Local to Cloud
```elisp
;; Start with local
M-x llm-use-cheap  ; Uses local first

;; Need better quality
M-x llm-use-best   ; Switches to GPT-4o/Claude

;; Check cost after session
M-x llm-check-spend
```

## Troubleshooting from Emacs

### Model Not Responding
```elisp
;; Check service status
M-x llm-check-status

;; Try different backend
M-x llm-switch-backend
; Select "Ollama-Direct" to bypass LiteLLM

;; Test with simple prompt
M-x llm-test-current
```

### High Costs
```elisp
;; Check current model
M-x llm-show-status  ; See which model is active

;; Switch to cheaper
M-x llm-use-cheap

;; Check spending
M-x llm-check-spend

;; Set project budget
(shell-command "cd ~/scripts && ./gen-virtual-key.sh low-budget 2 1d coding-cheap")
```

### Slow Response
```elisp
;; Check if streaming is enabled
(setq gptel-stream t)

;; Try local model
M-x use-local-ollama

;; Check latency
M-x llm-dashboard  ; Shows latencies
```

## Environment Variables

Set in shell or Emacs:
```elisp
;; In Emacs init
(setenv "LITELLM_BASE_URL" "http://localhost:4000/v1")
(setenv "LITELLM_MASTER_KEY" "sk-admin-yourkey")
(setenv "LITELLM_VIRTUAL_KEY" "sk-project-limited-key")

;; Check current values
(getenv "LITELLM_BASE_URL")
```

## Quick Status Dashboard

```elisp
(defun my-llm-dashboard ()
  "Personal LLM dashboard."
  (interactive)
  (let ((buffer (get-buffer-create "*My LLM*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "═══════════════════════\n")
      (insert (format "Backend: %s\n" 
                      (gptel-backend-name gptel-backend)))
      (insert (format "Model: %s\n" gptel-model))
      (insert "═══════════════════════\n")
      (insert-button "[Status]" 
                     'action (lambda (_) (llm-check-status)))
      (insert " ")
      (insert-button "[Models]" 
                     'action (lambda (_) (llm-show-models)))
      (insert " ")
      (insert-button "[Budget]" 
                     'action (lambda (_) (llm-show-budget)))
      (insert "\n")
      (insert-button "[Local]" 
                     'action (lambda (_) (llm-use-cheap)))
      (insert " ")
      (insert-button "[Cloud]" 
                     'action (lambda (_) (llm-use-best)))
      (goto-char (point-min)))
    (pop-to-buffer buffer)))

(global-set-key (kbd "<f9>") 'my-llm-dashboard)
```

## Tips

1. **Start with local models** - No costs, instant feedback
2. **Use routing aliases** - Let LiteLLM pick the best available
3. **Set project budgets** - Avoid surprise bills
4. **Monitor regularly** - Check spend after heavy sessions
5. **Cache system prompts** - Reduces token usage
6. **Use streaming** - See responses as they generate

## Essential Files to Edit

- `~/llm-gateway/.env` - API keys
- `~/llm-gateway/litellm/config.yaml` - Models & budgets
- `~/.emacs.d/init.el` - Load LLM configs
- `~/scripts/keys/*.env` - Project-specific keys