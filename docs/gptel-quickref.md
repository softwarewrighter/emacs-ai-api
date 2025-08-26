# gptel Quick Reference

## Loading the Enhanced Configuration

```elisp
;; In Emacs:
M-x load-file RET
~/github/softwarewrighter/emacs-ai-api/llm-gateway/emacs/gptel-enhanced.el
```

## Key Bindings

### Viewing Available Models

| Key       | Command                              | Description                        |
|-----------|--------------------------------------|------------------------------------|
| `C-c g l` | `gptel-show-available-models`       | List ALL available models          |
| `C-c g s` | `gptel-select-model-interactive`    | Select backend and model with menu |
| `C-c g ?` | `gptel-show-current-config`         | Show current backend/model         |

### Switching to Big72 Models

| Key         | Model Selected          | Description                      |
|-------------|------------------------|----------------------------------|
| `C-c g b d` | `big72-default`        | Default model on big72.local    |
| `C-c g b l` | `big72-llama3`         | Llama 3.2 on big72              |
| `C-c g b m` | `big72-mistral`        | Mistral on big72                |
| `C-c g b q` | `big72-qwen`           | Qwen Coder on big72             |
| `C-c g b s` | `big72-deepseek`       | DeepSeek on big72               |
| `C-c g b b` | (interactive select)   | Direct Ollama big72 with menu   |

### Quick Model Switches

| Key       | Model Selected           | Description                    |
|-----------|-------------------------|--------------------------------|
| `C-c g 1` | `localhost-llama3`      | Llama on your Mac             |
| `C-c g 2` | `localhost-qwen-coder`  | Qwen Coder on your Mac        |

### Sending Content

| Key         | Command              | Description                    |
|-------------|---------------------|--------------------------------|
| `C-c g RET` | `gptel-send`        | Send current buffer            |
| `C-c g r`   | `gptel-send-region` | Send selected region           |
| `C-c g m`   | `gptel-menu`        | Open gptel menu                |

## Interactive Model Selection

### Method 1: Menu Selection
```
C-c g s
Select backend: LiteLLM Gateway
Select model: big72-llama3
```

### Method 2: View All Models First
```
C-c g l    ; Shows all available models in a buffer
C-c g s    ; Then select what you want
```

### Method 3: Direct Big72 Selection
```
C-c g b b  ; Switches to big72 Ollama and shows model menu
```

## Example Workflow

```elisp
;; 1. Load configuration
M-x load-file RET gptel-enhanced.el

;; 2. Check what's available
C-c g l    ; See all models

;; 3. Switch to big72 Llama
C-c g b l  ; Quick switch to big72-llama3

;; 4. Verify selection
C-c g ?    ; Shows: "Backend: LiteLLM-Local | Model: big72-llama3"

;; 5. Type your prompt in a buffer
"Explain quantum computing in simple terms"

;; 6. Send to the model
C-c g RET  ; Sends buffer content

;; 7. Switch to a different big72 model
C-c g b q  ; Switch to big72-qwen for coding

;; 8. Or interactively browse and select
C-c g s    ; Choose from menu
```

## Available Big72 Models via LiteLLM

- `big72-default` - Whatever is configured as default on big72
- `big72-llama3` - Llama 3.2 model
- `big72-mistral` - Mistral model
- `big72-qwen` - Qwen 2.5 Coder
- `big72-deepseek` - DeepSeek R1

## Direct Ollama Access (Bypass LiteLLM)

If you want to bypass LiteLLM and connect directly to big72:

```elisp
C-c g b b  ; Opens menu to select from actual models on big72
```

This queries big72.local:11434 directly and shows the actual model list.

## Checking Current Configuration

At any time, press `C-c g ?` to see:
- Current backend (LiteLLM, Ollama localhost, Ollama big72)
- Current model
- Streaming status

## Tips

1. **See available models before choosing**: Use `C-c g l` first
2. **Quick switch for coding**: `C-c g b q` for big72 Qwen Coder
3. **Quick switch for chat**: `C-c g b l` for big72 Llama
4. **Interactive selection**: `C-c g s` when unsure
5. **Direct access**: `C-c g b b` to bypass LiteLLM routing