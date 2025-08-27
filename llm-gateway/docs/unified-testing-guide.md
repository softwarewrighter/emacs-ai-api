# Unified Configuration Testing Guide

## Quick Verification Checklist

Test these keybindings in Emacs after loading gptel-unified.el:

### 1. Load Configuration
```elisp
M-x load-file RET ~/path/to/llm-gateway/emacs/gptel-unified.el
```

### 2. Test Help System
- **C-c C-h** - Should display comprehensive help buffer
  - Shows current status
  - Lists all keybindings
  - Shows available models by provider

### 3. Test Model Selection
- **C-c C-m** - Interactive model selection with completion
  - Should show all discovered models
  - Test selecting different models
  
- **C-c C-1** - Quick select best quality
  - Should select Claude or GPT-4o
  
- **C-c C-2** - Quick select fast/cheap
  - Should select Haiku or GPT-4o-mini
  
- **C-c C-3** - Quick select local
  - Should select Ollama model

### 4. Test Utilities
- **C-c C-l** - List all models
  - Should show comma-separated list in minibuffer
  
- **C-c C-?** - Show status
  - Should display: Model | Backend | Total models
  
- **C-c C-u** - Show usage
  - Should open usage buffer with costs

- **C-c C-r** - Refresh models
  - Should rediscover models from LiteLLM

### 5. Test Sending (Interactive)
- **C-c C-n** - Open new chat buffer
- Type: "What is 2+2?"
- **C-c RET** - Send the message
  - Should get response "4" or similar

## Expected Behavior

### Help Buffer (C-c C-h)
Should show:
- Current backend status (Connected)
- Current model (e.g., llama3.2:latest)
- Number of available models
- Complete keybinding reference
- Models organized by provider

### Usage Buffer (C-c C-u)
Should show:
- Recent requests with model@location format
- Token counts
- Costs for paid models
- Total summary
- Refresh with 'g', close with 'q'

### Model Selection (C-c C-m)
Should offer completion with models like:
- llama3.2:latest
- qwen2.5-coder:14b
- gpt-4o
- gpt-4o-mini
- claude-3-5-sonnet-20241022
- gemini-1.5-pro

## Common Issues

### No Models Available
1. Check LiteLLM is running: `docker compose ps`
2. Press **C-c C-r** to refresh models
3. Check health: `curl http://localhost:4000/health`

### Backend Not Connected
1. Ensure LiteLLM is running
2. Reload configuration with M-x load-file
3. Press **C-c C-r** to refresh

### API Keys Not Working
1. Check ~/.env has your keys
2. Restart Docker: `docker compose restart`
3. Verify in UI: http://localhost:4000/ui

## Verification Commands

```bash
# Check LiteLLM health
curl http://localhost:4000/health/readiness

# List available models
curl -s http://localhost:4000/v1/models \
  -H "Authorization: Bearer sk-local-test-key-123" | \
  jq -r '.data[].id'

# Test a model directly
curl -s -X POST http://localhost:4000/v1/chat/completions \
  -H "Authorization: Bearer sk-local-test-key-123" \
  -H "Content-Type: application/json" \
  -d '{
    "model": "llama3.2:latest",
    "messages": [{"role": "user", "content": "Hello"}],
    "max_tokens": 50
  }' | jq .
```