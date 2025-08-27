# Emacs LLM Configuration

## ⚠️ IMPORTANT: Use Only ONE Configuration

### Current Configuration (USE THIS)
- **gptel-unified.el** - The single, clean configuration for all providers

### Deprecated Configurations (DO NOT USE)
- ~~gptel-openai.el~~ - Old hardcoded configuration
- ~~gptel-ollama-selector.el~~ - Old Ollama-only configuration  
- ~~gptel-all-providers.el~~ - Previous attempt at dynamic config

## Setup

Add to your Emacs init file:
```elisp
(load-file "~/path/to/llm-gateway/emacs/gptel-unified.el")
```

That's it. Nothing else needed.

## Quick Start

1. **Get help**: `C-c C-h` - Shows all commands and current status
2. **Select model**: `C-c C-m` - Choose from available models
3. **Send text**: `C-c RET` - Send current buffer/paragraph

## All Commands (Consistent C-c C-* pattern)

| Key | Command | Description |
|-----|---------|-------------|
| **C-c C-h** | **Help** | **Comprehensive help (START HERE)** |
| C-c C-m | Select model | Choose any model |
| C-c C-1 | Best quality | Claude Sonnet or GPT-4o |
| C-c C-2 | Fast/cheap | Haiku, GPT-4o-mini, or Flash |
| C-c C-3 | Local | Ollama models |
| C-c RET | Send | Send buffer/paragraph |
| C-c C-SPC | Send region | Send selected text |
| C-c C-n | New chat | Open new gptel buffer |
| C-c C-u | Usage | View costs and tokens |
| C-c C-r | Refresh | Re-discover models |
| C-c C-l | List | Show all models |
| C-c C-? | Status | Current model and backend |

## Features

- **Dynamic discovery** - No hardcoded models
- **All providers** - OpenAI, Anthropic, Google, Ollama
- **Usage tracking** - See costs and token usage
- **Built-in help** - Press `C-c C-h` anytime
- **Consistent bindings** - All commands follow C-c C-* pattern

## Troubleshooting

If models don't appear:
1. Ensure LiteLLM is running: `docker compose ps`
2. Refresh models: `C-c C-r`
3. Check help: `C-c C-h` to see status

## File Cleanup Plan

### Keep
- `gptel-unified.el` - The one true configuration
- `README.md` - This documentation

### Archive/Remove
- Other .el files - Deprecated, causes confusion
- Old documentation - Outdated bindings