# Emacs LLM Usage Guide

## Quick Start

1. Load the configuration:
   ```elisp
   (load-file "~/path/to/llm-gateway/emacs/gptel-all-providers.el")
   ```

2. Select a model:
   - `C-c l m` - Choose any model from list
   - `C-c l b` - Use best available (Claude Sonnet > GPT-4o > Gemini Pro)
   - `C-c l f` - Use fast/cheap model

3. Send text to the model:
   - `C-c RET` - Send current buffer/paragraph
   - `C-c l SPC` - Send selected region
   - `C-c l s` - Safe send (with error handling)

## Common Workflows

### Send a Region to Claude
1. Select text in buffer
2. `C-c l m` and choose `claude-3-5-sonnet-20241022`
3. `C-c l SPC` to send the region

### Quick Question in New Buffer
1. `C-c l n` - Opens new gptel buffer
2. `C-c l f` - Select fast model
3. Type your question
4. `C-c RET` - Send and get response

### Send Current Buffer
1. `C-c l b` - Select best model
2. `C-c RET` - Send entire buffer content

## All Key Bindings

### Model Selection
| Key | Function | Description |
|-----|----------|-------------|
| `C-c l l` | List all models | Shows grouped by provider |
| `C-c l m` | Select any model | Interactive selection |
| `C-c l p` | Select by provider | Filter by provider first |
| `C-c l b` | Best model | Auto-select highest quality |
| `C-c l f` | Fast model | Auto-select fast/cheap |
| `C-c l c` | Coding model | Optimized for code |
| `C-c l r` | Refresh models | Re-discover from LiteLLM |

### Sending Text
| Key | Function | Description |
|-----|----------|-------------|
| `C-c RET` | Send | Send buffer/paragraph |
| `C-c l RET` | Send | Alternative binding |
| `C-c l SPC` | Send region | Send selected text |
| `C-c l s` | Safe send | With error handling |

### Utilities
| Key | Function | Description |
|-----|----------|-------------|
| `C-c l n` | New gptel buffer | Start fresh conversation |
| `C-c l u` | Usage report | View costs and tokens |
| `C-c l ?` | Current config | Show model and settings |

## Tips

1. **Check current model**: Use `C-c l ?` to see which model is selected
2. **View costs**: Use `C-c l u` after sending to see token usage
3. **Refresh models**: Use `C-c l r` after adding new models to config
4. **Error handling**: Use `C-c l s` if you get streaming errors

## Model Categories

### Best Quality
- Claude 3.5 Sonnet
- GPT-4o
- Gemini 1.5 Pro
- Claude 3 Opus

### Fast/Cheap
- Claude 3.5 Haiku
- GPT-4o-mini
- Gemini 1.5 Flash
- Local Ollama models

### Coding
- Claude 3.5 Sonnet (best)
- GPT-4o
- qwen2.5-coder:14b (local)

## Troubleshooting

### "Wrong type argument: stringp, :null"
Use `C-c l s` (safe send) instead of `C-c RET`

### Model not found
1. `C-c l r` to refresh model list
2. `./restart-litellm.sh` if you added new API keys

### No response
1. Check LiteLLM is running: `docker compose ps`
2. Check model with `C-c l ?`
3. Try a different model with `C-c l m`