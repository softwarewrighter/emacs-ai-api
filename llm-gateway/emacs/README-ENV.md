# Emacs gptel Environment Configuration

The `gptel-unified.el` configuration now supports loading API keys and configuration from environment variables.

## Setup Instructions

### 1. Create your ~/.env file

Copy the example configuration to your home directory:
```bash
cp /path/to/llm-gateway/.env.example ~/.env
```

### 2. Configure your environment variables

Edit `~/.env` and add your configuration:

```bash
# LiteLLM connection settings
LITELLM_HOST=localhost:4000
LITELLM_KEY=sk-local-test-key-123

# If you have provider API keys, add them to your LiteLLM config.yaml
# The Emacs client doesn't use these directly - LiteLLM uses them
```

### 3. Load the configuration in Emacs

```elisp
;; Load the unified configuration
(load-file "/path/to/llm-gateway/emacs/gptel-unified.el")
```

## Features

- **Automatic env loading**: Reads from `~/.env` file on startup
- **Fallback support**: Uses sensible defaults if variables not found
- **Reload support**: Use `C-c C-r` to reload config after changing `~/.env`

## Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `LITELLM_HOST` | LiteLLM proxy host and port | `localhost:4000` |
| `LITELLM_KEY` | API key for LiteLLM proxy | `sk-local-test-key-123` |

## Key Bindings (Updated)

The key bindings have been simplified to avoid conflicts:

### Model Selection
- `C-c m` - Select any model (with completion)
- `C-c 1` - Quick select best quality model (Claude/GPT-4o)
- `C-c 2` - Quick select fast/cheap model (Haiku/GPT-4o-mini)
- `C-c 3` - Quick select local model (Ollama)

### Sending Text
- `C-c RET` - Send buffer/paragraph at point
- `C-c C-SPC` - Send selected region
- `C-c n` - Open new chat buffer

### Utilities
- `C-c C-h` - Show help
- `C-c C-u` - Show usage/costs
- `C-c C-r` - Refresh models from LiteLLM
- `C-c C-l` - List all available models
- `C-c C-?` - Show current configuration

## Troubleshooting

### Issue: "API key not valid" error with Gemini

This means the Google API key is not configured in LiteLLM. Add it to your LiteLLM `config.yaml`:

```yaml
model_list:
  - model_name: gemini-1.5-flash
    litellm_params:
      model: gemini/gemini-1.5-flash
      api_key: ${GOOGLE_API_KEY}  # or ${GEMINI_API_KEY}
```

Then make sure the key is in your `.env` file that LiteLLM reads.

### Issue: C-c C-m evaluates region instead of selecting model

This was a keybinding conflict. The new version uses `C-c m` instead of `C-c C-m`.

### Issue: Models not loading

1. Check LiteLLM is running: `docker compose ps`
2. Test connection: `curl http://localhost:4000/v1/models -H "Authorization: Bearer sk-local-test-key-123"`
3. Reload in Emacs: `C-c C-r`

## Advanced Usage

### Custom env file location

You can specify a different env file:
```elisp
(gptel-unified-load-env-file "~/my-project/.env")
```

### Check loaded values

In Emacs, check what was loaded:
```elisp
M-: gptel-unified-litellm-host RET
M-: gptel-unified-litellm-key RET
```

### Debug environment loading

```elisp
M-: (gptel-unified-get-env "LITELLM_HOST") RET
M-: (gptel-unified-load-env-file) RET
```