# Testing OpenAI Models in Emacs via LiteLLM

## Prerequisites

1. **LiteLLM Gateway Running**
   ```bash
   cd llm-gateway
   docker compose up -d
   ```

2. **OpenAI API Key Configured**
   - Ensure `.env.openai` contains your `OPENAI_API_KEY`
   - LiteLLM should be restarted after adding the key

## Available OpenAI Models

The following models are configured and available (with valid API key):

- **gpt-4o-mini**: Fast, cost-effective model for most tasks
- **gpt-4o**: More capable model for complex reasoning
- **gpt-4-turbo**: Previous generation, still very capable

Note: o1-preview and o1-mini require special access from OpenAI and are not available with standard API keys.

## Batch Testing

Run the automated test script to verify end-to-end functionality:

```bash
cd llm-gateway
./test-openai-batch.sh
```

This script:
- Tests each available OpenAI model
- Verifies responses are received
- Validates correct answers
- Reports any errors or timeouts

## Interactive Testing in Emacs

### 1. Load the Configuration

```elisp
M-x load-file RET
/path/to/llm-gateway/emacs/gptel-openai.el RET
```

Or add to your init.el:
```elisp
(load-file "~/github/softwarewrighter/emacs-ai-api/llm-gateway/emacs/gptel-openai.el")
```

### 2. Key Bindings

The configuration provides these key bindings:

| Key Binding | Function | Description |
|------------|----------|-------------|
| `C-c o m` | `gptel-use-gpt4o-mini` | Select GPT-4o-mini (cheaper, faster) |
| `C-c o g` | `gptel-use-gpt4o` | Select GPT-4o (more capable) |
| `C-c o c` | `gptel-check-openai-models` | List available OpenAI models |
| `C-c o s` | `gptel-safe-send` | Send with error handling |
| `C-c o ?` | `gptel-show-current-config` | Show current backend/model |

### 3. Testing Workflow

1. **Check Available Models**
   ```
   C-c o c
   ```
   Should show: "Available OpenAI models: gpt-4o, gpt-4o-mini, gpt-4-turbo"

2. **Select a Model**
   ```
   C-c o m    ; For GPT-4o-mini (recommended for testing)
   ```
   Message: "Selected: GPT-4o-mini via LiteLLM"

3. **Create Test Prompt**
   - Open a new buffer or use scratch buffer
   - Type your prompt:
   ```
   What is the capital of France? Reply in one word.
   ```

4. **Send to Model**
   ```
   C-c o s    ; Safe send with error handling
   ```
   Or use standard gptel:
   ```
   C-c RET    ; Standard gptel-send
   ```

5. **View Response**
   - Response appears in the same buffer
   - Should see: "Paris"

### 4. Troubleshooting

#### Error: `:null` or streaming errors
The configuration includes `gptel-safe-send` which automatically retries without streaming:
```
C-c o s    ; Instead of C-c RET
```

#### Error: Model not found
Check your API key is properly configured:
```bash
# Verify key is in environment
cat llm-gateway/.env.openai

# Restart LiteLLM
cd llm-gateway
docker compose restart litellm

# Check models via curl
curl -H "Authorization: Bearer sk-local-test-key-123" \
     http://localhost:4000/v1/models | jq '.data[].id' | grep gpt
```

#### No response or timeout
1. Check LiteLLM logs:
   ```bash
   cd llm-gateway
   docker compose logs -f litellm
   ```

2. Verify connectivity:
   ```bash
   curl -X POST http://localhost:4000/v1/chat/completions \
     -H "Content-Type: application/json" \
     -H "Authorization: Bearer sk-local-test-key-123" \
     -d '{
       "model": "gpt-4o-mini",
       "messages": [{"role": "user", "content": "Hi"}]
     }'
   ```

3. Check OpenAI API status: https://status.openai.com/

### 5. Monitor Usage

View usage statistics in LiteLLM UI:
1. Open http://localhost:4000/ui
2. Login with:
   - Username: `admin`
   - Password: `sk-local-test-key-123`
3. Navigate to:
   - **Logs**: Recent requests and responses
   - **Usage**: Token consumption and costs
   - **Models**: Available models and their status

### 6. Example Interactive Session

```elisp
;; 1. Check current configuration
C-c o ?
;; Shows: "Backend: LiteLLM-OpenAI | Model: gpt-4o-mini | Stream: enabled"

;; 2. List available models
C-c o c
;; Shows: "Available OpenAI models: gpt-4o, gpt-4o-mini, gpt-4-turbo"

;; 3. Select GPT-4o for complex task
C-c o g
;; Message: "Selected: GPT-4o via LiteLLM"

;; 4. Type prompt in buffer
Write a haiku about programming.

;; 5. Send safely
C-c o s

;; 6. Response appears:
Logic flows like streams
Bugs hide in syntax shadows
Coffee fuels the code
```

## Cost Considerations

Approximate costs (as of 2024):
- **GPT-4o-mini**: $0.15 / 1M input tokens, $0.60 / 1M output tokens
- **GPT-4o**: $5 / 1M input tokens, $15 / 1M output tokens
- **GPT-4-turbo**: $10 / 1M input tokens, $30 / 1M output tokens

Use GPT-4o-mini for testing and simple queries to minimize costs.

## Security Notes

- Never commit `.env.openai` with your actual API key
- The master key `sk-local-test-key-123` is for local development only
- In production, use proper secrets management
- Monitor usage regularly to detect any unexpected activity