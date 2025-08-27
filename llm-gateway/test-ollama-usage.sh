#!/bin/bash
# Test Ollama models usage tracking via LiteLLM

set -e

echo "================================================"
echo "Testing Ollama Usage Tracking via LiteLLM"
echo "================================================"

# Test with qwen2.5-coder:14b model
echo -e "\nTesting qwen2.5-coder:14b (localhost)..."
curl -X POST http://localhost:4000/v1/chat/completions \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer sk-local-test-key-123" \
  -d '{
    "model": "qwen2.5-coder:14b",
    "messages": [{"role": "user", "content": "What is 2+2? Reply with just the number."}],
    "max_tokens": 10
  }' 2>/dev/null | jq -r '.choices[0].message.content'

# Test with qwen2.5:7b on big72
echo -e "\nTesting qwen2.5:7b (big72)..."
curl -X POST http://localhost:4000/v1/chat/completions \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer sk-local-test-key-123" \
  -d '{
    "model": "qwen2.5:7b",
    "messages": [{"role": "user", "content": "What is 3+3? Reply with just the number."}],
    "max_tokens": 10
  }' 2>/dev/null | jq -r '.choices[0].message.content' || echo "Failed - is big72 available?"

# Test with mistral on big72
echo -e "\nTesting mistral:latest (big72)..."
curl -X POST http://localhost:4000/v1/chat/completions \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer sk-local-test-key-123" \
  -d '{
    "model": "mistral:latest",
    "messages": [{"role": "user", "content": "What is 4+4? Reply with just the number."}],
    "max_tokens": 10
  }' 2>/dev/null | jq -r '.choices[0].message.content' || echo "Failed - is big72 available?"

# Wait a moment for logs to update
sleep 2

# Check usage logs
echo -e "\n================================================"
echo "Checking Usage Logs..."
echo "================================================"

# Get the last 5 spend logs
echo -e "\nRecent Usage (last 5 requests):"
curl -s -H "Authorization: Bearer sk-local-test-key-123" \
     http://localhost:4000/spend/logs | \
     jq -r '.[:5] | .[] | 
     "Model: \(.model) | Tokens: \(.total_tokens) | Cost: $\(.spend) | Time: \(.startTime[:19])"'

# Write usage to log file
LOG_FILE="/tmp/ollama-usage-test.log"
echo -e "\n================================================" > $LOG_FILE
echo "Full Usage Report (written to $LOG_FILE)" | tee -a $LOG_FILE
echo "================================================" | tee -a $LOG_FILE
curl -s -H "Authorization: Bearer sk-local-test-key-123" \
     http://localhost:4000/spend/logs | \
     jq -r '.[] | 
     "Time: \(.startTime[:19])
Model: \(.model)
Provider: \(.custom_llm_provider) 
API Base: \(.api_base)
Tokens: \(.total_tokens) (prompt: \(.prompt_tokens), completion: \(.completion_tokens))
Cost: $\(.spend)
---"' >> $LOG_FILE

echo -e "\nUsage report saved to: $LOG_FILE"

# Summary by model
echo -e "\n================================================"
echo "Usage Summary by Model:"
echo "================================================"
curl -s -H "Authorization: Bearer sk-local-test-key-123" \
     http://localhost:4000/spend/logs | \
     jq -r 'group_by(.model) | 
     map({
       model: .[0].model,
       count: length,
       total_tokens: map(.total_tokens) | add,
       total_cost: map(.spend) | add
     }) | 
     .[] | 
     "• \(.model): \(.count) requests, \(.total_tokens) tokens, $\(.total_cost)"'

echo -e "\n================================================"
echo "Test Complete!"
echo "================================================"
echo ""
echo "To view usage in Emacs:"
echo "1. Reload config: M-x load-file RET .../gptel-openai.el"
echo "2. Check usage: C-c o u"
echo ""
echo "The usage should now show ALL models with their actual names."