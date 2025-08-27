#!/bin/bash
# Check recent LLM usage and costs

echo "========================================="
echo "Recent LLM Usage (via LiteLLM)"
echo "========================================="

# Get recent usage via API
echo -e "\nRecent Requests:"
curl -s -H "Authorization: Bearer sk-local-test-key-123" \
     http://localhost:4000/global/activity/logs \
     2>/dev/null | jq -r '.[] | 
     "\(.startTime | strftime("%Y-%m-%d %H:%M:%S")): \(.model) - \(.total_tokens) tokens - $\(.response_cost)"' \
     2>/dev/null | head -10

# Get model-specific stats
echo -e "\nModel Usage Summary:"
curl -s -H "Authorization: Bearer sk-local-test-key-123" \
     http://localhost:4000/v1/models \
     2>/dev/null | jq -r '.data[] | select(.id | startswith("gpt")) | .id' | while read model; do
    echo "  $model: Available"
done

echo -e "\n========================================="
echo "For detailed stats, open:"
echo "http://localhost:4000/ui"
echo "Login: admin / sk-local-test-key-123"
echo "========================================="