#!/bin/bash
# Restart LiteLLM to pick up new API keys

echo "Restarting LiteLLM to load new API keys..."
docker compose restart litellm

echo "Waiting for LiteLLM to be ready..."
sleep 5

# Check health (try both authenticated and unauthenticated)
if curl -s -H "Authorization: Bearer sk-local-test-key-123" http://localhost:4000/v1/models > /dev/null 2>&1; then
    echo "✓ LiteLLM is running"
    
    # Show available models
    echo ""
    echo "Available models:"
    curl -s -H "Authorization: Bearer sk-local-test-key-123" \
         http://localhost:4000/v1/models | \
         jq -r '.data[].id' | sort
else
    echo "✗ LiteLLM health check failed"
    echo "Check logs with: docker compose logs litellm"
fi