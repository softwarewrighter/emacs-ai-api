#!/bin/bash

# Fix the .env file by removing the extra parenthesis
if [ -f ~/.env ]; then
    echo "Fixing ~/.env file..."

    # Backup the original
    cp ~/.env ~/.env.backup

    # Fix the LITELLM_KEY line (remove trailing parenthesis)
    sed -i '' 's/LITELLM_KEY=sk-local-test-key-123)/LITELLM_KEY=sk-local-test-key-123/' ~/.env

    echo "Fixed! Here are the LITELLM variables:"
    grep "^LITELLM_" ~/.env
else
    echo "No ~/.env file found. Creating one..."
    cat > ~/.env << 'EOF'
# LiteLLM Configuration for Emacs
LITELLM_HOST=localhost:4000
LITELLM_KEY=sk-local-test-key-123
LITELLM_MASTER_KEY=sk-local-test-key-123
EOF
    echo "Created ~/.env with default settings"
fi