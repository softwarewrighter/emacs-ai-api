#!/usr/bin/env bash
# Generate virtual keys for LiteLLM with per-project budgets and model restrictions
# Usage: ./gen-virtual-key.sh [project-name] [budget] [duration] [models]
# Example: ./gen-virtual-key.sh "web-scraper" 10 "1d" "coding-cheap,gpt-4o-mini"

set -euo pipefail

# Configuration
BASE="${BASE:-http://localhost:4000}"
ADMIN_KEY="${LITELLM_MASTER_KEY:?LITELLM_MASTER_KEY must be set}"

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored output
print_info() { echo -e "${BLUE}ℹ ${NC}$1"; }
print_success() { echo -e "${GREEN}✓${NC} $1"; }
print_error() { echo -e "${RED}✗${NC} $1"; }
print_warning() { echo -e "${YELLOW}⚠${NC} $1"; }

# Parse arguments
PROJECT_NAME="${1:-}"
BUDGET="${2:-10}"
DURATION="${3:-1d}"
MODELS="${4:-}"

# Interactive mode if no arguments
if [ -z "$PROJECT_NAME" ]; then
    echo -e "${BLUE}LiteLLM Virtual Key Generator${NC}"
    echo "=============================="
    echo ""
    
    read -p "Project name (e.g., 'web-app', 'data-pipeline'): " PROJECT_NAME
    if [ -z "$PROJECT_NAME" ]; then
        PROJECT_NAME="default-project"
    fi
    
    read -p "Budget limit in USD (default: 10): " BUDGET
    BUDGET="${BUDGET:-10}"
    
    echo "Duration options: 1h, 6h, 12h, 1d, 7d, 30d, 1mo"
    read -p "Budget duration (default: 1d): " DURATION
    DURATION="${DURATION:-1d}"
    
    echo ""
    echo "Available model aliases:"
    echo "  - coding-best (GPT-4o, Claude Sonnet)"
    echo "  - coding-balanced (GPT-4o-mini, Haiku, Gemini Flash)"
    echo "  - coding-cheap (local models, DeepSeek)"
    echo "  - coding-auto (all available)"
    echo "  - reasoning (o1-preview, o1-mini, Opus)"
    echo ""
    echo "Or specific models: gpt-4o, claude-3-5-sonnet, deepseek-coder, etc."
    echo ""
    read -p "Allowed models (comma-separated, or 'all' for all models): " MODELS
fi

# Set default models if not specified
if [ -z "$MODELS" ] || [ "$MODELS" = "all" ]; then
    MODELS_JSON='["coding-best","coding-balanced","coding-cheap","coding-auto","gpt-4o","gpt-4o-mini","claude-3-5-sonnet","claude-3-5-haiku","deepseek-coder","gemini-1.5-pro","gemini-1.5-flash","local-deepseek","local-kimi","ollama-deepseek","ollama-qwen","ollama-llama"]'
else
    # Convert comma-separated list to JSON array
    MODELS_JSON=$(echo "$MODELS" | jq -R -s -c 'split(",") | map(gsub("^\\s+|\\s+$"; ""))')
fi

# Create user email from project name
USER_EMAIL="${PROJECT_NAME}@project.local"

print_info "Creating virtual key for project: $PROJECT_NAME"
echo "  Budget: \$${BUDGET} per ${DURATION}"
echo "  Models: $(echo "$MODELS_JSON" | jq -r '.[]' | paste -sd ', ' -)"
echo ""

# Step 1: Create or get user
print_info "Creating/retrieving user..."

USER_RESPONSE=$(curl -sf -X POST "$BASE/user/new" \
    -H "Authorization: Bearer $ADMIN_KEY" \
    -H "Content-Type: application/json" \
    -d "{\"user_email\":\"$USER_EMAIL\",\"user_id\":\"$PROJECT_NAME\"}" 2>&1) || {
    # User might already exist, try to get it
    USER_RESPONSE=$(curl -sf -X GET "$BASE/user/info" \
        -H "Authorization: Bearer $ADMIN_KEY" \
        -H "Content-Type: application/json" \
        -d "{\"user_email\":\"$USER_EMAIL\"}" 2>&1) || {
        print_error "Failed to create or retrieve user"
        echo "$USER_RESPONSE"
        exit 1
    }
}

USER_ID=$(echo "$USER_RESPONSE" | jq -r '.user_id // .user_email // empty')
if [ -z "$USER_ID" ]; then
    print_warning "Could not extract user ID, using project name"
    USER_ID="$PROJECT_NAME"
fi

print_success "User: $USER_ID"

# Step 2: Generate key with budget
print_info "Generating API key with budget..."

KEY_REQUEST=$(jq -nc \
    --argjson models "$MODELS_JSON" \
    --arg user_id "$USER_ID" \
    --arg budget "$BUDGET" \
    --arg duration "$DURATION" \
    --arg alias "key-$PROJECT_NAME-$(date +%s)" \
    '{
        models: $models,
        user_id: $user_id,
        max_budget: ($budget | tonumber),
        budget_duration: $duration,
        metadata: {
            project: $user_id,
            created_at: now | todate
        },
        key_alias: $alias
    }')

KEY_RESPONSE=$(curl -sf -X POST "$BASE/key/generate" \
    -H "Authorization: Bearer $ADMIN_KEY" \
    -H "Content-Type: application/json" \
    -d "$KEY_REQUEST" 2>&1) || {
    print_error "Failed to generate key"
    echo "$KEY_RESPONSE"
    exit 1
}

# Extract the key
VIRTUAL_KEY=$(echo "$KEY_RESPONSE" | jq -r '.key // empty')
KEY_INFO=$(echo "$KEY_RESPONSE" | jq -r '.key_alias // .key_name // "N/A"')

if [ -z "$VIRTUAL_KEY" ]; then
    print_error "Failed to extract virtual key from response"
    echo "Response: $KEY_RESPONSE"
    exit 1
fi

print_success "Virtual key created successfully!"
echo ""

# Step 3: Save to file
KEY_FILE="keys/${PROJECT_NAME}.env"
mkdir -p keys

cat > "$KEY_FILE" << EOF
# LiteLLM Virtual Key for Project: $PROJECT_NAME
# Generated: $(date)
# Budget: \$$BUDGET per $DURATION
# Models: $MODELS

# API Configuration
LITELLM_VIRTUAL_KEY=$VIRTUAL_KEY
LITELLM_BASE_URL=$BASE/v1
PROJECT_NAME=$PROJECT_NAME

# Usage example:
# source $KEY_FILE
# curl \$LITELLM_BASE_URL/chat/completions \\
#   -H "Authorization: Bearer \$LITELLM_VIRTUAL_KEY" \\
#   -H "Content-Type: application/json" \\
#   -d '{"model":"coding-cheap","messages":[{"role":"user","content":"Hello"}]}'
EOF

print_success "Key saved to: $KEY_FILE"
echo ""

# Display summary
echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${GREEN}Virtual Key Created Successfully${NC}"
echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""
echo "Project:    $PROJECT_NAME"
echo "Key:        $VIRTUAL_KEY"
echo "Budget:     \$$BUDGET per $DURATION"
echo "Models:     $(echo "$MODELS_JSON" | jq -r '.[]' | head -3 | paste -sd ', ' -)..."
echo "Config:     $KEY_FILE"
echo ""
echo "To use this key:"
echo "  source $KEY_FILE"
echo "  ./probe.sh  # Uses LITELLM_VIRTUAL_KEY automatically"
echo ""
echo "Or in Emacs:"
echo "  (setenv \"LITELLM_VIRTUAL_KEY\" \"$VIRTUAL_KEY\")"
echo ""
echo "To check budget status:"
echo "  ./show-budget.sh $PROJECT_NAME"