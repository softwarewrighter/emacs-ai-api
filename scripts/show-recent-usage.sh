#!/usr/bin/env bash
# Show recent LiteLLM usage

set -euo pipefail

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m'

MASTER_KEY="${LITELLM_MASTER_KEY:-sk-local-test-key-123}"
BASE_URL="http://localhost:4000"

echo -e "${BLUE}═══════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}     Recent LiteLLM Usage${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════${NC}"
echo ""

# Get spend logs (last 10 requests)
echo -e "${GREEN}Recent Requests:${NC}"
curl -s -X GET "$BASE_URL/spend/logs" \
  -H "Authorization: Bearer $MASTER_KEY" | jq -r '
  .[] | 
  "\(.created_at // "unknown") | Model: \(.model // "unknown") | Tokens: \(.total_tokens // 0) | Cost: $\(.spend // 0)"
  ' | head -10

echo ""
echo -e "${GREEN}Model Usage Summary:${NC}"
# Get model usage stats
curl -s -X GET "$BASE_URL/global/spend" \
  -H "Authorization: Bearer $MASTER_KEY" | jq '.'

echo ""
echo -e "${GREEN}Virtual Keys:${NC}"
# List virtual keys
curl -s -X GET "$BASE_URL/key/info" \
  -H "Authorization: Bearer $MASTER_KEY" | jq -r '
  .[] | 
  "Key: \(.token[0:20])... | Models: \(.models // ["all"]) | Budget: $\(.max_budget // "unlimited")"
  '

echo ""
echo -e "${YELLOW}Access the full UI at:${NC} http://localhost:4000/ui"
echo -e "${YELLOW}Master key:${NC} $MASTER_KEY"