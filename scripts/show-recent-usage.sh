#!/usr/bin/env bash
# Show recent LiteLLM usage

set -euo pipefail

# Help function
show_help() {
    cat << EOF
PURPOSE:
    Display recent LiteLLM usage including requests, model statistics, and virtual keys.
    Provides a quick overview of recent API activity and costs.

USAGE:
    $0 [OPTIONS]
    $0 -h | --help

OPTIONS:
    -h, --help    Show this help message
    -n, --number  Number of recent requests to show (default: 10)
    -d, --days    Show usage for last N days (default: all)

ENVIRONMENT:
    LITELLM_MASTER_KEY    Admin key for LiteLLM (default: sk-local-test-key-123)
    BASE_URL              API base URL (default: http://localhost:4000)

OUTPUT:
    - Recent requests with timestamps, models, tokens, and costs
    - Model usage summary with total spend
    - Virtual key information
    - Link to web UI

EXAMPLES:
    # Show last 10 requests (default)
    $0

    # Show last 20 requests
    $0 -n 20

    # Show usage for last 7 days
    $0 -d 7

REQUIREMENTS:
    - LiteLLM running at localhost:4000
    - Docker containers up (cd llm-gateway && docker compose up -d)
    - jq for JSON parsing

ALTERNATIVE:
    View in web UI: http://localhost:4000/ui
    Login with master key

EOF
    exit 0
}

# Default values
NUM_REQUESTS=10
DAYS_FILTER=""

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            show_help
            ;;
        -n|--number)
            NUM_REQUESTS="$2"
            shift 2
            ;;
        -d|--days)
            DAYS_FILTER="$2"
            shift 2
            ;;
        *)
            echo "Unknown option: $1"
            echo "Use -h or --help for usage information"
            exit 1
            ;;
    esac
done

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

MASTER_KEY="${LITELLM_MASTER_KEY:-sk-local-test-key-123}"
BASE_URL="${BASE_URL:-http://localhost:4000}"

# Check if LiteLLM is accessible
if ! curl -sf "$BASE_URL/health/readiness" >/dev/null 2>&1; then
    echo -e "${RED}✗ LiteLLM not accessible at $BASE_URL${NC}"
    echo "  Start with: cd llm-gateway && docker compose up -d"
    echo "  Check health: ./scripts/health.sh"
    exit 1
fi

echo -e "${BLUE}═══════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}     Recent LiteLLM Usage${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════${NC}"
echo ""

# Try to get data from PostgreSQL first (more reliable)
if docker ps --format "table {{.Names}}" | grep -q "litellm-postgres"; then
    # Build date filter if specified
    DATE_FILTER=""
    if [ -n "$DAYS_FILTER" ]; then
        DATE_FILTER="AND \"startTime\" > NOW() - INTERVAL '$DAYS_FILTER days'"
    fi
    
    echo -e "${GREEN}Recent Requests (last $NUM_REQUESTS):${NC}"
    echo "Time                 | Model               | Tokens | Cost"
    echo "-------------------- | ------------------- | ------ | ----------"
    
    docker exec litellm-postgres psql -U litellm -d litellm -t --no-align -F' | ' -c "
        SELECT 
            TO_CHAR(\"startTime\", 'YYYY-MM-DD HH24:MI:SS') as time,
            COALESCE(model, 'unknown') as model,
            COALESCE(total_tokens, 0) as tokens,
            '$' || COALESCE(spend, 0)::numeric(10,6) as cost
        FROM \"LiteLLM_SpendLogs\"
        WHERE 1=1 $DATE_FILTER
        ORDER BY \"startTime\" DESC
        LIMIT $NUM_REQUESTS;" 2>/dev/null || {
            echo "No data available"
        }
    
    echo ""
    echo -e "${GREEN}Model Usage Summary:${NC}"
    echo "Model                | Requests | Total Tokens | Total Cost"
    echo "-------------------- | -------- | ------------ | ----------"
    
    docker exec litellm-postgres psql -U litellm -d litellm -t --no-align -F' | ' -c "
        SELECT 
            model,
            COUNT(*) as requests,
            SUM(total_tokens) as total_tokens,
            '$' || SUM(spend)::numeric(10,6) as total_cost
        FROM \"LiteLLM_SpendLogs\"
        WHERE 1=1 $DATE_FILTER
        GROUP BY model
        ORDER BY SUM(spend) DESC NULLS LAST
        LIMIT 10;" 2>/dev/null || {
            echo "No data available"
        }
else
    # Fallback to API endpoints
    echo -e "${GREEN}Recent Requests:${NC}"
    RESPONSE=$(curl -s -X GET "$BASE_URL/spend/logs" \
      -H "Authorization: Bearer $MASTER_KEY" 2>/dev/null || echo "{}")
    
    if [ "$RESPONSE" != "{}" ] && [ -n "$RESPONSE" ]; then
        echo "$RESPONSE" | jq -r --arg num "$NUM_REQUESTS" '
          .[:($num | tonumber)] | .[] | 
          "\(.created_at // "unknown") | Model: \(.model // "unknown") | Tokens: \(.total_tokens // 0) | Cost: $\(.spend // 0)"
          ' 2>/dev/null || echo "No data available"
    else
        echo "No recent requests found"
    fi
    
    echo ""
    echo -e "${GREEN}Model Usage Summary:${NC}"
    # Get model usage stats
    GLOBAL_SPEND=$(curl -s -X GET "$BASE_URL/global/spend" \
      -H "Authorization: Bearer $MASTER_KEY" 2>/dev/null || echo "{}")
    
    if [ "$GLOBAL_SPEND" != "{}" ] && [ -n "$GLOBAL_SPEND" ]; then
        echo "$GLOBAL_SPEND" | jq '.' 2>/dev/null || echo "No data available"
    else
        echo "No usage data available"
    fi
fi

echo ""
echo -e "${GREEN}Virtual Keys:${NC}"
# Try to list virtual keys
KEY_INFO=$(curl -s -X GET "$BASE_URL/key/list" \
  -H "Authorization: Bearer $MASTER_KEY" 2>/dev/null || echo "[]")

if [ "$KEY_INFO" != "[]" ] && [ -n "$KEY_INFO" ] && echo "$KEY_INFO" | jq -e '.[0]' >/dev/null 2>&1; then
    echo "$KEY_INFO" | jq -r '
      .[] | 
      "Key: \(.token[0:20])... | Models: \(.models // ["all"] | join(",")) | Budget: $\(.max_budget // "unlimited")"
      ' 2>/dev/null || echo "No virtual keys found"
else
    # Check local keys directory
    if [ -d "keys" ] && [ "$(ls -A keys 2>/dev/null)" ]; then
        echo "Local key files found:"
        for key_file in keys/*.env; do
            if [ -f "$key_file" ]; then
                echo "  • $(basename "$key_file" .env)"
            fi
        done
    else
        echo "No virtual keys found"
        echo "Create one with: ./gen-virtual-key.sh <project-name>"
    fi
fi

echo ""
echo -e "${YELLOW}═══════════════════════════════════════════════════════${NC}"
echo -e "${YELLOW}Access the full UI at:${NC} http://localhost:4000/ui"
echo -e "${YELLOW}Master key:${NC} $MASTER_KEY"
echo ""
echo "For more detailed information:"
echo "  • Budget status: ./show-budget.sh"
echo "  • Usage history: ./llm-history.sh"
echo "  • Health check:  ./health.sh"