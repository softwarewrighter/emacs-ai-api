#!/usr/bin/env bash
# Show recent LLM usage history from PostgreSQL using Docker

set -euo pipefail

# Help function
show_help() {
    cat << EOF
PURPOSE:
    Display LLM usage history and statistics from LiteLLM's PostgreSQL database.
    Shows recent requests, model usage summary, and total costs.

USAGE:
    $0
    $0 -h | --help

DESCRIPTION:
    Queries the LiteLLM PostgreSQL database to show:
    - Recent requests (last 10)
    - Model usage summary (last 24 hours)
    - Total usage statistics (all time)

REQUIREMENTS:
    - Docker containers running (cd llm-gateway && docker compose up -d)
    - PostgreSQL container (litellm-postgres) must be healthy
    - Docker command available

DATABASE INFO:
    - Container: litellm-postgres
    - Database: litellm
    - Table: LiteLLM_SpendLogs

ALTERNATIVE:
    View in web UI: http://localhost:4000/ui
    Login key: sk-local-test-key-123

EXIT CODES:
    0  Success
    1  Docker containers not running

EOF
    exit 0
}

# Check for help flag
if [[ "${1:-}" == "-h" ]] || [[ "${1:-}" == "--help" ]]; then
    show_help
fi

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

# Database connection
CONTAINER="litellm-postgres"
PGUSER="litellm"
PGDB="litellm"

echo -e "${BLUE}═══════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}     LiteLLM Usage History${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════${NC}"
echo ""

# Check if Docker container is running
if ! docker ps --format "table {{.Names}}" | grep -q "^$CONTAINER$"; then
    echo -e "${YELLOW}⚠ PostgreSQL container not running${NC}"
    echo "Please start Docker containers:"
    echo "  cd llm-gateway && docker compose up -d"
    exit 1
fi

# Recent requests (last 10)
echo -e "${GREEN}Recent Requests (last 10):${NC}"
echo "Time                 | Model               | Tokens | Cost"
echo "-------------------- | ------------------- | ------ | ----------"
docker exec $CONTAINER psql -U $PGUSER -d $PGDB -t --no-align -F' | ' -c "
SELECT 
    TO_CHAR(\"startTime\", 'YYYY-MM-DD HH24:MI:SS') as time,
    COALESCE(model, 'unknown') as model,
    COALESCE(total_tokens, 0) as tokens,
    COALESCE(spend, 0)::numeric(10,6) as cost
FROM \"LiteLLM_SpendLogs\"
ORDER BY \"startTime\" DESC
LIMIT 10;
" 2>/dev/null || echo "No data available"

echo ""
echo -e "${GREEN}Model Usage Summary (last 24 hours):${NC}"
echo "Model               | Requests | Tokens  | Cost"
echo "------------------- | -------- | ------- | ----------"
docker exec $CONTAINER psql -U $PGUSER -d $PGDB -t --no-align -F' | ' -c "
SELECT 
    model,
    COUNT(*) as requests,
    SUM(total_tokens) as total_tokens,
    SUM(spend)::numeric(10,6) as total_cost
FROM \"LiteLLM_SpendLogs\"
WHERE \"startTime\" > NOW() - INTERVAL '24 hours'
GROUP BY model
ORDER BY total_cost DESC NULLS LAST;
" 2>/dev/null || echo "No data available"

echo ""
echo -e "${GREEN}Total Usage (all time):${NC}"
docker exec $CONTAINER psql -U $PGUSER -d $PGDB -t --no-align -F' | ' -c "
SELECT 
    COUNT(*) as total_requests,
    SUM(total_tokens) as total_tokens,
    SUM(spend)::numeric(10,6) as total_cost
FROM \"LiteLLM_SpendLogs\";
" 2>/dev/null || echo "No data available"

echo ""
echo -e "${CYAN}═══════════════════════════════════════════════════════${NC}"
echo "View in UI: http://localhost:4000/ui (key: sk-local-test-key-123)"