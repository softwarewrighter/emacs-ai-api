#!/usr/bin/env bash
# Show recent LLM usage history from PostgreSQL

set -euo pipefail

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

# Database connection
PGPASSWORD="${POSTGRES_PASSWORD:-localtest123}"
PGHOST="localhost"
PGPORT="5432"
PGDB="litellm"
PGUSER="postgres"

echo -e "${BLUE}═══════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}     LiteLLM Usage History${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════${NC}"
echo ""

# Check if PostgreSQL is accessible
if ! PGPASSWORD=$PGPASSWORD psql -h $PGHOST -p $PGPORT -U $PGUSER -d postgres -c '\q' 2>/dev/null; then
    echo -e "${YELLOW}⚠ PostgreSQL not accessible${NC}"
    echo "Make sure Docker is running: cd llm-gateway && docker compose up -d"
    exit 1
fi

# Recent requests (last 10)
echo -e "${GREEN}Recent Requests (last 10):${NC}"
PGPASSWORD=$PGPASSWORD psql -h $PGHOST -p $PGPORT -U $PGUSER -d $PGDB -t --no-align -F' | ' <<EOF 2>/dev/null || true
SELECT 
    TO_CHAR(created_at, 'HH24:MI:SS') as time,
    COALESCE(model, 'unknown') as model,
    COALESCE(api_key, 'no-key')::text as key,
    COALESCE(total_tokens, 0) as tokens,
    COALESCE(spend, 0)::numeric(10,6) as cost
FROM spend_logs
ORDER BY created_at DESC
LIMIT 10;
EOF

echo ""
echo -e "${GREEN}Model Usage Summary (last hour):${NC}"
PGPASSWORD=$PGPASSWORD psql -h $PGHOST -p $PGPORT -U $PGUSER -d $PGDB -t --no-align -F' | ' <<EOF 2>/dev/null || true
SELECT 
    model,
    COUNT(*) as requests,
    SUM(total_tokens) as total_tokens,
    SUM(spend)::numeric(10,6) as total_cost
FROM spend_logs
WHERE created_at > NOW() - INTERVAL '1 hour'
GROUP BY model
ORDER BY requests DESC;
EOF

echo ""
echo -e "${GREEN}Virtual Keys Usage:${NC}"
PGPASSWORD=$PGPASSWORD psql -h $PGHOST -p $PGPORT -U $PGUSER -d $PGDB -t --no-align -F' | ' <<EOF 2>/dev/null || true
SELECT 
    SUBSTRING(token, 1, 20) || '...' as key,
    COALESCE(spend, 0)::numeric(10,6) as spent,
    COALESCE(max_budget, 0)::numeric(10,2) as budget,
    CASE 
        WHEN max_budget > 0 THEN 
            ROUND((spend / max_budget * 100)::numeric, 1) || '%'
        ELSE 'unlimited'
    END as usage_pct
FROM litellm_verificationtoken
ORDER BY spend DESC
LIMIT 5;
EOF

echo ""
echo -e "${CYAN}Quick Commands:${NC}"
echo "  View UI:           http://localhost:4000/ui"
echo "  Master key:        sk-local-test-key-123"
echo "  Recent usage:      ./scripts/show-recent-usage.sh"
echo "  Test models:       ./scripts/probe.sh"
echo ""