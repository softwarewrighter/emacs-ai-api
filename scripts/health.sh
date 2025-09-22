#!/usr/bin/env bash
# Health check script for LiteLLM gateway and related services

set -euo pipefail

# Help function
show_help() {
    cat << EOF
PURPOSE:
    Check health status of LiteLLM gateway and related services.
    Verifies Docker containers, API endpoints, database, and model availability.

USAGE:
    $0 [OPTIONS]
    $0 -h | --help

OPTIONS:
    -h, --help    Show this help message
    -v, --verbose Show detailed output
    -q, --quiet   Minimal output (exit code only)

CHECKS PERFORMED:
    1. Docker containers (litellm, postgres)
    2. LiteLLM API readiness
    3. PostgreSQL connectivity
    4. Model availability
    5. Ollama instances (localhost, big72.local)
    6. API key validation

EXIT CODES:
    0  All services healthy
    1  One or more services unhealthy

EXAMPLES:
    # Basic health check
    $0

    # Verbose mode with details
    $0 -v

    # Quiet mode for scripts
    $0 -q && echo "healthy" || echo "unhealthy"

EOF
    exit 0
}

# Parse arguments
VERBOSE=false
QUIET=false

while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            show_help
            ;;
        -v|--verbose)
            VERBOSE=true
            shift
            ;;
        -q|--quiet)
            QUIET=true
            shift
            ;;
        *)
            echo "Unknown option: $1"
            echo "Use -h or --help for usage information"
            exit 1
            ;;
    esac
done

# Configuration
BASE="${BASE:-http://localhost:4000}"
KEY="${LITELLM_MASTER_KEY:-sk-local-test-key-123}"

# Color output (disabled in quiet mode)
if [ "$QUIET" = false ]; then
    RED='\033[0;31m'
    GREEN='\033[0;32m'
    YELLOW='\033[1;33m'
    BLUE='\033[0;34m'
    NC='\033[0m'
else
    RED=''
    GREEN=''
    YELLOW=''
    BLUE=''
    NC=''
fi

# Status tracking
OVERALL_STATUS=0
FAILED_SERVICES=""

# Function to check service and track status
check_service() {
    local service_name="$1"
    local check_command="$2"
    local success_msg="$3"
    local failure_msg="$4"
    
    if [ "$QUIET" = false ]; then
        printf "%-30s" "$service_name:"
    fi
    
    if eval "$check_command" >/dev/null 2>&1; then
        if [ "$QUIET" = false ]; then
            echo -e "${GREEN}✓ $success_msg${NC}"
        fi
        if [ "$VERBOSE" = true ] && [ "$QUIET" = false ]; then
            echo "  Details: $check_command"
        fi
        return 0
    else
        FAILED_SERVICES="$FAILED_SERVICES• $service_name\n"
        OVERALL_STATUS=1
        if [ "$QUIET" = false ]; then
            echo -e "${RED}✗ $failure_msg${NC}"
        fi
        if [ "$VERBOSE" = true ] && [ "$QUIET" = false ]; then
            echo "  Command failed: $check_command"
        fi
        return 1
    fi
}

# Header
if [ "$QUIET" = false ]; then
    echo -e "${BLUE}═══════════════════════════════════════════════════════${NC}"
    echo -e "${BLUE}     LiteLLM Gateway Health Check${NC}"
    echo -e "${BLUE}═══════════════════════════════════════════════════════${NC}"
    echo ""
fi

# 1. Check Docker containers
if [ "$QUIET" = false ]; then
    echo -e "${BLUE}Docker Containers:${NC}"
fi

check_service "LiteLLM Container" \
    "docker ps --format 'table {{.Names}}' | grep -q '^litellm$'" \
    "Running" \
    "Not running"

check_service "PostgreSQL Container" \
    "docker ps --format 'table {{.Names}}' | grep -q '^litellm-postgres$'" \
    "Running" \
    "Not running"

if [ "$QUIET" = false ]; then
    echo ""
    echo -e "${BLUE}API Endpoints:${NC}"
fi

# 2. Check LiteLLM API
check_service "LiteLLM Readiness" \
    "curl -sf $BASE/health/readiness" \
    "Ready" \
    "Not ready"

check_service "LiteLLM Models API" \
    "curl -sf -H 'Authorization: Bearer $KEY' $BASE/v1/models | jq -e '.data | length > 0'" \
    "Models available" \
    "No models"

# 3. Check PostgreSQL via Docker
if [ "$QUIET" = false ]; then
    echo ""
    echo -e "${BLUE}Database:${NC}"
fi

check_service "PostgreSQL Connection" \
    "docker exec litellm-postgres psql -U litellm -d litellm -c 'SELECT 1' 2>/dev/null | grep -q '1 row'" \
    "Connected" \
    "Connection failed"

# Get record count if verbose
if [ "$VERBOSE" = true ] && [ "$QUIET" = false ]; then
    RECORD_COUNT=$(docker exec litellm-postgres psql -U litellm -d litellm -t -c \
        "SELECT COUNT(*) FROM \"LiteLLM_SpendLogs\"" 2>/dev/null | tr -d ' ' || echo "0")
    if [ -n "$RECORD_COUNT" ] && [ "$RECORD_COUNT" != "0" ]; then
        echo "  Spend logs: $RECORD_COUNT records"
    fi
fi

# 4. Check Ollama instances
if [ "$QUIET" = false ]; then
    echo ""
    echo -e "${BLUE}Ollama Instances:${NC}"
fi

check_service "Ollama (localhost)" \
    "curl -sf http://localhost:11434/api/tags" \
    "Available" \
    "Not accessible"

check_service "Ollama (big72.local)" \
    "curl -sf http://big72.local:11434/api/tags" \
    "Available" \
    "Not accessible"

# 5. Test actual model availability
if [ "$QUIET" = false ]; then
    echo ""
    echo -e "${BLUE}Model Availability:${NC}"
fi

# Check specific models if verbose
if [ "$VERBOSE" = true ] && [ "$QUIET" = false ]; then
    MODELS=$(curl -sf -H "Authorization: Bearer $KEY" "$BASE/v1/models" 2>/dev/null | \
        jq -r '.data[].id' 2>/dev/null | head -10)
    
    if [ -n "$MODELS" ]; then
        echo "  Available models:"
        echo "$MODELS" | while read -r model; do
            echo "    • $model"
        done
    fi
fi

MODEL_COUNT=$(curl -sf -H "Authorization: Bearer $KEY" "$BASE/v1/models" 2>/dev/null | \
    jq -r '.data | length' 2>/dev/null || echo "0")

check_service "Model Count" \
    "[ $MODEL_COUNT -gt 0 ]" \
    "$MODEL_COUNT models" \
    "No models available"

# 6. Test API with a simple request
if [ "$VERBOSE" = true ] && [ "$QUIET" = false ]; then
    echo ""
    echo -e "${BLUE}API Test:${NC}"
    
    check_service "Test Request" \
        "curl -sf -X POST $BASE/v1/chat/completions \
            -H 'Authorization: Bearer $KEY' \
            -H 'Content-Type: application/json' \
            -d '{\"model\":\"llama3.2:latest\",\"messages\":[{\"role\":\"user\",\"content\":\"Hi\"}],\"max_tokens\":5}' | \
            jq -e '.choices[0].message.content'" \
        "API responding" \
        "API not responding"
fi

# Summary
if [ "$QUIET" = false ]; then
    echo ""
    echo -e "${BLUE}═══════════════════════════════════════════════════════${NC}"
    
    if [ $OVERALL_STATUS -eq 0 ]; then
        echo -e "${GREEN}✓ All services healthy${NC}"
    else
        echo -e "${RED}✗ Some services are unhealthy${NC}"
        echo ""
        echo "Failed services:"
        echo -e "$FAILED_SERVICES"
        echo ""
        echo "Troubleshooting:"
        echo "  1. Start Docker: cd llm-gateway && docker compose up -d"
        echo "  2. Check logs: docker compose logs litellm"
        echo "  3. Verify Ollama: ollama list"
        echo "  4. Check network: ping big72.local"
    fi
    
    echo ""
    echo "Web UI: http://localhost:4000/ui (key: $KEY)"
fi

exit $OVERALL_STATUS