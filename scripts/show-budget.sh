#!/usr/bin/env bash
# Show budget status for LiteLLM providers, models, and virtual keys

set -euo pipefail

# Help function
show_help() {
    cat << EOF
PURPOSE:
    Display budget status and usage statistics for LiteLLM.
    Shows provider budgets, global spend, virtual keys, and model availability.

USAGE:
    $0 [OPTIONS] [FILTER]
    $0 -h | --help

ARGUMENTS:
    FILTER        Optional: 'all' (default), project-name, or key name

OPTIONS:
    -h, --help    Show this help message

ENVIRONMENT:
    LITELLM_MASTER_KEY    Admin key for LiteLLM (default: sk-local-test-key-123)
    BASE                  API base URL (default: http://localhost:4000)

EXAMPLES:
    # Show all budget information
    $0

    # Show budget for specific project
    $0 my-project

    # Show only virtual keys
    $0 all

REQUIREMENTS:
    - LiteLLM running at localhost:4000
    - Docker containers up (cd llm-gateway && docker compose up -d)
    - bc command for calculations

OUTPUT:
    - Provider budgets with usage bars
    - Global spend statistics
    - Virtual key details
    - Model availability by category

EOF
    exit 0
}

# Check for help flag
if [[ "${1:-}" == "-h" ]] || [[ "${1:-}" == "--help" ]]; then
    show_help
fi

# Configuration with better defaults
BASE="${BASE:-http://localhost:4000}"
ADMIN_KEY="${LITELLM_MASTER_KEY:-sk-local-test-key-123}"

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
NC='\033[0m' # No Color

# Function to print colored output
print_info() { echo -e "${BLUE}ℹ ${NC}$1"; }
print_success() { echo -e "${GREEN}✓${NC} $1"; }
print_error() { echo -e "${RED}✗${NC} $1"; }
print_warning() { echo -e "${YELLOW}⚠${NC} $1"; }

# Parse arguments
FILTER="${1:-all}"

# Function to format currency
format_currency() {
    printf "$%.2f" "${1:-0}"
}

# Function to calculate percentage (without bc dependency)
calc_percentage() {
    local used="${1:-0}"
    local total="${2:-0}"
    if [ "$total" = "0" ] || [ -z "$total" ]; then
        echo "0"
    else
        # Use awk for floating point math instead of bc
        echo "$used $total" | awk '{printf "%.1f", ($1 / $2) * 100}'
    fi
}

# Function to draw progress bar
progress_bar() {
    local percentage="${1:-0}"
    local width=30
    
    # Convert percentage to integer for shell arithmetic
    local pct_int=$(echo "$percentage" | cut -d. -f1)
    pct_int="${pct_int:-0}"
    
    local filled=$((pct_int * width / 100))
    local empty=$((width - filled))
    
    # Choose color based on percentage
    local color=$GREEN
    if [ "$pct_int" -gt 75 ]; then
        color=$RED
    elif [ "$pct_int" -gt 50 ]; then
        color=$YELLOW
    fi
    
    echo -ne "$color"
    for ((i=0; i<filled; i++)); do echo -n "█"; done
    echo -ne "$NC"
    for ((i=0; i<empty; i++)); do echo -n "░"; done
}

echo -e "${BLUE}LiteLLM Budget Status${NC}"
echo "===================="
echo ""

# Check if LiteLLM is running (use readiness endpoint which doesn't require auth)
if ! curl -sf "$BASE/health/readiness" >/dev/null 2>&1; then
    print_warning "LiteLLM not accessible at $BASE"
    echo "  Start with: cd llm-gateway && docker compose up -d"
    exit 1
fi

# Get provider budgets
print_info "Provider Budgets:"
echo ""

PROVIDER_BUDGETS=$(curl -sf "$BASE/provider/budgets" \
    -H "Authorization: Bearer $ADMIN_KEY" 2>&1) || {
    print_warning "Unable to fetch provider budgets"
    PROVIDER_BUDGETS="{}"
}

if [ "$PROVIDER_BUDGETS" != "{}" ] && [ -n "$PROVIDER_BUDGETS" ] && echo "$PROVIDER_BUDGETS" | jq -e '.[0]' >/dev/null 2>&1; then
    echo "$PROVIDER_BUDGETS" | jq -r '.[] | 
        "\(.provider_name // "Unknown")|\(.budget_limit // 0)|\(.budget_used // 0)|\(.budget_duration // "N/A")"' | \
    while IFS='|' read -r provider limit used duration; do
        percentage=$(calc_percentage "$used" "$limit")
        printf "  %-15s " "$provider:"
        progress_bar "$percentage"
        printf " %5.1f%% " "$percentage"
        printf "($(format_currency $used) / $(format_currency $limit) per %s)\n" "$duration"
    done
else
    echo "  No provider budgets configured"
fi
echo ""

# Get global spend from database
print_info "Global Spend (Last 30 days):"
echo ""

# Try to get spend from PostgreSQL via Docker
if docker ps --format "table {{.Names}}" | grep -q "litellm-postgres"; then
    TOTAL_SPEND=$(docker exec litellm-postgres psql -U litellm -d litellm -t -c "
        SELECT COALESCE(SUM(spend), 0)::numeric(10,2) 
        FROM \"LiteLLM_SpendLogs\" 
        WHERE \"startTime\" > NOW() - INTERVAL '30 days';" 2>/dev/null | tr -d ' ')
    
    if [ -n "$TOTAL_SPEND" ]; then
        echo "  Total: $(format_currency $TOTAL_SPEND)"
        
        # Show top spending models
        echo ""
        echo "  Top Models by Spend:"
        docker exec litellm-postgres psql -U litellm -d litellm -t --no-align -F'|' -c "
            SELECT model, SUM(spend)::numeric(10,4) as total_spend
            FROM \"LiteLLM_SpendLogs\"
            WHERE \"startTime\" > NOW() - INTERVAL '30 days'
            GROUP BY model
            ORDER BY total_spend DESC
            LIMIT 5;" 2>/dev/null | while IFS='|' read -r model spend; do
            if [ -n "$model" ]; then
                printf "    • %-20s $(format_currency ${spend:-0})\n" "$model:"
            fi
        done
    else
        echo "  Total: $0.00"
    fi
else
    # Fallback to API
    GLOBAL_SPEND=$(curl -sf "$BASE/global/spend" \
        -H "Authorization: Bearer $ADMIN_KEY" 2>&1) || GLOBAL_SPEND="{}"
    
    if [ "$GLOBAL_SPEND" != "{}" ] && [ -n "$GLOBAL_SPEND" ]; then
        TOTAL_SPEND=$(echo "$GLOBAL_SPEND" | jq -r '.total_spend // 0')
        echo "  Total: $(format_currency $TOTAL_SPEND)"
    else
        echo "  Total: $0.00"
    fi
fi
echo ""

# If specific project/key requested
if [ "$FILTER" != "all" ]; then
    # Check if it's a key file
    KEY_FILE="keys/${FILTER}.env"
    if [ -f "$KEY_FILE" ]; then
        print_info "Project Key: $FILTER"
        source "$KEY_FILE"
        
        # Get key info
        KEY_INFO=$(curl -sf "$BASE/key/info" \
            -H "Authorization: Bearer ${LITELLM_VIRTUAL_KEY:-}" 2>&1) || {
            print_warning "Unable to fetch key info for $FILTER"
            KEY_INFO="{}"
        }
        
        if [ "$KEY_INFO" != "{}" ]; then
            echo "$KEY_INFO" | jq -r '
                "  Key: \(.key // "N/A")",
                "  Budget: $\(.max_budget // 0) per \(.budget_duration // "N/A")",
                "  Used: $\(.budget_used // 0)",
                "  Remaining: $\((.max_budget // 0) - (.budget_used // 0))",
                "  Models: \(.models // [] | join(", "))"'
            
            # Show usage percentage
            BUDGET=$(echo "$KEY_INFO" | jq -r '.max_budget // 0')
            USED=$(echo "$KEY_INFO" | jq -r '.budget_used // 0')
            if [ "$BUDGET" != "0" ]; then
                PERCENTAGE=$(calc_percentage "$USED" "$BUDGET")
                echo -n "  Usage: "
                progress_bar "$PERCENTAGE"
                printf " %.1f%%\n" "$PERCENTAGE"
            fi
        fi
    else
        print_warning "No key file found for project: $FILTER"
        echo "  Try: ./gen-virtual-key.sh $FILTER"
    fi
    echo ""
fi

# List all virtual keys
print_info "Virtual Keys:"
echo ""

if [ -d "keys" ] && [ "$(ls -A keys 2>/dev/null)" ]; then
    for key_file in keys/*.env; do
        if [ -f "$key_file" ]; then
            PROJECT=$(basename "$key_file" .env)
            echo "  • $PROJECT"
            
            # Try to get budget info if key exists
            source "$key_file"
            if [ -n "${LITELLM_VIRTUAL_KEY:-}" ]; then
                KEY_SPEND=$(curl -sf "$BASE/key/spend" \
                    -H "Authorization: Bearer $LITELLM_VIRTUAL_KEY" \
                    -H "Content-Type: application/json" \
                    -d "{\"key\":\"$LITELLM_VIRTUAL_KEY\"}" 2>/dev/null) || KEY_SPEND="{}"
                
                if [ "$KEY_SPEND" != "{}" ] && [ "$KEY_SPEND" != "null" ]; then
                    SPEND=$(echo "$KEY_SPEND" | jq -r '.spend // 0')
                    echo "    Spend: $(format_currency $SPEND)"
                fi
            fi
        fi
    done
else
    echo "  No virtual keys found"
    echo "  Create one with: ./gen-virtual-key.sh"
fi
echo ""

# Show model availability
print_info "Model Availability:"
echo ""

MODELS=$(curl -sf "$BASE/v1/models" \
    -H "Authorization: Bearer $ADMIN_KEY" 2>&1) || {
    print_warning "Unable to fetch model list"
    MODELS='{"data":[]}'
}

MODEL_COUNT=$(echo "$MODELS" | jq -r '.data | length')
echo "  Total models available: $MODEL_COUNT"

if [ "$MODEL_COUNT" -gt 0 ]; then
    echo ""
    
    # Model Aliases (if any exist)
    ALIASES=$(echo "$MODELS" | jq -r '.data[].id' | grep -E '^(coding-|test-|custom-)' 2>/dev/null | sort -u)
    if [ -n "$ALIASES" ]; then
        echo "  Model Aliases:"
        echo "$ALIASES" | while read -r alias; do
            [ -n "$alias" ] && echo "    • $alias"
        done
        echo ""
    fi
    
    # Cloud Models
    CLOUD_MODELS=$(echo "$MODELS" | jq -r '.data[].id' | grep -E '^(gpt|claude|gemini|deepseek)' 2>/dev/null | sort -u | head -5)
    if [ -n "$CLOUD_MODELS" ]; then
        echo "  Cloud Models:"
        echo "$CLOUD_MODELS" | while read -r model; do
            [ -n "$model" ] && echo "    • $model"
        done
        echo ""
    fi
    
    # Local Models (Ollama)
    LOCAL_MODELS=$(echo "$MODELS" | jq -r '.data[].id' | grep -vE '^(gpt|claude|gemini|deepseek|coding-|test-|custom-)' 2>/dev/null | sort -u | head -5)
    if [ -n "$LOCAL_MODELS" ]; then
        echo "  Local Models (Ollama):"
        echo "$LOCAL_MODELS" | while read -r model; do
            [ -n "$model" ] && echo "    • $model"
        done
    fi
fi

echo ""
print_success "Budget check complete"