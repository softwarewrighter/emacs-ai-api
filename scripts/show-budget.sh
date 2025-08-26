#!/usr/bin/env bash
# Show budget status for LiteLLM providers, models, and virtual keys
# Usage: ./show-budget.sh [project-name|key|all]

set -euo pipefail

# Configuration
BASE="${BASE:-http://localhost:4000}"
ADMIN_KEY="${LITELLM_MASTER_KEY:?LITELLM_MASTER_KEY must be set}"

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
    printf "$%.2f" "$1"
}

# Function to calculate percentage
calc_percentage() {
    if (( $(echo "$2 == 0" | bc -l) )); then
        echo "0"
    else
        echo "scale=1; ($1 / $2) * 100" | bc -l
    fi
}

# Function to draw progress bar
progress_bar() {
    local percentage=$1
    local width=30
    local filled=$(echo "scale=0; ($percentage * $width) / 100" | bc -l | cut -d. -f1)
    local empty=$((width - filled))
    
    # Choose color based on percentage
    local color=$GREEN
    if (( $(echo "$percentage > 75" | bc -l) )); then
        color=$RED
    elif (( $(echo "$percentage > 50" | bc -l) )); then
        color=$YELLOW
    fi
    
    echo -ne "$color"
    printf '█%.0s' $(seq 1 $filled) 2>/dev/null || true
    echo -ne "$NC"
    printf '░%.0s' $(seq 1 $empty) 2>/dev/null || true
}

echo -e "${BLUE}LiteLLM Budget Status${NC}"
echo "===================="
echo ""

# Get provider budgets
print_info "Provider Budgets:"
echo ""

PROVIDER_BUDGETS=$(curl -sf "$BASE/provider/budgets" \
    -H "Authorization: Bearer $ADMIN_KEY" 2>&1) || {
    print_warning "Unable to fetch provider budgets"
    PROVIDER_BUDGETS="{}"
}

if [ "$PROVIDER_BUDGETS" != "{}" ] && [ -n "$PROVIDER_BUDGETS" ]; then
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

# Get global spend
print_info "Global Spend (Last 30 days):"
echo ""

GLOBAL_SPEND=$(curl -sf "$BASE/global/spend" \
    -H "Authorization: Bearer $ADMIN_KEY" 2>&1) || {
    print_warning "Unable to fetch global spend"
    GLOBAL_SPEND="{}"
}

if [ "$GLOBAL_SPEND" != "{}" ] && [ -n "$GLOBAL_SPEND" ]; then
    TOTAL_SPEND=$(echo "$GLOBAL_SPEND" | jq -r '.total_spend // 0')
    echo "  Total: $(format_currency $TOTAL_SPEND)"
    
    # Show top spending models
    echo ""
    echo "  Top Models by Spend:"
    echo "$GLOBAL_SPEND" | jq -r '.model_spend // {} | to_entries | 
        sort_by(-.value) | .[0:5] | .[] | 
        "    \(.key): $\(.value)"' 2>/dev/null || echo "    No model spend data"
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
            -H "Authorization: Bearer $LITELLM_VIRTUAL_KEY" 2>&1) || {
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
if [ "$FILTER" = "all" ]; then
    print_info "Virtual Keys:"
    echo ""
    
    if [ -d "keys" ] && [ "$(ls -A keys 2>/dev/null)" ]; then
        for key_file in keys/*.env; do
            if [ -f "$key_file" ]; then
                PROJECT=$(basename "$key_file" .env)
                echo "  • $PROJECT"
                
                # Try to get budget info if key exists
                if [ -f "$key_file" ]; then
                    source "$key_file"
                    if [ -n "${LITELLM_VIRTUAL_KEY:-}" ]; then
                        KEY_SPEND=$(curl -sf "$BASE/key/spend" \
                            -H "Authorization: Bearer $LITELLM_VIRTUAL_KEY" \
                            -H "Content-Type: application/json" \
                            -d "{\"key\":\"$LITELLM_VIRTUAL_KEY\"}" 2>/dev/null) || KEY_SPEND="{}"
                        
                        if [ "$KEY_SPEND" != "{}" ]; then
                            SPEND=$(echo "$KEY_SPEND" | jq -r '.spend // 0')
                            echo "    Spend: $(format_currency $SPEND)"
                        fi
                    fi
                fi
            fi
        done
    else
        echo "  No virtual keys found"
        echo "  Create one with: ./gen-virtual-key.sh"
    fi
    echo ""
fi

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
    echo "  Model Aliases:"
    echo "$MODELS" | jq -r '.data[].id' | grep -E '^coding-' | sort -u | while read -r alias; do
        echo "    • $alias"
    done
    
    echo ""
    echo "  Cloud Models:"
    echo "$MODELS" | jq -r '.data[].id' | grep -E '^(gpt|claude|gemini|deepseek)' | sort -u | head -5 | while read -r model; do
        echo "    • $model"
    done
    
    echo ""
    echo "  Local Models:"
    echo "$MODELS" | jq -r '.data[].id' | grep -E '^(local|ollama)' | sort -u | while read -r model; do
        echo "    • $model"
    done
fi

echo ""
print_success "Budget check complete"