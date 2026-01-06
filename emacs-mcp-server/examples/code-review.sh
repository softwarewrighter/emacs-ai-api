#!/bin/bash
# Code Review Assistant - Insert review comments into code files

set -e

EMACS_MCP_SERVER="../target/release/emacs-mcp-server"

# Review a single file
review_file() {
    local file="$1"
    local review_text="$2"

    echo "Reviewing $file..."

    if [ ! -f "$file" ]; then
        echo "Error: File not found: $file"
        return 1
    fi

    # Get absolute path
    local abs_path=$(cd "$(dirname "$file")" && pwd)/$(basename "$file")

    # Open the file in Emacs
    echo "Opening $file..."
    local req='{"jsonrpc":"2.0","method":"tools/call","params":{"name":"open-file","arguments":{"path":"'"$abs_path"'"}},"id":1}'
    echo "$req" | "$EMACS_MCP_SERVER" > /dev/null

    # Insert review comment at beginning of file
    echo "Inserting review..."
    local escaped_review=$(echo "$review_text" | sed 's/\\/\\\\/g; s/"/\\"/g')
    local insert_req='{"jsonrpc":"2.0","method":"tools/call","params":{"name":"insert","arguments":{"text":";;; Code Review: '"$escaped_review"'\n\n"}},"id":2}'
    echo "$insert_req" | "$EMACS_MCP_SERVER" > /dev/null

    echo "Done: $file"
    echo "Check your Emacs window to see the review comment."
}

# Usage
if [ $# -eq 0 ]; then
    echo "Code Review Assistant"
    echo ""
    echo "Usage: $0 <file-path> [review-comment]"
    echo ""
    echo "Examples:"
    echo "  $0 src/main.rs \"Consider adding error handling here\""
    echo "  $0 myproject/config.yml \"Add documentation for configuration options\""
    echo ""
    echo "This tool opens the file in Emacs and inserts a review comment."
    exit 1
fi

FILE="$1"
REVIEW="${2:-Review completed. Please check for potential improvements.}"

if [ ! -f "$EMACS_MCP_SERVER" ]; then
    echo "Error: MCP server binary not found at $EMACS_MCP_SERVER"
    echo "Build with: cd .. && cargo build --release"
    exit 1
fi

review_file "$FILE" "$REVIEW"
