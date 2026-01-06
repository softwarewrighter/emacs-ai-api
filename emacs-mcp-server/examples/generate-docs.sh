#!/bin/bash
# Documentation Generator - Auto-generate documentation for code files

set -e

EMACS_MCP_SERVER="../target/release/emacs-mcp-server"

generate_docs_for_file() {
    local file="$1"
    local doc_text="$2"

    echo "Generating docs for $file..."

    if [ ! -f "$file" ]; then
        echo "Error: File not found: $file"
        return 1
    fi

    # Get absolute path
    local abs_path=$(cd "$(dirname "$file")" && pwd)/$(basename "$file")

    # Get file info
    local filename=$(basename "$file")
    local filesize=$(stat -f%z "$file" 2>/dev/null || stat -c%s "$file" 2>/dev/null || echo "unknown")

    # Open file
    local req='{"jsonrpc":"2.0","method":"tools/call","params":{"name":"open-file","arguments":{"path":"'"$abs_path"'"}},"id":1}'
    echo "$req" | "$EMACS_MCP_SERVER" > /dev/null

    # Insert documentation at beginning of file
    local escaped_doc=$(echo "$doc_text" | sed 's/\\/\\\\/g; s/"/\\"/g; s/$/\\n/g')
    local insert_req='{"jsonrpc":"2.0","method":"tools/call","params":{"name":"insert","arguments":{"text":"'"$escaped_doc"'"}},"id":2}'
    echo "$insert_req" | "$EMACS_MCP_SERVER" > /dev/null

    echo "Documentation added to $file"
}

# Generate documentation text
generate_doc_text() {
    local file="$1"
    local filename=$(basename "$file")

    cat <<EOF
;;; Auto-generated Documentation
;;; File: $filename
;;; Generated: $(date '+%Y-%m-%d %H:%M:%S')
;;;
;;; Purpose: Core module file
;;;
;;; TODO: Add detailed documentation describing:
;;;   - Module purpose and responsibilities
;;;   - Main functions and their interfaces
;;;   - Dependencies and external requirements
;;;   - Usage examples
;;;
EOF
}

# Usage
if [ $# -eq 0 ]; then
    echo "Documentation Generator"
    echo ""
    echo "Usage: $0 <file-path>"
    echo "       $0 <directory-path>"
    echo ""
    echo "Examples:"
    echo "  $0 src/main.rs"
    echo "  $0 myproject/src/"
    echo ""
    echo "This tool inserts documentation headers into code files."
    exit 1
fi

TARGET="$1"

if [ ! -f "$EMACS_MCP_SERVER" ]; then
    echo "Error: MCP server binary not found at $EMACS_MCP_SERVER"
    echo "Build with: cd .. && cargo build --release"
    exit 1
fi

if [ -d "$TARGET" ]; then
    # Process all files in directory
    for file in "$TARGET"/*; do
        if [ -f "$file" ]; then
            local doc_text=$(generate_doc_text "$file")
            generate_docs_for_file "$file" "$doc_text"
        fi
    done
elif [ -f "$TARGET" ]; then
    # Process single file
    local doc_text=$(generate_doc_text "$TARGET")
    generate_docs_for_file "$TARGET" "$doc_text"
else
    echo "Error: Not a file or directory: $TARGET"
    exit 1
fi

echo ""
echo "Documentation generation complete."
echo "Check your Emacs window to see the changes."
