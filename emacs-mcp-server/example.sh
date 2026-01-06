#!/bin/bash
# Example script to test the Emacs MCP Server

set -e

echo "=== Emacs MCP Server Example ==="
echo ""
echo "This script demonstrates how to interact with the MCP server."
echo "Make sure Emacs server is running: emacs --daemon"
echo ""

# Path to the server binary
SERVER="./target/release/emacs-mcp-server"

if [ ! -f "$SERVER" ]; then
    echo "Error: Server binary not found at $SERVER"
    echo "Run: cargo build --release"
    exit 1
fi

# Example 1: Initialize
echo "1. Initialize"
echo '{"jsonrpc":"2.0","method":"initialize","params":{},"id":1}' | "$SERVER"
echo ""

# Example 2: List tools
echo "2. List tools"
echo '{"jsonrpc":"2.0","method":"tools/list","params":{},"id":2}' | "$SERVER"
echo ""

# Example 3: Call dired tool
echo "3. Call dired tool (current directory)"
echo '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"dired","arguments":{"path":"'"$(pwd)"'"}},"id":3}' | "$SERVER"
echo ""

# Example 4: Call open-file tool
echo "4. Call open-file tool (README.md)"
echo '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"open-file","arguments":{"path":"'"$(pwd)"/README.md'"}},"id":4}' | "$SERVER"
echo ""

echo "=== Examples complete ==="
echo ""
echo "Note: These examples spawn a new server process for each request."
echo "In production, the server would stay running and handle multiple requests."
