#!/bin/bash

set -e

echo "Building Rust CLI tools..."

# Build llm-tools (main CLI utilities)
cd rust-wip
cargo build --release
cd ..

echo "Building emacs-mcp-server..."
cd emacs-mcp-server
cargo build --release
cd ..

echo "Rust tools built successfully!"
echo ""
echo "Available binaries:"
echo "  rust-wip/target/release/llm-status    - Check LLM provider status"
echo "  rust-wip/target/release/llm-history   - View API usage history"
echo "  rust-wip/target/release/llm-costs     - View cost summaries"
echo "  rust-wip/target/release/llm-stats     - View usage statistics"
echo "  rust-wip/target/release/llm-config    - Manage configuration"
echo "  emacs-mcp-server/target/release/mcp-server - MCP server for Emacs"
echo ""
echo "To install to PATH:"
echo "  sudo ln -s \$PWD/rust-wip/target/release/llm-* /usr/local/bin/"
echo "  sudo ln -s \$PWD/emacs-mcp-server/target/release/mcp-server /usr/local/bin/"
