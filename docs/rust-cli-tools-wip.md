# Rust CLI Tools - Work In Progress

This document contains the Rust CLI tools code that is being deferred for later implementation.

## Overview

The Rust CLI tools were intended to provide:
- `llm-status` - Check LLM provider status
- `llm-costs` - Track and display costs
- `llm-history` - Query usage history
- `llm-stats` - Display statistics
- `llm-config` - Manage configuration

## Current State

The Rust implementation has compilation issues related to:
- SQLx database types and row mapping
- Missing trait implementations
- Incomplete client implementations

## Files to be Completed Later

The following Rust source files contain work-in-progress code:

### src/main.rs
Main entry point for the CLI tools.

### src/lib.rs
Library module exposing public API.

### src/models.rs
Data models for LLM providers and responses.

### src/config.rs
Configuration management for the tools.

### src/client.rs
HTTP clients for LiteLLM, Ollama, and llama.cpp.

### src/database.rs
PostgreSQL database integration using SQLx.

### src/bin/status.rs
Status checking binary (incomplete).

## Dependencies (Cargo.toml)

The project uses:
- tokio for async runtime
- reqwest for HTTP requests
- sqlx for PostgreSQL
- clap for CLI argument parsing
- serde for JSON serialization
- colored for terminal output
- comfy-table for formatted tables

## Next Steps

1. Fix SQLx row mapping issues
2. Complete trait implementations
3. Add proper error handling
4. Write comprehensive tests
5. Add documentation

## Temporary Workaround

For now, use the shell scripts in the `scripts/` directory:
- `show-recent-usage.sh` - Show recent LLM usage
- `llm-history.sh` - Query PostgreSQL directly
- `probe.sh` - Test LLM endpoints
- `test-gptel-setup.sh` - Verify gptel configuration

These provide similar functionality while the Rust implementation is being completed.