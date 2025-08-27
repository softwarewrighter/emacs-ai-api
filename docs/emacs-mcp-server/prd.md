# Emacs MCP Server – Product Requirements

## Goal
Enable external AI coding agents to interact with a running Emacs session through a small Rust command-line application that implements the MCP 2.0 server protocol.

## User Stories
- As a developer running Emacs, I can start the `emacs-mcp-server` and allow an AI assistant to list available tools.
- As an AI assistant, I can invoke `dired` with a path to show directory contents in the user's Emacs instance.
- As a developer, I can extend the server with additional tools without modifying the MCP plumbing.

## Functional Requirements
1. **MCP Compatibility** – server follows the MCP 2.0 spec for registration, tool discovery and invocation.
2. **Tool Exposure** – at least four tools (`dired`, `open-file`, `insert`, `split-window`).
3. **Emacs Communication** – tools execute elisp via `emacsclient` and report success or errors.
4. **CLI Usage** – server runs as a standalone process started manually; termination by Ctrl-C.
5. **Documentation** – tool descriptions include parameter schemas and human-readable help.

## Non‑Functional Requirements
- Written in Rust using async execution and JSON-RPC libraries.
- Minimal dependencies; should compile with stable Rust.
- Graceful handling when Emacs is not running or returns an error.

## Out of Scope
- Dynamic library or native Emacs module interface.
- Network services or long‑running daemons.
- Authentication, authorization and multi-user support.

## Success Metrics
- CLI agent can discover tool list via MCP.
- Agent receives confirmation after invoking `dired`.
- Documentation clearly explains how to run the server.
