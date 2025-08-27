# Emacs MCP Server Architecture

## Overview
A Rust-based command-line application will implement a [Model Context Protocol (MCP) 2.0](https://spec.modelcontextprotocol.com/) server that exposes selected Emacs actions as callable tools.  An AI coding agent connects over MCP, discovers the available tools, and invokes them to manipulate the user's editor.

The server acts as an intermediary between an external AI agent and a running Emacs instance.  Initially the server is launched manually from the command line and communicates with Emacs using `emacsclient` in batch mode.

## Components
- **MCP Transport** – JSON-RPC over standard I/O following the MCP 2.0 specification.
- **Tool Registry** – Rust module describing each exposed action, its parameters and documentation.
- **Emacs Bridge** – Wrapper around `emacsclient` for evaluating elisp expressions and returning results.
- **CLI Runtime** – Starts the MCP event loop and wires transports, registry and bridge together.

## Request Flow
1. CLI server starts and advertises itself to the AI client as an MCP server.
2. The client performs the MCP handshake and retrieves the tool descriptions.
3. The client invokes a tool such as `dired` with parameters (e.g., directory path).
4. The server maps the invocation to an elisp snippet executed through `emacsclient`.
5. Emacs performs the action and the server returns the result or any error back to the client.

## Initial Tool Set
- `dired`: open a directory listing in Emacs.
- `open-file`: load a file into a buffer.
- `insert`: insert text at point in the current buffer.
- `split-window`: create horizontal or vertical window splits.

## Future Extensions
- Dynamic library exposing the same API for loading into Emacs as a module.
- Authentication and permissions for AI clients.
- Rich result objects (e.g., screenshots, structured buffer metadata).
