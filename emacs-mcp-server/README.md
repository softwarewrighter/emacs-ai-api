# Emacs MCP Server

A Rust-based MCP 2.0 server that exposes Emacs actions as tools for AI coding agents. This enables external AI assistants to interact with a running Emacs session through a command-line interface.

## Features

- **MCP 2.0 Compatible**: Implements the Model Context Protocol 2.0 specification
- **JSON-RPC Transport**: Communication over standard I/O using JSON-RPC
- **Emacs Integration**: Executes elisp commands via `emacsclient`
- **Four Core Tools**: dired, open-file, insert, split-window
- **Async Architecture**: Built with Tokio for efficient async execution
- **Graceful Error Handling**: Detailed error messages for troubleshooting

## Quick Start

### Prerequisites

1. **Rust** (1.89.0 or later)
2. **Emacs** with server mode enabled
3. **emacsclient** in your PATH

### Building

```bash
cd emacs-mcp-server
cargo build --release
```

### Running the Server

The server communicates via standard I/O. Start it and connect via an MCP client:

```bash
cargo run --release
```

The server will wait for JSON-RPC requests on stdin and send responses on stdout.

### Quick Example

Try a simple example to see it in action:

```bash
# 1. Start Emacs in server mode (if not already running)
emacs --daemon

# 2. Test opening a file
echo '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"open-file","arguments":{"path":"README.md"}},"id":1}' | ./target/release/emacs-mcp-server

# Check your Emacs - the README.md file should now be open!
```

## Practical Examples

The `examples/` directory contains real-world use cases:

- **Code Review Assistant**: Insert AI-generated review comments
- **Log Analyzer**: Web interface for log file analysis
- **Remote Pair Programming**: Collaborative editing via web UI
- **Documentation Generator**: Auto-generate code documentation

See [examples/README.md](examples/README.md) for detailed instructions.

## Available Tools

### dired

Open a directory listing in Emacs using dired.

**Parameters:**
- `path` (string, required): Directory path to open in dired

**Example Request:**
```json
{
  "jsonrpc": "2.0",
  "method": "tools/call",
  "params": {
    "name": "dired",
    "arguments": {
      "path": "/home/user/projects"
    }
  },
  "id": 1
}
```

### open-file

Open a file in an Emacs buffer.

**Parameters:**
- `path` (string, required): File path to open

**Example Request:**
```json
{
  "jsonrpc": "2.0",
  "method": "tools/call",
  "params": {
    "name": "open-file",
    "arguments": {
      "path": "/home/user/projects/main.rs"
    }
  },
  "id": 2
}
```

### insert

Insert text at point in the current buffer.

**Parameters:**
- `text` (string, required): Text to insert

**Example Request:**
```json
{
  "jsonrpc": "2.0",
  "method": "tools/call",
  "params": {
    "name": "insert",
    "arguments": {
      "text": "fn hello() { println!(\"world\"); }"
    }
  },
  "id": 3
}
```

### split-window

Split the current window horizontally or vertically.

**Parameters:**
- `direction` (string, optional): "horizontal" (default) or "vertical"

**Example Request:**
```json
{
  "jsonrpc": "2.0",
  "method": "tools/call",
  "params": {
    "name": "split-window",
    "arguments": {
      "direction": "vertical"
    }
  },
  "id": 4
}
```

## Protocol Flow

### 1. Initialize

```json
{
  "jsonrpc": "2.0",
  "method": "initialize",
  "params": {},
  "id": 1
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "result": {
    "protocolVersion": "2024-11-05",
    "capabilities": {
      "tools": {}
    },
    "serverInfo": {
      "name": "emacs-mcp-server",
      "version": "0.1.0"
    }
  },
  "id": 1
}
```

### 2. List Tools

```json
{
  "jsonrpc": "2.0",
  "method": "tools/list",
  "params": {},
  "id": 2
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "result": {
    "tools": [
      {
        "name": "dired",
        "description": "Open a directory listing in Emacs using dired",
        "inputSchema": {
          "type": "object",
          "properties": {
            "path": {
              "type": "string",
              "description": "Directory path to open in dired"
            }
          },
          "required": ["path"]
        }
      }
    ]
  },
  "id": 2
}
```

### 3. Call Tool

```json
{
  "jsonrpc": "2.0",
  "method": "tools/call",
  "params": {
    "name": "dired",
    "arguments": {
      "path": "/home/user/projects"
    }
  },
  "id": 3
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "result": {
    "content": [
      {
        "type": "text",
        "text": "Opened dired: /home/user/projects"
      }
    ]
  },
  "id": 3
}
```

### 4. Shutdown

```json
{
  "jsonrpc": "2.0",
  "method": "shutdown",
  "params": {},
  "id": 4
}
```

This will terminate the server process.

## Configuration

### Emacs Server Socket

The server uses the `EMACS_SERVER_SOCKET` environment variable to specify the Emacs server socket. If not set, `emacsclient` will use the default server.

```bash
export EMACS_SERVER_SOCKET=/tmp/emacs1000/server
cargo run --release
```

### Starting Emacs Server Mode

Ensure Emacs is running in server mode:

```bash
emacs --daemon
```

Or in your Emacs configuration:

```elisp
(server-start)
```

## Development

### Project Structure

```
emacs-mcp-server/
  src/
    main.rs    # Entry point and MCP request handler
    mcp.rs     # MCP protocol types and traits
    emacs.rs   # Emacs client bridge
    tools.rs   # Tool definitions and executor
```

### Building and Testing

```bash
# Check code
cargo check

# Run clippy (strict mode)
cargo clippy --all-targets --all-features -- -D warnings

# Format code
cargo fmt

# Run tests
cargo test

# Build release binary
cargo build --release
```

### Code Style

The project follows Rust best practices:
- Rust 2024 edition
- Inline format arguments: `format!("{var}")`
- No unused imports
- All clippy warnings treated as errors
- Module docs with `//!`, item docs with `///`

## Error Handling

The server provides detailed error messages:

- **IO Errors**: Issues with stdin/stdout communication
- **Parse Errors**: Invalid JSON-RPC requests
- **Serialize Errors**: Issues creating JSON responses
- **Emacs Errors**: Failures executing elisp via emacsclient

Error responses follow the JSON-RPC error format:

```json
{
  "jsonrpc": "2.0",
  "error": {
    "code": -32603,
    "message": "Tool execution failed: Missing required parameter: path"
  },
  "id": 3
}
```

## Limitations

- Requires a running Emacs server
- No authentication or authorization
- No session state management
- Synchronous tool execution (blocks until Emacs responds)
- No support for concurrent tool invocations

## Future Enhancements

Based on the planning document:

### Phase 2
- Buffer manipulation tools (save, close, switch)
- Window management helpers
- Structured JSON results
- Logging and diagnostics

### Phase 3
- Dynamic library interface
- Background service with auth tokens
- Session state management

## Troubleshooting

### emacsclient not found

Ensure `emacsclient` is in your PATH:

```bash
which emacsclient
```

### Connection refused

Ensure Emacs server is running:

```bash
ps aux | grep emacs
```

Start the server:

```bash
emacs --daemon
```

### Tool execution fails

Check Emacs error messages by running the elisp directly:

```bash
emacsclient --eval '(dired "/tmp")'
```

## Contributing

This project is part of the larger Emacs AI API ecosystem. Contributions should follow the established patterns and pass all linting and formatting checks.

## License

MIT

## See Also

- [MCP 2.0 Specification](https://spec.modelcontextprotocol.com/)
- [Project Architecture](../docs/emacs-mcp-server/architecture.md)
- [Product Requirements](../docs/emacs-mcp-server/prd.md)
- [Planning Document](../docs/emacs-mcp-server/planning.md)
