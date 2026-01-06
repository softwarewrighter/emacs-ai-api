//! Emacs MCP Server
//!
//! A Rust-based MCP 2.0 server that exposes Emacs actions as tools for AI agents.

use std::io::{self, BufRead, BufReader, Write};

mod emacs;
mod mcp;
mod tools;

use mcp::{McpError, McpRequest, McpResponse};
use tools::{ToolExecutor, tool_registry};

#[tokio::main]
async fn main() -> Result<(), McpError> {
    let stdin = io::stdin();
    let stdout = io::stdout();
    let mut reader = BufReader::new(stdin);
    let mut writer = stdout.lock();

    let mut executor = ToolExecutor::new();

    loop {
        let mut line = String::new();
        reader
            .read_line(&mut line)
            .map_err(|e| McpError::IoError(e.to_string()))?;

        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }

        let request: McpRequest =
            serde_json::from_str(trimmed).map_err(|e| McpError::ParseError(e.to_string()))?;

        let response = handle_request(request, &mut executor).await;

        let response_json = serde_json::to_string(&response)
            .map_err(|e| McpError::SerializeError(e.to_string()))?;

        writeln!(writer, "{}", response_json).map_err(|e| McpError::IoError(e.to_string()))?;
    }
}

async fn handle_request(request: McpRequest, executor: &mut ToolExecutor) -> McpResponse {
    match request.method.as_str() {
        "initialize" => handle_initialize(),
        "tools/list" => handle_tools_list(),
        "tools/call" => handle_tools_call(request, executor).await,
        "shutdown" => handle_shutdown(),
        _ => McpResponse::error(-32601, "Method not found", None),
    }
}

fn handle_initialize() -> McpResponse {
    McpResponse::success(
        serde_json::json!({
            "protocolVersion": "2024-11-05",
            "capabilities": {
                "tools": {}
            },
            "serverInfo": {
                "name": "emacs-mcp-server",
                "version": "0.1.0"
            }
        }),
        None,
    )
}

fn handle_tools_list() -> McpResponse {
    let tools = tool_registry();
    McpResponse::success(serde_json::json!({ "tools": tools }), None)
}

async fn handle_tools_call(request: McpRequest, executor: &mut ToolExecutor) -> McpResponse {
    let tool_name = request
        .params
        .get("name")
        .and_then(|v| v.as_str())
        .unwrap_or("");

    let arguments = request.params.get("arguments");

    match executor.execute(tool_name, arguments).await {
        Ok(result) => McpResponse::success(
            serde_json::json!({
                "content": [{
                    "type": "text",
                    "text": result
                }]
            }),
            request.id,
        ),
        Err(e) => McpResponse::error(-32603, &format!("Tool execution failed: {e}"), request.id),
    }
}

fn handle_shutdown() -> McpResponse {
    std::process::exit(0);
}
