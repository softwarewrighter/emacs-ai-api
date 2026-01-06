//! Tool Definitions and Executor

use super::emacs::EmacsClient;
use super::mcp::{McpError, Tool};
use serde_json::Value;

pub struct DiredTool;

impl Tool for DiredTool {
    fn name(&self) -> &str {
        "dired"
    }

    fn description(&self) -> &str {
        "Open a directory listing in Emacs using dired"
    }

    fn input_schema(&self) -> serde_json::Value {
        serde_json::json!({
            "type": "object",
            "properties": {
                "path": {
                    "type": "string",
                    "description": "Directory path to open in dired"
                }
            },
            "required": ["path"]
        })
    }
}

pub struct OpenFileTool;

impl Tool for OpenFileTool {
    fn name(&self) -> &str {
        "open-file"
    }

    fn description(&self) -> &str {
        "Open a file in an Emacs buffer"
    }

    fn input_schema(&self) -> serde_json::Value {
        serde_json::json!({
            "type": "object",
            "properties": {
                "path": {
                    "type": "string",
                    "description": "File path to open"
                }
            },
            "required": ["path"]
        })
    }
}

pub struct InsertTool;

impl Tool for InsertTool {
    fn name(&self) -> &str {
        "insert"
    }

    fn description(&self) -> &str {
        "Insert text at point in the current buffer"
    }

    fn input_schema(&self) -> serde_json::Value {
        serde_json::json!({
            "type": "object",
            "properties": {
                "text": {
                    "type": "string",
                    "description": "Text to insert"
                }
            },
            "required": ["text"]
        })
    }
}

pub struct SplitWindowTool;

impl Tool for SplitWindowTool {
    fn name(&self) -> &str {
        "split-window"
    }

    fn description(&self) -> &str {
        "Split the current window (horizontal or vertical)"
    }

    fn input_schema(&self) -> serde_json::Value {
        serde_json::json!({
            "type": "object",
            "properties": {
                "direction": {
                    "type": "string",
                    "enum": ["horizontal", "vertical"],
                    "description": "Split direction"
                }
            }
        })
    }
}

pub fn tool_registry() -> Vec<Value> {
    let tools: Vec<Box<dyn Tool>> = vec![
        Box::new(DiredTool),
        Box::new(OpenFileTool),
        Box::new(InsertTool),
        Box::new(SplitWindowTool),
    ];

    tools
        .into_iter()
        .map(|tool| {
            serde_json::json!({
                "name": tool.name(),
                "description": tool.description(),
                "inputSchema": tool.input_schema()
            })
        })
        .collect()
}

pub struct ToolExecutor {
    client: EmacsClient,
}

impl ToolExecutor {
    pub fn new() -> Self {
        Self {
            client: EmacsClient::new(),
        }
    }

    pub async fn execute(&self, name: &str, args: Option<&Value>) -> Result<String, McpError> {
        match name {
            "dired" => self.execute_dired(args).await,
            "open-file" => self.execute_open_file(args).await,
            "insert" => self.execute_insert(args).await,
            "split-window" => self.execute_split_window(args).await,
            _ => Err(McpError::EmacsError(format!("Unknown tool: {name}"))),
        }
    }

    async fn execute_dired(&self, args: Option<&Value>) -> Result<String, McpError> {
        let path = args
            .and_then(|v| v.get("path"))
            .and_then(|v| v.as_str())
            .ok_or_else(|| McpError::EmacsError("Missing required parameter: path".to_string()))?;

        let escaped_path = path.replace("\\", "\\\\").replace("\"", "\\\"");
        let expr = format!("(dired \"{}\")", escaped_path);

        self.client
            .execute(&expr)
            .await
            .map(|_| format!("Opened dired: {path}"))
    }

    async fn execute_open_file(&self, args: Option<&Value>) -> Result<String, McpError> {
        let path = args
            .and_then(|v| v.get("path"))
            .and_then(|v| v.as_str())
            .ok_or_else(|| McpError::EmacsError("Missing required parameter: path".to_string()))?;

        let escaped_path = path.replace("\\", "\\\\").replace("\"", "\\\"");
        let expr = format!("(find-file \"{}\")", escaped_path);

        self.client
            .execute(&expr)
            .await
            .map(|_| format!("Opened file: {path}"))
    }

    async fn execute_insert(&self, args: Option<&Value>) -> Result<String, McpError> {
        let text = args
            .and_then(|v| v.get("text"))
            .and_then(|v| v.as_str())
            .ok_or_else(|| McpError::EmacsError("Missing required parameter: text".to_string()))?;

        let escaped_text = text.replace("\\", "\\\\").replace("\"", "\\\"");
        let expr = format!("(insert \"{}\")", escaped_text);

        self.client
            .execute(&expr)
            .await
            .map(|_| format!("Inserted {len} characters", len = text.len()))
    }

    async fn execute_split_window(&self, args: Option<&Value>) -> Result<String, McpError> {
        let direction = args
            .and_then(|v| v.get("direction"))
            .and_then(|v| v.as_str())
            .unwrap_or("horizontal");

        let expr = match direction {
            "vertical" => "(split-window-right)",
            _ => "(split-window-below)",
        };

        self.client
            .execute(expr)
            .await
            .map(|_| format!("Split window: {direction}"))
    }
}

impl Default for ToolExecutor {
    fn default() -> Self {
        Self::new()
    }
}
