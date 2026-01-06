//! Emacs Client Bridge

use super::mcp::McpError;
use tokio::process::Command;

pub struct EmacsClient {
    server_socket: Option<String>,
}

impl EmacsClient {
    pub fn new() -> Self {
        Self {
            server_socket: std::env::var("EMACS_SERVER_SOCKET").ok(),
        }
    }

    pub async fn evaluate(&self, expression: &str) -> Result<String, McpError> {
        let mut cmd = Command::new("emacsclient");

        if let Some(socket) = &self.server_socket {
            cmd.arg("-s").arg(socket);
        }

        cmd.arg("--eval").arg(expression);

        let output = cmd
            .output()
            .await
            .map_err(|e| McpError::EmacsError(format!("Failed to execute emacsclient: {e}")))?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(McpError::EmacsError(format!(
                "emacsclient failed: {stderr}"
            )));
        }

        let stdout = String::from_utf8_lossy(&output.stdout);
        Ok(stdout.trim().to_string())
    }

    pub async fn execute(&self, expression: &str) -> Result<String, McpError> {
        let wrapped = format!("(progn {expression})");
        self.evaluate(&wrapped).await
    }
}

impl Default for EmacsClient {
    fn default() -> Self {
        Self::new()
    }
}
