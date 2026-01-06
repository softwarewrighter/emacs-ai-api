use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use sqlx::FromRow;
use std::collections::HashMap;

#[derive(Debug, Clone, Serialize, Deserialize, FromRow)]
pub struct ApiUsage {
    pub id: Option<i64>,
    pub timestamp: DateTime<Utc>,
    pub provider: String,
    pub model: String,
    pub prompt_tokens: i32,
    pub completion_tokens: i32,
    pub total_tokens: i32,
    pub cost: f64,
    pub project: Option<String>,
    pub request_id: Option<String>,
    pub latency_ms: Option<i64>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModelInfo {
    pub id: String,
    pub provider: String,
    pub input_cost_per_token: f64,
    pub output_cost_per_token: f64,
    pub max_tokens: Option<i32>,
    pub context_window: Option<i32>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProviderStatus {
    pub name: String,
    pub endpoint: String,
    pub is_online: bool,
    pub latency_ms: Option<u64>,
    pub models: Vec<String>,
    pub last_checked: DateTime<Utc>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BudgetInfo {
    pub provider: Option<String>,
    pub model: Option<String>,
    pub project: Option<String>,
    pub limit: f64,
    pub used: f64,
    pub period: String,
    pub reset_at: DateTime<Utc>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CostSummary {
    pub period: String,
    pub total_cost: f64,
    pub total_tokens: i64,
    pub request_count: i32,
    pub by_provider: HashMap<String, f64>,
    pub by_model: HashMap<String, f64>,
    pub by_project: HashMap<String, f64>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LiteLLMConfig {
    pub base_url: String,
    pub master_key: Option<String>,
    pub virtual_keys: HashMap<String, VirtualKey>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VirtualKey {
    pub key: String,
    pub project: String,
    pub models: Vec<String>,
    pub budget: Option<f64>,
    pub budget_duration: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChatCompletionRequest {
    pub model: String,
    pub messages: Vec<Message>,
    pub temperature: Option<f64>,
    pub max_tokens: Option<i32>,
    pub stream: Option<bool>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Message {
    pub role: String,
    pub content: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChatCompletionResponse {
    pub id: String,
    pub model: String,
    pub choices: Vec<Choice>,
    pub usage: Option<Usage>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Choice {
    pub index: i32,
    pub message: Message,
    pub finish_reason: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Usage {
    pub prompt_tokens: i32,
    pub completion_tokens: i32,
    pub total_tokens: i32,
}
