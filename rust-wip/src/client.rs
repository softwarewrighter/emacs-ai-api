use anyhow::{Context, Result};
use reqwest::{Client, Response};
use serde::Serialize;
use std::time::Duration;

use crate::models::*;

pub struct LiteLLMClient {
    client: Client,
    base_url: String,
    api_key: Option<String>,
}

impl LiteLLMClient {
    pub fn new(base_url: impl Into<String>, api_key: Option<String>) -> Result<Self> {
        let client = Client::builder()
            .timeout(Duration::from_secs(30))
            .build()
            .context("Failed to create HTTP client")?;

        Ok(Self {
            client,
            base_url: base_url.into(),
            api_key,
        })
    }

    pub async fn check_health(&self) -> Result<bool> {
        let url = format!("{}/health/readiness", self.base_url.trim_end_matches("/v1"));
        let response = self
            .client
            .get(&url)
            .send()
            .await
            .context("Failed to check health")?;

        Ok(response.status().is_success())
    }

    pub async fn list_models(&self) -> Result<Vec<String>> {
        let url = format!("{}/models", self.base_url);
        let response = self.get(&url).await?;

        #[derive(serde::Deserialize)]
        struct ModelsResponse {
            data: Vec<ModelData>,
        }

        #[derive(serde::Deserialize)]
        struct ModelData {
            id: String,
        }

        let models: ModelsResponse = response
            .json()
            .await
            .context("Failed to parse models response")?;

        Ok(models.data.into_iter().map(|m| m.id).collect())
    }

    pub async fn get_spend(&self, key: Option<&str>) -> Result<f64> {
        let url = format!("{}/spend/total", self.base_url.trim_end_matches("/v1"));
        let mut request = self.client.get(&url);

        if let Some(k) = key.or(self.api_key.as_deref()) {
            request = request.header("Authorization", format!("Bearer {}", k));
        }

        let response = request.send().await.context("Failed to get spend")?;

        #[derive(serde::Deserialize)]
        struct SpendResponse {
            total_spend: f64,
        }

        let spend: SpendResponse = response
            .json()
            .await
            .context("Failed to parse spend response")?;

        Ok(spend.total_spend)
    }

    pub async fn get_budget_info(&self) -> Result<Vec<BudgetInfo>> {
        let url = format!("{}/provider/budgets", self.base_url.trim_end_matches("/v1"));
        let response = self.get_auth(&url).await?;

        #[derive(serde::Deserialize)]
        struct BudgetResponse {
            provider_name: String,
            budget_limit: f64,
            budget_used: f64,
            budget_duration: String,
        }

        let budgets: Vec<BudgetResponse> = response
            .json()
            .await
            .context("Failed to parse budget response")?;

        Ok(budgets
            .into_iter()
            .map(|b| BudgetInfo {
                provider: Some(b.provider_name),
                model: None,
                project: None,
                limit: b.budget_limit,
                used: b.budget_used,
                period: b.budget_duration,
                reset_at: chrono::Utc::now(), // TODO: Get actual reset time
            })
            .collect())
    }

    pub async fn chat_completion(
        &self,
        request: ChatCompletionRequest,
    ) -> Result<ChatCompletionResponse> {
        let url = format!("{}/chat/completions", self.base_url);
        let response = self.post(&url, &request).await?;

        response
            .json()
            .await
            .context("Failed to parse chat completion response")
    }

    async fn get(&self, url: &str) -> Result<Response> {
        self.client
            .get(url)
            .send()
            .await
            .with_context(|| format!("Failed to GET {}", url))
    }

    async fn get_auth(&self, url: &str) -> Result<Response> {
        let mut request = self.client.get(url);

        if let Some(key) = &self.api_key {
            request = request.header("Authorization", format!("Bearer {}", key));
        }

        request
            .send()
            .await
            .with_context(|| format!("Failed to GET {}", url))
    }

    async fn post<T: Serialize>(&self, url: &str, body: &T) -> Result<Response> {
        let mut request = self.client.post(url).json(body);

        if let Some(key) = &self.api_key {
            request = request.header("Authorization", format!("Bearer {}", key));
        }

        request
            .send()
            .await
            .with_context(|| format!("Failed to POST to {}", url))
    }
}

pub struct OllamaClient {
    client: Client,
    base_url: String,
}

impl OllamaClient {
    pub fn new(base_url: impl Into<String>) -> Result<Self> {
        let client = Client::builder()
            .timeout(Duration::from_secs(10))
            .build()
            .context("Failed to create HTTP client")?;

        Ok(Self {
            client,
            base_url: base_url.into(),
        })
    }

    pub async fn check_health(&self) -> Result<bool> {
        let url = format!("{}/api/tags", self.base_url);
        let response = self
            .client
            .get(&url)
            .timeout(Duration::from_secs(2))
            .send()
            .await;

        Ok(response.is_ok())
    }

    pub async fn list_models(&self) -> Result<Vec<String>> {
        let url = format!("{}/api/tags", self.base_url);
        let response = self
            .client
            .get(&url)
            .send()
            .await
            .context("Failed to list Ollama models")?;

        #[derive(serde::Deserialize)]
        struct TagsResponse {
            models: Vec<ModelInfo>,
        }

        #[derive(serde::Deserialize)]
        struct ModelInfo {
            name: String,
        }

        let tags: TagsResponse = response
            .json()
            .await
            .context("Failed to parse Ollama models")?;

        Ok(tags.models.into_iter().map(|m| m.name).collect())
    }
}

pub struct LlamaCppClient {
    client: Client,
    base_url: String,
}

impl LlamaCppClient {
    pub fn new(base_url: impl Into<String>) -> Result<Self> {
        let client = Client::builder()
            .timeout(Duration::from_secs(10))
            .build()
            .context("Failed to create HTTP client")?;

        Ok(Self {
            client,
            base_url: base_url.into(),
        })
    }

    pub async fn check_health(&self) -> Result<bool> {
        let url = format!("{}/health", self.base_url);
        let response = self
            .client
            .get(&url)
            .timeout(Duration::from_secs(2))
            .send()
            .await;

        Ok(response.is_ok())
    }

    pub async fn get_props(&self) -> Result<serde_json::Value> {
        let url = format!("{}/props", self.base_url);
        let response = self
            .client
            .get(&url)
            .send()
            .await
            .context("Failed to get llama.cpp props")?;

        response
            .json()
            .await
            .context("Failed to parse llama.cpp props")
    }
}
