use anyhow::{Context, Result};
use dirs;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    pub litellm: LiteLLMConfig,
    pub database: DatabaseConfig,
    pub projects: HashMap<String, ProjectConfig>,
    pub defaults: DefaultsConfig,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LiteLLMConfig {
    pub base_url: String,
    pub master_key: Option<String>,
    pub virtual_keys: HashMap<String, String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DatabaseConfig {
    pub url: String,
    pub history_retention_days: Option<i32>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProjectConfig {
    pub name: String,
    pub virtual_key: Option<String>,
    pub default_model: Option<String>,
    pub budget_limit: Option<f64>,
    pub budget_period: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DefaultsConfig {
    pub model: String,
    pub temperature: f64,
    pub max_tokens: Option<i32>,
}

impl Config {
    pub fn load() -> Result<Self> {
        let config_path = Self::config_path()?;

        if config_path.exists() {
            let contents = fs::read_to_string(&config_path)
                .with_context(|| format!("Failed to read config from {:?}", config_path))?;
            toml::from_str(&contents)
                .with_context(|| format!("Failed to parse config from {:?}", config_path))
        } else {
            Ok(Self::default())
        }
    }

    pub fn save(&self) -> Result<()> {
        let config_path = Self::config_path()?;

        // Create config directory if it doesn't exist
        if let Some(parent) = config_path.parent() {
            fs::create_dir_all(parent)
                .with_context(|| format!("Failed to create config directory {:?}", parent))?;
        }

        let contents = toml::to_string_pretty(self).context("Failed to serialize config")?;

        fs::write(&config_path, contents)
            .with_context(|| format!("Failed to write config to {:?}", config_path))?;

        Ok(())
    }

    pub fn config_path() -> Result<PathBuf> {
        let config_dir = dirs::config_dir().context("Could not determine config directory")?;
        Ok(config_dir.join("llm-tools").join("config.toml"))
    }

    pub fn get_api_key(&self, project: Option<&str>) -> Option<String> {
        // First check for project-specific key
        if let Some(proj) = project {
            if let Some(proj_config) = self.projects.get(proj) {
                if let Some(key) = &proj_config.virtual_key {
                    return Some(key.clone());
                }
            }
            if let Some(key) = self.litellm.virtual_keys.get(proj) {
                return Some(key.clone());
            }
        }

        // Fall back to master key
        self.litellm
            .master_key
            .clone()
            .or_else(|| std::env::var("LITELLM_MASTER_KEY").ok())
            .or_else(|| std::env::var("LITELLM_VIRTUAL_KEY").ok())
    }
}

impl Default for Config {
    fn default() -> Self {
        Self {
            litellm: LiteLLMConfig {
                base_url: std::env::var("LITELLM_BASE_URL")
                    .unwrap_or_else(|_| "http://localhost:4000/v1".to_string()),
                master_key: std::env::var("LITELLM_MASTER_KEY").ok(),
                virtual_keys: HashMap::new(),
            },
            database: DatabaseConfig {
                url: std::env::var("DATABASE_URL").unwrap_or_else(|_| {
                    let data_dir = dirs::data_dir().unwrap_or_else(|| PathBuf::from("."));
                    let db_path = data_dir.join("llm-tools").join("history.db");
                    format!("sqlite://{}", db_path.display())
                }),
                history_retention_days: Some(90),
            },
            projects: HashMap::new(),
            defaults: DefaultsConfig {
                model: "coding-balanced".to_string(),
                temperature: 0.7,
                max_tokens: Some(1024),
            },
        }
    }
}
