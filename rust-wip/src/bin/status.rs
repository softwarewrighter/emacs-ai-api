use anyhow::Result;
use clap::Parser;
use colored::Colorize;
use comfy_table::{Cell, Color as TableColor, ContentArrangement, Table};
use indicatif::{ProgressBar, ProgressStyle};
use tokio::time::Instant;

use llm_tools::client::{LiteLLMClient, LlamaCppClient, OllamaClient};
use llm_tools::config::Config;

#[derive(Parser, Debug)]
#[command(author, version, about = "Check status of LLM providers and models")]
struct Args {
    /// Check only specific provider
    #[arg(short, long)]
    provider: Option<String>,

    /// Show detailed information
    #[arg(short, long)]
    verbose: bool,

    /// Output format (table, json, simple)
    #[arg(short, long, default_value = "table")]
    format: String,

    /// Custom LiteLLM base URL
    #[arg(long, env = "LITELLM_BASE_URL")]
    base_url: Option<String>,
}

#[tokio::main]
async fn main() -> Result<()> {
    let args = Args::parse();
    let config = Config::load()?;

    let base_url = args
        .base_url
        .or(Some(config.litellm.base_url.clone()))
        .unwrap_or_else(|| "http://localhost:4000/v1".to_string());

    println!("{}", "LLM Provider Status Check".bold().cyan());
    println!("{}", "=========================".cyan());
    println!();

    let pb = ProgressBar::new_spinner();
    pb.set_style(
        ProgressStyle::default_spinner()
            .template("{spinner:.green} {msg}")
            .unwrap(),
    );

    // Check LiteLLM Gateway
    pb.set_message("Checking LiteLLM gateway...");
    let litellm_status = check_litellm(&base_url, &config).await;

    // Check Ollama
    pb.set_message("Checking Ollama...");
    let ollama_status = check_ollama().await;

    // Check llama.cpp
    pb.set_message("Checking llama.cpp...");
    let llamacpp_status = check_llamacpp().await;

    pb.finish_and_clear();

    // Display results
    match args.format.as_str() {
        "json" => print_json_status(litellm_status, ollama_status, llamacpp_status)?,
        "simple" => print_simple_status(litellm_status, ollama_status, llamacpp_status),
        _ => print_table_status(litellm_status, ollama_status, llamacpp_status, args.verbose),
    }

    Ok(())
}

#[derive(Debug)]
struct ServiceStatus {
    name: String,
    online: bool,
    latency_ms: Option<u64>,
    models: Vec<String>,
    error: Option<String>,
    endpoint: String,
}

async fn check_litellm(base_url: &str, config: &Config) -> ServiceStatus {
    let start = Instant::now();
    let client = match LiteLLMClient::new(base_url, config.litellm.master_key.clone()) {
        Ok(c) => c,
        Err(e) => {
            return ServiceStatus {
                name: "LiteLLM Gateway".to_string(),
                online: false,
                latency_ms: None,
                models: vec![],
                error: Some(e.to_string()),
                endpoint: base_url.to_string(),
            }
        }
    };

    let health = client.check_health().await.unwrap_or(false);
    let latency = start.elapsed().as_millis() as u64;

    let models = if health {
        client.list_models().await.unwrap_or_default()
    } else {
        vec![]
    };

    ServiceStatus {
        name: "LiteLLM Gateway".to_string(),
        online: health,
        latency_ms: Some(latency),
        models,
        error: if !health {
            Some("Gateway not responding".to_string())
        } else {
            None
        },
        endpoint: base_url.to_string(),
    }
}

async fn check_ollama() -> ServiceStatus {
    let base_url = "http://localhost:11434";
    let start = Instant::now();

    let client = match OllamaClient::new(base_url) {
        Ok(c) => c,
        Err(e) => {
            return ServiceStatus {
                name: "Ollama".to_string(),
                online: false,
                latency_ms: None,
                models: vec![],
                error: Some(e.to_string()),
                endpoint: base_url.to_string(),
            }
        }
    };

    let health = client.check_health().await.unwrap_or(false);
    let latency = start.elapsed().as_millis() as u64;

    let models = if health {
        client.list_models().await.unwrap_or_default()
    } else {
        vec![]
    };

    ServiceStatus {
        name: "Ollama".to_string(),
        online: health,
        latency_ms: Some(latency),
        models,
        error: if !health {
            Some("Ollama not running (start with: ollama serve)".to_string())
        } else {
            None
        },
        endpoint: base_url.to_string(),
    }
}

async fn check_llamacpp() -> ServiceStatus {
    let base_url = "http://localhost:8080";
    let start = Instant::now();

    let client = match LlamaCppClient::new(base_url) {
        Ok(c) => c,
        Err(e) => {
            return ServiceStatus {
                name: "llama.cpp".to_string(),
                online: false,
                latency_ms: None,
                models: vec![],
                error: Some(e.to_string()),
                endpoint: base_url.to_string(),
            }
        }
    };

    let health = client.check_health().await.unwrap_or(false);
    let latency = start.elapsed().as_millis() as u64;

    let models = if health {
        // llama.cpp doesn't list models, but we can get props
        if let Ok(props) = client.get_props().await {
            vec![props
                .get("model")
                .and_then(|m| m.as_str())
                .unwrap_or("model")
                .to_string()]
        } else {
            vec![]
        }
    } else {
        vec![]
    };

    ServiceStatus {
        name: "llama.cpp".to_string(),
        online: health,
        latency_ms: Some(latency),
        models,
        error: if !health {
            Some("Server not running (start with: ./server -m model.gguf)".to_string())
        } else {
            None
        },
        endpoint: base_url.to_string(),
    }
}

fn print_table_status(
    litellm: ServiceStatus,
    ollama: ServiceStatus,
    llamacpp: ServiceStatus,
    verbose: bool,
) {
    let mut table = Table::new();
    table
        .set_content_arrangement(ContentArrangement::Dynamic)
        .set_header(vec!["Service", "Status", "Latency", "Models", "Endpoint"]);

    for status in [litellm, ollama, llamacpp] {
        let status_cell = if status.online {
            Cell::new("● Online".green())
        } else {
            Cell::new("● Offline".red())
        };

        let latency_cell = if let Some(ms) = status.latency_ms {
            Cell::new(format!("{}ms", ms))
        } else {
            Cell::new("-")
        };

        let models_cell = if status.models.is_empty() {
            Cell::new("-")
        } else if verbose {
            Cell::new(status.models.join(", "))
        } else {
            Cell::new(format!("{} models", status.models.len()))
        };

        table.add_row(vec![
            Cell::new(&status.name),
            status_cell,
            latency_cell,
            models_cell,
            Cell::new(&status.endpoint),
        ]);

        if let Some(error) = status.error {
            if verbose {
                table.add_row(vec![
                    Cell::new(""),
                    Cell::new(format!("Error: {}", error)).fg(TableColor::Red),
                    Cell::new(""),
                    Cell::new(""),
                    Cell::new(""),
                ]);
            }
        }
    }

    println!("{}", table);
}

fn print_simple_status(litellm: ServiceStatus, ollama: ServiceStatus, llamacpp: ServiceStatus) {
    for status in [litellm, ollama, llamacpp] {
        let status_icon = if status.online {
            "✓".green()
        } else {
            "✗".red()
        };
        println!(
            "{} {} - {}",
            status_icon,
            status.name,
            if status.online {
                format!("{} models available", status.models.len())
            } else {
                status.error.unwrap_or_else(|| "Not available".to_string())
            }
        );
    }
}

fn print_json_status(
    litellm: ServiceStatus,
    ollama: ServiceStatus,
    llamacpp: ServiceStatus,
) -> Result<()> {
    let json = serde_json::json!({
        "services": {
            "litellm": {
                "online": litellm.online,
                "latency_ms": litellm.latency_ms,
                "models": litellm.models,
                "endpoint": litellm.endpoint,
                "error": litellm.error,
            },
            "ollama": {
                "online": ollama.online,
                "latency_ms": ollama.latency_ms,
                "models": ollama.models,
                "endpoint": ollama.endpoint,
                "error": ollama.error,
            },
            "llamacpp": {
                "online": llamacpp.online,
                "latency_ms": llamacpp.latency_ms,
                "models": llamacpp.models,
                "endpoint": llamacpp.endpoint,
                "error": llamacpp.error,
            },
        },
        "timestamp": chrono::Utc::now(),
    });

    println!("{}", serde_json::to_string_pretty(&json)?);
    Ok(())
}
