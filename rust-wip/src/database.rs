use anyhow::{Context, Result};
use chrono::{DateTime, Utc};
use sqlx::{Pool, Sqlite, SqlitePool};
use std::path::Path;

use crate::models::*;

pub struct Database {
    pool: Pool<Sqlite>,
}

impl Database {
    pub async fn connect(url: &str) -> Result<Self> {
        // Ensure parent directory exists for SQLite
        if url.starts_with("sqlite://") {
            let path = url.trim_start_matches("sqlite://");
            if let Some(parent) = Path::new(path).parent() {
                std::fs::create_dir_all(parent).context("Failed to create database directory")?;
            }
        }

        let pool = SqlitePool::connect(url)
            .await
            .with_context(|| format!("Failed to connect to database: {}", url))?;

        // Run migrations
        sqlx::migrate!("./migrations")
            .run(&pool)
            .await
            .context("Failed to run database migrations")?;

        Ok(Self { pool })
    }

    pub async fn record_usage(&self, usage: &ApiUsage) -> Result<i64> {
        let result = sqlx::query(
            r#"
            INSERT INTO api_usage (
                timestamp, provider, model, prompt_tokens, completion_tokens,
                total_tokens, cost, project, request_id, latency_ms
            )
            VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10)
            "#,
        )
        .bind(usage.timestamp)
        .bind(&usage.provider)
        .bind(&usage.model)
        .bind(usage.prompt_tokens)
        .bind(usage.completion_tokens)
        .bind(usage.total_tokens)
        .bind(usage.cost)
        .bind(&usage.project)
        .bind(&usage.request_id)
        .bind(usage.latency_ms)
        .execute(&self.pool)
        .await
        .context("Failed to record API usage")?;

        Ok(result.last_insert_rowid())
    }

    pub async fn get_usage_history(
        &self,
        limit: Option<i32>,
        project: Option<&str>,
    ) -> Result<Vec<ApiUsage>> {
        let limit = limit.unwrap_or(100);

        let records = if let Some(proj) = project {
            sqlx::query_as::<_, ApiUsage>(
                r#"
                SELECT * FROM api_usage
                WHERE project = ?1
                ORDER BY timestamp DESC
                LIMIT ?2
                "#,
            )
            .bind(proj)
            .bind(limit)
            .fetch_all(&self.pool)
            .await?
        } else {
            sqlx::query_as::<_, ApiUsage>(
                r#"
                SELECT * FROM api_usage
                ORDER BY timestamp DESC
                LIMIT ?1
                "#,
            )
            .bind(limit)
            .fetch_all(&self.pool)
            .await?
        };

        Ok(records)
    }

    pub async fn get_cost_summary(
        &self,
        start_date: DateTime<Utc>,
        end_date: DateTime<Utc>,
        project: Option<&str>,
    ) -> Result<CostSummary> {
        let records = if let Some(proj) = project {
            sqlx::query_as::<_, ApiUsage>(
                r#"
                SELECT * FROM api_usage
                WHERE timestamp >= ?1 AND timestamp <= ?2 AND project = ?3
                ORDER BY timestamp DESC
                "#,
            )
            .bind(start_date)
            .bind(end_date)
            .bind(proj)
            .fetch_all(&self.pool)
            .await?
        } else {
            sqlx::query_as::<_, ApiUsage>(
                r#"
                SELECT * FROM api_usage
                WHERE timestamp >= ?1 AND timestamp <= ?2
                ORDER BY timestamp DESC
                "#,
            )
            .bind(start_date)
            .bind(end_date)
            .fetch_all(&self.pool)
            .await?
        };

        let mut summary = CostSummary {
            period: format!(
                "{} to {}",
                start_date.format("%Y-%m-%d"),
                end_date.format("%Y-%m-%d")
            ),
            total_cost: 0.0,
            total_tokens: 0,
            request_count: records.len() as i32,
            by_provider: std::collections::HashMap::new(),
            by_model: std::collections::HashMap::new(),
            by_project: std::collections::HashMap::new(),
        };

        for record in records {
            summary.total_cost += record.cost;
            summary.total_tokens += record.total_tokens as i64;

            *summary
                .by_provider
                .entry(record.provider.clone())
                .or_insert(0.0) += record.cost;
            *summary.by_model.entry(record.model.clone()).or_insert(0.0) += record.cost;

            if let Some(proj) = record.project {
                *summary.by_project.entry(proj).or_insert(0.0) += record.cost;
            }
        }

        Ok(summary)
    }
}
