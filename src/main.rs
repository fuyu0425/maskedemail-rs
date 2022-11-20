use anyhow::Result;
use once_cell::sync::OnceCell;
use serde::{Deserialize, Serialize};
use tracing::{debug, info};
use tracing_subscriber::{fmt, prelude::*, EnvFilter};

static APP_NAME: OnceCell<&str> = OnceCell::new();

shadow_rs::shadow!(build);

#[derive(Default, Debug, Serialize, Deserialize)]
struct Config {
    api_token: String,
}

static CONFIG: OnceCell<Config> = OnceCell::new();

fn get_api_token() -> &'static String {
    &CONFIG.get().unwrap().api_token
}

static CLIENT: OnceCell<reqwest::Client> = OnceCell::new();

fn get_client() -> &'static reqwest::Client {
    &CLIENT.get().unwrap()
}

#[tokio::main]
async fn main() -> Result<()> {
    tracing_subscriber::registry()
        .with(fmt::layer())
        .with(EnvFilter::from_default_env())
        .init();
    APP_NAME.set("maskedmail").unwrap();
    let cfg: Config = confy::load(APP_NAME.get().unwrap(), "config")?;
    let file = confy::get_configuration_file_path(APP_NAME.get().unwrap(), "config")?;
    tracing::debug!(?cfg);
    tracing::debug!(?file);
    CONFIG.set(cfg).unwrap();
    tracing::debug!("{}", get_api_token());
    let client = reqwest::Client::new();
    CLIENT.set(client).unwrap();
    Ok(())
}
