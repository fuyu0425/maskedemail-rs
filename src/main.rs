use anyhow::Result;
use clap::Parser;
use maskedemail::with_client;
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

#[derive(Parser, Debug)]
#[clap(author, version, about)]
pub struct Args {
    #[command(subcommand)]
    action: Action,
}

#[derive(clap::Subcommand, Debug)]
enum Action {
    Session,
    Create,
    Enable,
    Disable,
    List,
}

#[with_client]
async fn list() -> Result<()> {
    Ok(())
}

#[with_client]
async fn session() -> Result<()> {
    debug!("session");
    debug!("{:#?}", client);
    Ok(())
}

#[tokio::main]
async fn main() -> Result<()> {
    let args = Args::parse();
    debug!("args {:#?}", args);
    tracing_subscriber::registry()
        .with(fmt::layer())
        .with(EnvFilter::from_default_env())
        .init();
    APP_NAME.set("maskedmail").unwrap();
    let cfg: Config = confy::load(APP_NAME.get().unwrap(), "config")?;
    let file = confy::get_configuration_file_path(APP_NAME.get().unwrap(), "config")?;
    debug!(?cfg);
    debug!(?file);
    CONFIG.set(cfg).unwrap();
    debug!("{}", get_api_token());
    let client = reqwest::Client::new();
    CLIENT.set(client).unwrap();

    match &args.action {
        Action::Session => session().await?,
        _ => todo!(),
    };

    Ok(())
}
