#![allow(warnings)]

mod consts;
mod request;
mod response;
use consts::*;
use request::*;
use response::*;

use serde_json::Value;

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

    /// emacs-friendly output
    #[clap(long = "emacs", short = 'e', default_value = "false")]
    emacs: bool,
}

#[derive(clap::Subcommand, Debug)]
enum Action {
    Session,
    Create,
    Enable,
    Disable,
    List,
}

#[derive(Debug, Default)]
pub struct JMAPClient {
    api_token: String,
    client: reqwest::Client,
    session: OnceCell<SessionResource>,
}

impl JMAPClient {
    pub fn new(api_token: String) -> Result<Self> {
        let mut headers = reqwest::header::HeaderMap::new();
        let mut auth_value =
            reqwest::header::HeaderValue::from_str(format!("Bearer {}", api_token).as_str())?;
        auth_value.set_sensitive(true);
        headers.insert(reqwest::header::AUTHORIZATION, auth_value);
        let client = reqwest::Client::builder()
            .default_headers(headers)
            .build()?;
        Ok(Self {
            api_token,
            client,
            ..Default::default()
        })
    }

    pub async fn init(&mut self) -> Result<()> {
        debug!("session");
        let session = self
            .client
            .get(SESSION_ENDPOINT)
            .send()
            .await?
            .json::<SessionResource>()
            .await?;
        self.session.set(session).unwrap();
        Ok(())
    }

    pub fn get_session(&self) -> &SessionResource {
        self.session.get().expect("session is not initialized")
    }

    pub fn get_api_url(&self) -> &String {
        let s = self.session.get().unwrap();
        &s.api_url
    }

    pub async fn do_session(&mut self) -> Result<()> {
        self.init().await?;
        debug!("{:#?}", &self.get_session());
        Ok(())
    }

    pub async fn do_list(&mut self) -> Result<()> {
        self.init().await?;
        debug!("do list");
        let account_id = self
            .get_session()
            .get_default_account_for_cap(MASKEDEMAIL_CAP)?;

        let m = MaskedMailGetAll::new(account_id.to_string());
        let invo: Invocation = m.into();
        let jmap_request = JMAPRequest::new(
            vec![JMAP_CORE_CAP.to_string(), MASKEDEMAIL_CAP.to_string()],
            vec![invo],
        );
        debug!("payload: {}", serde_json::to_string(&jmap_request)?);
        let r = self
            .client
            .post(self.get_api_url())
            .json(&jmap_request)
            .send()
            .await?
            .json::<MaskedMailGetResponse>()
            .await?;
        debug!("{:#?}", r);
        let all_masked_mails: &Vec<MaskedMail> = r.get_all();

        for m in all_masked_mails.iter() {
            println!("{}", m);
        }

        Ok(())
    }
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

    let mut jmap_client = JMAPClient::new(get_api_token().to_string())?;
    match &args.action {
        Action::Session => jmap_client.do_session().await?,
        Action::List => jmap_client.do_list().await?,
        _ => todo!(),
    };

    Ok(())
}
