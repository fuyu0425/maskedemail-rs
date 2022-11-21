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
// use async_once_cell::OnceCell;
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
    Create {
        domain: String,
        /// enable after creation
        #[clap(long = "enable", default_value = "false")]
        enable: bool,
    },
    Enable {
        email: String,
    },
    Disable {
        email: String,
    },
    Remove {
        id: String,
    },
    List,
    /// destory all pending ones
    Clean,
}

#[derive(Debug)]
pub struct JMAPClient {
    api_token: String,
    client: reqwest::Client,
    session: async_once_cell::OnceCell<SessionResource>,
    all_masked_mails: Option<MaskedMail>,
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
            session: async_once_cell::OnceCell::<SessionResource>::new(),
            all_masked_mails: None,
        })
    }

    pub async fn get_session(&self) -> &SessionResource {
        self.session
            .get_or_init(async {
                debug!("session");
                let session = self
                    .client
                    .get(SESSION_ENDPOINT)
                    .send()
                    .await
                    .unwrap()
                    .json::<SessionResource>()
                    .await
                    .unwrap();
                session
            })
            .await
    }

    pub fn get_api_url(&self) -> &String {
        let s = self.session.get().unwrap();
        &s.api_url
    }

    pub async fn do_session(&mut self) -> Result<()> {
        debug!("{:#?}", &self.get_session().await);
        Ok(())
    }

    pub async fn do_list(&mut self) -> Result<MaskedMailGetResponse> {
        debug!("do list");
        let account_id = self
            .get_session()
            .await
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

        Ok(r)
    }
    pub async fn do_create(&mut self, domain: String, enable: bool) -> Result<()> {
        debug!("do create for domain {domain}");
        let account_id = self
            .get_session()
            .await
            .get_default_account_for_cap(MASKEDEMAIL_CAP)?;
        let mut masked_mail_create = MaskedMailCreate::new(
            domain,
            "test create".to_string(),
            if enable {
                Some("enabled".to_string())
            } else {
                None
            },
        );
        let mut masked_mail_set =
            MaskedMailSet::new_create(account_id.to_string(), masked_mail_create);
        let invo: Invocation = masked_mail_set.into();
        let jmap_request = JMAPRequest::new(
            vec![JMAP_CORE_CAP.to_string(), MASKEDEMAIL_CAP.to_string()],
            vec![invo],
        );
        let r = self
            .client
            .post(self.get_api_url())
            .json(&jmap_request)
            .send()
            .await?
            .json::<Value>()
            .await?;
        debug!("{:#?}", r);

        let m: MaskedMailSetResponse = serde_json::from_value(r)?;
        debug!("{:#?}", m);
        Ok(())
    }

    pub async fn do_remove(&mut self, ids: Vec<String>) -> Result<()> {
        debug!("do remove for {ids:#?}");
        let account_id = self
            .get_session()
            .await
            .get_default_account_for_cap(MASKEDEMAIL_CAP)?;
        let mut masked_mail_set = MaskedMailSet::new_destory(account_id.to_string(), ids);
        let invo: Invocation = masked_mail_set.into();
        let jmap_request = JMAPRequest::new(
            vec![JMAP_CORE_CAP.to_string(), MASKEDEMAIL_CAP.to_string()],
            vec![invo],
        );
        let r = self
            .client
            .post(self.get_api_url())
            .json(&jmap_request)
            .send()
            .await?
            .json::<Value>()
            .await?;
        debug!("{:#?}", r);

        let m: MaskedMailSetResponse = serde_json::from_value(r)?;
        debug!("{:#?}", m);
        Ok(())
    }

    pub async fn do_clean(&mut self) -> Result<()> {
        let list_result = self.do_list().await?;
        let all_masked_mails: &Vec<MaskedMail> = list_result.get_all();
        let mut destroy_ids = vec![];
        for m in all_masked_mails.iter() {
            if let Some(state) = &m.state {
                if state == "pending" {
                    destroy_ids.push(m.id.to_string());
                }
            }
        }

        self.do_remove(destroy_ids).await?;
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
    // debug!(?cfg);
    debug!(?file);
    CONFIG.set(cfg).unwrap();

    let mut jmap_client = JMAPClient::new(get_api_token().to_string())?;
    match &args.action {
        Action::Session => jmap_client.do_session().await?,
        Action::List => {
            jmap_client.do_list().await?;
        }
        Action::Create { domain, enable } => {
            jmap_client.do_create(domain.to_string(), *enable).await?
        }
        Action::Remove { id } => jmap_client.do_remove(vec![id.to_string()]).await?,
        Action::Enable { email } => todo!(),
        Action::Disable { email } => todo!(),
        Action::Clean => jmap_client.do_clean().await?,
        _ => todo!(),
    };

    Ok(())
}
