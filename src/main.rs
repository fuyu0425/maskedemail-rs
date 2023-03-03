#![allow(warnings)]

use clap_verbosity_flag::Verbosity;
use futures::future::try_join_all;
mod consts;
mod request;
mod response;
use consts::*;
use request::*;
use response::*;

use serde_json::Value;

use anyhow::Result;
use async_once_cell::OnceCell as AsyncOnceCell;
use clap::Parser;
use once_cell::sync::OnceCell;
use serde::{Deserialize, Serialize};
use tracing::{debug, info};
use tracing_subscriber::{fmt, prelude::*, EnvFilter};

use maskedemail::{is_emacs, APP_NAME, EMACS};

shadow_rs::shadow!(build);

#[derive(Default, Debug, Serialize, Deserialize)]
struct Config {
    api_token: String,
}

static CONFIG: OnceCell<Config> = OnceCell::new();

fn get_api_token() -> &'static String {
    &CONFIG.get().unwrap().api_token
}

#[derive(Parser, Debug)]
#[clap(author, version, about)]
pub struct Args {
    #[command(subcommand)]
    action: Action,

    /// emacs-friendly output
    #[clap(long = "emacs", short = 'e', default_value = "false", global = true)]
    emacs: bool,

    #[clap(flatten)]
    verbose: clap_verbosity_flag::Verbosity,
}

#[derive(clap::Subcommand, Debug)]
enum Action {
    Session,
    Create {
        /// website domain (e.g. facebook.com)
        domain: String,
        /// enable when creation
        #[clap(long = "enable", default_value = "false")]
        enable: bool,
        #[clap(long = "prefix", short = 'p')]
        prefix: Option<String>,
    },
    Enable {
        id_or_email: String,
    },
    Disable {
        id_or_email: String,
    },
    Remove {
        id_or_email: String,
    },
    #[clap(alias = "ls")]
    List {
        /// show deleted ones
        #[clap(long = "deleted", default_value = "false")]
        show_deleted: bool,
    },
    /// clean pending ones
    Clean {
        /// also clean disabled ones
        #[clap(long = "disable", default_value = "false")]
        disable: bool,
    },
}

#[derive(Debug)]
pub struct JMAPClient {
    api_token: String,
    client: reqwest::Client,
    session: AsyncOnceCell<SessionResource>,
    all_masked_mails: AsyncOnceCell<Vec<MaskedMail>>,
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
            session: AsyncOnceCell::<SessionResource>::new(),
            all_masked_mails: AsyncOnceCell::<Vec<MaskedMail>>::new(),
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

    pub async fn get_all_masked_mails(&self) -> Result<&Vec<MaskedMail>> {
        Ok(self
            .all_masked_mails
            .get_or_init(async { self.do_list(true).await.unwrap() })
            .await)
    }

    pub fn get_api_url(&self) -> &String {
        let s = self.session.get().unwrap();
        &s.api_url
    }

    pub async fn do_session(&mut self) -> Result<()> {
        debug!("{:#?}", &self.get_session().await);
        if is_emacs() {
            println!("ok");
        }
        Ok(())
    }

    pub async fn do_list(&self, show_deleted: bool) -> Result<Vec<MaskedMail>> {
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
        let all_masked_mails: Vec<MaskedMail> = r.get_all();

        let d = Some("deleted".to_string());
        for m in all_masked_mails.iter() {
            if show_deleted || m.state != d {
                println!("{}", m);
            }
        }

        Ok(all_masked_mails)
    }
    pub async fn do_create(
        &self,
        domain: String,
        enable: bool,
        email_prefix: &Option<String>,
    ) -> Result<()> {
        debug!("do create for domain {domain}");
        let account_id = self
            .get_session()
            .await
            .get_default_account_for_cap(MASKEDEMAIL_CAP)?;
        let mut masked_mail_create = MaskedMailCreate::new(
            domain.clone(),
            domain.clone(),
            if enable {
                Some("enabled".to_string())
            } else {
                None
            },
            email_prefix.clone(),
        );
        let tmp_id = "tmp_id".to_string();
        let mut masked_mail_set =
            MaskedMailSet::new_create(account_id.to_string(), masked_mail_create, &tmp_id);
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
        let mut created = m.get_created(&tmp_id);
        if let Some(mut created) = created {
            created.for_domain = Some(domain);
            println!("{}", created);
        }
        Ok(())
    }

    pub async fn do_enable(&self, id: String) -> Result<()> {
        debug!("do enable for {id:#?}");
        let account_id = self
            .get_session()
            .await
            .get_default_account_for_cap(MASKEDEMAIL_CAP)?;
        let mut masked_mail_set = MaskedMailSet::new_enable(account_id.to_string(), id);
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

    pub async fn do_remove(&self, ids: Vec<String>) -> Result<()> {
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

    pub async fn do_disable(&self, id: String) -> Result<()> {
        debug!("do disable for {id:#?}");
        let account_id = self
            .get_session()
            .await
            .get_default_account_for_cap(MASKEDEMAIL_CAP)?;
        let mut masked_mail_set = MaskedMailSet::new_disable(account_id.to_string(), id);
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
    pub async fn do_clean(&self, disable: bool) -> Result<()> {
        let mut destroy_ids = vec![];
        for m in self.get_all_masked_mails().await?.iter() {
            if let Some(state) = &m.state {
                if state == "pending" {
                    destroy_ids.push(m.id.to_string());
                } else if disable && state == "disabled" {
                    destroy_ids.push(m.id.to_string());
                }
            }
        }

        self.do_remove(destroy_ids).await?;
        Ok(())
    }

    pub async fn find_id(&self, id_or_email: &String) -> Result<Option<String>> {
        if !id_or_email.contains("@") {
            return Ok(Some(id_or_email.to_string()));
        }

        for m in self.get_all_masked_mails().await?.iter() {
            if id_or_email == &m.email {
                return Ok(Some(m.id.to_string()));
            }
        }
        Ok(None)
    }

    pub async fn find_ids(&self, id_or_emails: Vec<&String>) -> Result<Vec<Option<String>>> {
        try_join_all(
            id_or_emails
                .iter()
                .map(|id_or_email| self.find_id(&id_or_email)),
        )
        .await
    }
}

fn convert_filter(filter: log::LevelFilter) -> tracing_subscriber::filter::LevelFilter {
    match filter {
        log::LevelFilter::Off => tracing_subscriber::filter::LevelFilter::OFF,
        log::LevelFilter::Error => tracing_subscriber::filter::LevelFilter::ERROR,
        log::LevelFilter::Warn => tracing_subscriber::filter::LevelFilter::WARN,
        log::LevelFilter::Info => tracing_subscriber::filter::LevelFilter::INFO,
        log::LevelFilter::Debug => tracing_subscriber::filter::LevelFilter::DEBUG,
        log::LevelFilter::Trace => tracing_subscriber::filter::LevelFilter::TRACE,
    }
}

#[tokio::main]
async fn main() -> Result<()> {
    let args = Args::parse();
    debug!("args {:#?}", args);
    tracing_subscriber::registry()
        .with(fmt::layer().with_filter(convert_filter(args.verbose.log_level_filter())))
        // .with(EnvFilter::from_default_env())
        .init();
    APP_NAME.set("maskedmail").unwrap();
    let cfg: Config = confy::load(APP_NAME.get().unwrap(), "config")?;
    let file = confy::get_configuration_file_path(APP_NAME.get().unwrap(), "config")?;
    // debug!(?cfg);
    debug!(?file);
    CONFIG.set(cfg).unwrap();

    EMACS.set(args.emacs).unwrap();

    let mut jmap_client = JMAPClient::new(get_api_token().to_string())?;
    match &args.action {
        Action::Session => jmap_client.do_session().await?,
        Action::List { show_deleted } => {
            jmap_client.do_list(*show_deleted).await?;
        }
        Action::Create {
            domain,
            enable,
            prefix,
        } => {
            jmap_client
                .do_create(domain.to_string(), *enable, prefix)
                .await?
        }
        Action::Remove { id_or_email } => {
            let id = jmap_client.find_id(id_or_email).await?.unwrap();
            jmap_client.do_remove(vec![id]).await?
        }
        Action::Enable { id_or_email } => {
            let id = jmap_client.find_id(id_or_email).await?.unwrap();
            jmap_client.do_enable(id).await?
        }
        Action::Disable { id_or_email } => {
            let id = jmap_client.find_id(id_or_email).await?.unwrap();
            jmap_client.do_disable(id).await?
        }
        Action::Clean { disable } => jmap_client.do_clean(*disable).await?,
        _ => todo!(),
    };

    Ok(())
}
