use anyhow::{anyhow, bail, Result};
use chrono;
use maskedemail::{is_emacs, APP_NAME, EMACS};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use serde_tuple::{Deserialize_tuple, Serialize_tuple};
use std::collections::BTreeMap;
use std::fmt;
use tracing::{debug, info};

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Capabilities {
    pub max_size_upload: usize,
    pub max_concurrent_upload: usize,
    pub max_size_request: usize,
    pub max_concurrent_requests: usize,
    pub max_calls_in_request: usize,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Account {
    pub name: String,
    pub account_capabilities: BTreeMap<String, Value>,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SessionResource {
    // capabilities: Capabilities
    pub capabilities: BTreeMap<String, Value>,
    pub username: String,
    pub api_url: String,
    pub upload_url: String,
    pub download_url: String,
    pub event_source_url: String,
    pub accounts: BTreeMap<String, Account>,
    pub primary_accounts: BTreeMap<String, String>,
    pub state: String,

    #[serde(flatten)]
    pub extra: BTreeMap<String, Value>,
}

impl SessionResource {
    pub fn list_accounts(&self) {
        for (_key, account) in self.accounts.iter() {
            println!("{}", account.name);
        }
    }

    pub fn get_default_account_for_cap(&self, cap: &str) -> Result<&String> {
        debug!("{:#?}", &self.primary_accounts);
        debug!("{:#?}", self.primary_accounts.get(cap));
        self.primary_accounts.get(cap).ok_or(anyhow!("not found"))
    }

    pub fn get_account(&self, account_id: &str) -> Result<&Account> {
        self.accounts
            .get(account_id)
            .ok_or(anyhow!("account not found"))
    }

    pub fn is_account_has_cap(&self, account_id: &str, cap: &str) -> Result<bool> {
        self.get_account(account_id)?.has_capability(cap)
    }
}

impl Account {
    pub fn has_capability(&self, cap: &str) -> Result<bool> {
        Ok(self.account_capabilities.contains_key(cap))
    }
}

#[derive(Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct MaskedMail {
    pub id: String,
    pub email: String,
    pub state: Option<String>,
    pub for_domain: Option<String>,
    pub description: Option<String>,
    pub last_message_at: Option<chrono::DateTime<chrono::Utc>>,
    pub created_at: chrono::DateTime<chrono::Utc>,
    pub created_by: String,
    pub url: Option<String>,
}

impl fmt::Display for MaskedMail {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if !is_emacs() {
            write!(
                f,
                "id: {}\t email: {}\t domain: {}\t state: {}",
                self.id,
                self.email,
                if let Some(domain) = &self.for_domain {
                    domain.as_str()
                } else {
                    ""
                },
                if let Some(state) = &self.state {
                    state.as_str()
                } else {
                    ""
                }
            )
        } else {
            write!(f, "{}", self.to_emacs())
        }
    }
}

impl MaskedMail {
    pub fn to_emacs(&self) -> String {
        format!(
            "{} {} {}",
            self.email,
            if let Some(domain) = &self.for_domain {
                if !domain.is_empty() {
                    domain.as_str()
                } else {
                    "nil"
                }
            } else {
                "nil"
            },
            if let Some(state) = &self.state {
                state.as_str()
            } else {
                "nil"
            }
        )
        .to_string()
    }
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct MaskedMailGetResponseList {
    pub account_id: String,
    pub list: Vec<MaskedMail>,
    pub not_found: Vec<String>,
    pub state: String,
}

#[derive(Debug, Deserialize_tuple, Serialize_tuple)]
#[serde(rename_all = "camelCase")]
pub struct ResInvocation {
    pub name: String,
    pub arguments: MaskedMailGetResponseList,
    pub method_call_id: String,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct MaskedMailGetResponse {
    pub latest_client_version: String,
    pub method_responses: Vec<ResInvocation>,
    pub session_state: String,
    #[serde(flatten)]
    pub extra: BTreeMap<String, Value>,
}

impl MaskedMailGetResponse {
    pub fn get_all(&self) -> Vec<MaskedMail> {
        self.method_responses[0].arguments.list.clone()
    }
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SetError {
    pub r#type: String,
    pub description: Option<String>,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct MaskedMailSetResponseAll {
    pub account_id: String,
    pub old_state: Option<String>,
    pub new_state: Option<String>,
    pub created: Option<BTreeMap<String, MaskedMail>>,
    pub updated: Option<BTreeMap<String, Option<MaskedMail>>>,
    pub destroyed: Option<Vec<String>>,
    pub not_created: Option<BTreeMap<String, SetError>>,
    pub not_updated: Option<BTreeMap<String, SetError>>,
    pub not_destroyed: Option<BTreeMap<String, SetError>>,
}

#[derive(Debug, Deserialize_tuple, Serialize_tuple)]
#[serde(rename_all = "camelCase")]
pub struct ResSetInvocation {
    pub name: String,
    pub arguments: MaskedMailSetResponseAll,
    pub method_call_id: String,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct MaskedMailSetResponse {
    pub latest_client_version: String,
    pub method_responses: Vec<ResSetInvocation>,
    pub session_state: String,
    #[serde(flatten)]
    pub extra: BTreeMap<String, Value>,
}
