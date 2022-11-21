use anyhow::{anyhow, bail, Result};
use chrono;
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

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct MaskedMail {
    pub id: String,
    pub email: String,
    pub state: String,
    pub for_domain: String,
    pub description: String,
    pub last_message_at: chrono::DateTime<chrono::Utc>,
    pub created_at: chrono::DateTime<chrono::Utc>,
    pub created_by: String,
    pub url: Option<String>,
}

impl fmt::Display for MaskedMail {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "id: {}\t email: {}\t domain: {}",
            self.id, self.email, self.for_domain
        )
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
    pub fn get_all(&self) -> &Vec<MaskedMail> {
        &self.method_responses[0].arguments.list
    }
}
