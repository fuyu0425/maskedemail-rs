use anyhow::{bail, Result};
use derivative::Derivative;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use serde_tuple::{Deserialize_tuple, Serialize_tuple};
use std::collections::BTreeMap;
use std::convert::From;

#[derive(Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum Method {
    Get(MaskedMailGetAll),
    Set(MaskedMailSet),
}

#[derive(Debug, Deserialize_tuple, Serialize_tuple)]
#[serde(rename_all = "camelCase")]
pub struct Invocation {
    pub name: String,
    pub arguments: Method,
    pub method_call_id: String,
}

#[derive(Debug, Deserialize, Serialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct JMAPRequest {
    pub using: Vec<String>,
    pub method_calls: Vec<Invocation>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub created_ids: Option<BTreeMap<String, String>>,
}

impl JMAPRequest {
    pub fn new(using: Vec<String>, method_calls: Vec<Invocation>) -> Self {
        Self {
            using,
            method_calls,
            ..Default::default()
        }
    }
}

#[derive(Debug, Deserialize, Serialize, Derivative)]
#[derivative(Default)]
#[serde(rename_all = "camelCase")]
pub struct MaskedMailCreate {
    #[derivative(Default(value = "\"pending\".to_string()"))]
    state: String,
    for_domain: String,
    description: String,
    url: Option<String>,
    email_prefix: Option<String>,
}

impl MaskedMailCreate {
    pub fn new(for_domain: String, description: String, state: Option<String>) -> Self {
        let mut ret = Self {
            for_domain,
            description,
            ..Default::default()
        };
        if let Some(state) = state {
            ret.state = state
        };
        ret
    }
}

#[derive(Debug, Deserialize, Serialize, Derivative)]
#[derivative(Default)]
#[serde(rename_all = "camelCase")]
pub struct MaskedMailPatch {
    #[serde(skip_serializing_if = "Option::is_none")]
    state: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    for_domain: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    url: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    email_prefix: Option<String>,
}

#[derive(Debug, Deserialize, Serialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct MaskedMailGetAll {
    pub account_id: String,
    pub ids: Option<Vec<String>>,
    pub properties: Option<Vec<String>>,
}

impl MaskedMailGetAll {
    pub fn new(account_id: String) -> Self {
        Self {
            account_id,
            ..Default::default()
        }
    }
}

impl From<MaskedMailGetAll> for Method {
    fn from(item: MaskedMailGetAll) -> Self {
        Method::Get(item)
    }
}

impl From<MaskedMailGetAll> for Invocation {
    fn from(item: MaskedMailGetAll) -> Self {
        Self {
            name: "MaskedEmail/get".to_string(),
            arguments: Method::Get(item),
            method_call_id: "default".to_string(),
        }
    }
}

#[derive(Debug, Deserialize, Serialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct MaskedMailSet {
    pub account_id: String,
    pub if_in_state: Option<String>,
    pub create: Option<BTreeMap<String, MaskedMailCreate>>,
    pub update: Option<BTreeMap<String, MaskedMailPatch>>,
    pub destroy: Option<Vec<String>>,
}

impl MaskedMailSet {
    pub fn new_create(account_id: String, create: MaskedMailCreate, tmp_id: &String) -> Self {
        let mut create_map = BTreeMap::new();
        create_map.insert(tmp_id.to_string(), create);
        Self {
            account_id,
            create: Some(create_map),
            ..Default::default()
        }
    }

    pub fn new_update(account_id: String, mm_id: String, patch: MaskedMailPatch) -> Self {
        let mut patch_map = BTreeMap::new();
        patch_map.insert(mm_id, patch);
        Self {
            account_id,
            update: Some(patch_map),
            ..Default::default()
        }
    }

    pub fn new_enable(account_id: String, mm_id: String) -> Self {
        let patch = MaskedMailPatch {
            state: Some("enabled".to_string()),
            ..Default::default()
        };
        Self::new_update(account_id, mm_id, patch)
    }

    pub fn new_disable(account_id: String, mm_id: String) -> Self {
        let patch = MaskedMailPatch {
            state: Some("disabled".to_string()),
            ..Default::default()
        };
        Self::new_update(account_id, mm_id, patch)
    }

    pub fn new_destory(account_id: String, ids: Vec<String>) -> Self {
        Self {
            account_id,
            destroy: Some(ids),
            ..Default::default()
        }
    }
}

impl From<MaskedMailSet> for Invocation {
    fn from(item: MaskedMailSet) -> Self {
        Self {
            name: "MaskedEmail/set".to_string(),
            arguments: Method::Set(item),
            method_call_id: "default".to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_method_serialize() {
        let m = Method::Get(MaskedMailGetAll {
            account_id: "123".to_string(),
            ..Default::default()
        });
        println!("{}", serde_json::to_string(&m).unwrap());
    }

    #[test]
    fn test_request_serialize() {
        let m = Method::Get(MaskedMailGetAll {
            account_id: "123".to_string(),
            ..Default::default()
        });

        let invo = Invocation {
            name: "test".to_string(),
            arguments: m,
            method_call_id: "mid".to_string(),
        };

        let r = JMAPRequest {
            using: vec!["123".to_string()],
            method_calls: vec![invo],
            ..Default::default()
        };

        println!("{}", serde_json::to_string(&r).unwrap());
    }

    #[test]
    fn test_maskedemail_request() {
        println!("{:#?}", MaskedMailCreate::default());
    }
}
