//NOTE this code is copied over from sdk-config-server with small changes for compatiblity
//TODO refactor, make eval MJOS agnostic

use crate::{utils::core::MapError, Context};
use jsonlogic;
use serde_json::{json, Map, Value};

fn get_overrides(
    query_data: &Map<String, Value>,
    contexts: &Vec<Context>,
    overrides: &Map<String, Value>,
) -> serde_json::Result<Value> {
    let mut required_overrides: Value = json!({});

    for context in contexts.iter() {
        // TODO :: Add semantic version comparator in Lib
        if let Ok(Value::Bool(true)) =
            jsonlogic::apply(&context.condition, &json!(query_data))
        {
            for override_key in &context.override_with_keys {
                if let Some(overriden_value) = overrides.get(override_key) {
                    json_patch::merge(&mut required_overrides, overriden_value)
                }
            }
        }
    }

    Ok(required_overrides)
}

fn merge_overrides_on_default_config(
    default_config: &mut Map<String, Value>,
    overrides: Map<String, Value>,
) {
    overrides.into_iter().for_each(|(key, val)| {
        if let Some(og_val) = default_config.get_mut(&key) {
            json_patch::merge(og_val, &val)
        } else {
            log::error!("CAC: found non-default_config key: {key} in overrides");
        }
    })
}

pub fn eval_cac(
    mut default_config: Map<String, Value>,
    contexts: &Vec<Context>,
    overrides: &Map<String, Value>,
    query_data: &Map<String, Value>,
) -> Result<Map<String, Value>, String> {
    let overrides: Map<String, Value> = get_overrides(&query_data, &contexts, &overrides)
        .and_then(|x| serde_json::from_value(x))
        .map_err_to_string()?;
    merge_overrides_on_default_config(&mut default_config, overrides);
    let overriden_config = default_config;
    Ok(overriden_config)
}
