mod eval;
mod utils;

#[macro_use]
extern crate lazy_static;

use actix_web::{
    rt::{time::interval},
    web::Data
};
use chrono::{DateTime, Utc};
use derive_more::{Deref, DerefMut};
use reqwest::{RequestBuilder, Response, StatusCode};
use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Value};
use std::{
    collections::HashMap,
    convert::identity,
    sync::{Arc, RwLock},
    time::{Duration, UNIX_EPOCH, SystemTime},
};

use utils::core::MapError;
use tokio::sync::oneshot;
use std::ffi::{CStr, CString};
use std::os::raw::{c_char, c_uint, c_ulonglong, c_int};
use tokio::task::LocalSet;

use superposition_client as sp;
use tokio::runtime::Runtime;

type FutureId = u64;

type SyncMap<K, V> = RwLock<HashMap<K, V>>;
// let local_set = LocalSet::new();

lazy_static! {
    // Map for feedback channels. Once result is computed, it is stored at `RESULTS`
    static ref STATUSES: SyncMap<FutureId, oneshot::Receiver<&'static str>> = SyncMap::default();
    // Cache storage for results
    static ref RESULTS: SyncMap<FutureId, &'static str> = SyncMap::default();
}

fn gen_unique_id() -> u64 {
    let start = SystemTime::now();
    let since_the_epoch = start.duration_since(UNIX_EPOCH)
        .expect("Time went backwards");
    since_the_epoch.as_millis() as u64
}

fn convert_c_str_to_rust_str(c_str: *const c_char) -> String {
    unsafe {
        CStr::from_ptr(c_str).to_string_lossy().into_owned()
    }
}

fn convert_c_array_to_vec(c_array: *const *const c_char, count: c_uint) -> Vec<String> {
    (0..count).map(|i| {
        let c_str = unsafe { *c_array.offset(i as isize) };
        convert_c_str_to_rust_str(c_str)
    }).collect()
}

fn convert_to_rust_duration(secs: c_ulonglong, nanos: c_uint) -> std::time::Duration {
    std::time::Duration::new(secs, nanos)
}

#[no_mangle]
pub extern "C" fn init_cac_clients(hostname: *const c_char, polling_interval_secs : c_ulonglong, update_cac_periodically : bool, tenants : *const *const c_char, tenants_count: c_uint) -> *const u64 {
    let (tx, rx) = oneshot::channel();
    let cac_hostname = convert_c_str_to_rust_str(hostname);
    let polling_interval = convert_to_rust_duration(polling_interval_secs, 0);
    let cac_tenants = convert_c_array_to_vec(tenants, tenants_count);


    let rt = Runtime::new().unwrap();

    // Spawn an async task
    rt.block_on(async {
        println!("Async task started!");

        for tenant in cac_tenants {
            println!("Async task started! 2");
            CLIENT_FACTORY
                .create_client(
                    tenant.to_string(),
                    update_cac_periodically,
                    polling_interval,
                    cac_hostname.to_string(),
                )
                .await
                .expect(format!("{}: Failed to acquire cac_client", tenant).as_str());
        }
        tx.send("CLIENTS_CREATED").unwrap();
        println!("Async task completed!");
    });
    let id = gen_unique_id();
    STATUSES.write().unwrap().insert(id, rx);
    &id
}

#[no_mangle]
pub extern "C" fn init_superposition_clients(hostname: *const c_char, polling_frequency : c_ulonglong, tenants : *const *const c_char, tenants_count: c_uint) -> *const u64 {
    let (tx, rx) = oneshot::channel();
    let hostname = convert_c_str_to_rust_str(hostname);
    let poll_frequency = polling_frequency as u64;
    let cac_tenants = convert_c_array_to_vec(tenants, tenants_count);
    let rt = Runtime::new().unwrap();
    rt.block_on(async {
        println!("Async task started!");

        for tenant in cac_tenants {
                sp::CLIENT_FACTORY
                    .create_client(tenant.to_string(), poll_frequency, hostname.to_string())
                    .await
                    .expect(format!("{}: Failed to acquire superposition_client", tenant).as_str());
                    // .clone().run_polling_updates().await;;
                // println!("Client {:?}", client);
                // tokio::spawn(async{
                //     client.run_polling_updates().await;
                // });
        }
        tx.send("CLIENTS_CREATED").unwrap();

        println!("Async task completed!");
    });
    let id = gen_unique_id();
    STATUSES.write().unwrap().insert(id, rx);
    println!("Superposition complete! 1");
    &id
}

#[no_mangle]
pub extern "C" fn run_polling_updates() {
    let rt = Runtime::new().unwrap();
    let tenant = "mjos".to_string();
    rt.block_on(async {
        println!("Async task polling started!");
        let sp_client = sp::CLIENT_FACTORY
            .get_client(tenant.clone())
            .await
            .map_err(|e| {
                log::error!("{}: {}", tenant, e);
                format!("{}: Failed to get cac client", tenant)
            }).expect("Failed to get superposition client");
        sp_client.run_polling_updates().await;
        println!("Async task polling completed!");
    });
}

fn serialize_map_to_json(map: &Map<String, Value>) -> Result<CString, serde_json::Error> {
    serde_json::to_string(map)
        .map(|json_str| CString::new(json_str).expect("CString::new failed"))
}

#[no_mangle]
pub extern "C" fn eval_ctx(c_tenant: *const c_char, ctx_json: *const c_char) -> *const c_char {
    let tenant = convert_c_str_to_rust_str(c_tenant);
    let cac_client = CLIENT_FACTORY.get_client(tenant.clone()).map_err(|e| {
        log::error!("{}: {}", tenant, e);
        format!("{}: Failed to get cac client", tenant)
    }).expect("Failed to get cac client");
    let ctx_str = unsafe { CStr::from_ptr(ctx_json).to_str().unwrap() };
    let ctx_obj: Map<String, Value> = serde_json::from_str(ctx_str).unwrap();
    let overrides = cac_client.eval(ctx_obj).expect("Failed to fetch the context");
    let searialized_string = serialize_map_to_json(&overrides).expect("JSON serialization failed");
    println!("JSON: {:?}", searialized_string);
    let c_string = CString::new(searialized_string).expect("Failed to create CString");
    println!("JSON: {:?}", c_string);
    return c_string.into_raw();

}

fn  c_char_to_json(ptr: *const c_char) -> Result<Value, String> {
    // Ensure the pointer is not null
    assert!(!ptr.is_null());

    // Convert to CStr and then to a Rust String
    let c_str = unsafe { CStr::from_ptr(ptr) };
    let result = c_str.to_str().map_err(|e| e.to_string());

    match result {
        Ok(str_slice) => {
            // Now `str_slice` is an &str
            serde_json::from_str(str_slice).map_err(|e| e.to_string())
        }
        Err(e) => Err(e.to_string()), // Convert the error to YourErrorType
    }

}

#[no_mangle]
pub extern "C" fn eval_experiment(c_tenant: *const c_char, context: *const c_char, toss: c_int) -> *const c_char {
    println!("context1 {:?}", context);
    let ctx_str = c_char_to_json(context).expect("Failed to parse the context");
    println!("context2 {:?}", ctx_str);
    let toss_value = toss as i8;
    let rt = Runtime::new().unwrap();
    let tenant = convert_c_str_to_rust_str(c_tenant);
    let sp_client = rt.block_on (async{sp::CLIENT_FACTORY
        .get_client(tenant.clone())
        .await
        .map_err(|e| {
            log::error!("{}: {}", tenant, e);
            format!("{}: Failed to get cac client", tenant)
        })}).expect("Failed to get superposition client");
    println!("sp_client here {:?}", sp_client);
    let variant_ids = rt.block_on(async{sp_client.get_applicable_variant(&ctx_str, toss_value).await});
    println!("variant being applied {:?} and context {:?}", variant_ids, ctx_str);
    let cac_client = CLIENT_FACTORY.get_client(tenant.clone()).map_err(|e| {
        log::error!("{}: {}", tenant, e);
        format!("{}: Failed to get cac client", tenant)
    }).expect("Failed to get cac client");
    let mut ctx: serde_json::Map<String, Value> = serde_json::Map::new();
    ctx.insert(String::from("variantIds"), json!(variant_ids));
    ctx.insert(String::from("Os"), json!("Linux"));
    println!("context map {:?}", ctx);
    let overrides = cac_client.eval(ctx).expect("Failed to evaluate the context");
    println!("overrides data {:?}", overrides);
    let searialized_string = serialize_map_to_json(&overrides).expect("JSON serialization failed");
    println!("JSON: {:?}", searialized_string);
    let c_string = CString::new(searialized_string).expect("Failed to create CString");
    println!("JSON: {:?}", c_string);
    return c_string.into_raw();
    // return context
}

#[no_mangle]
pub extern "C" fn free_json_data(s: *mut c_char) {
    unsafe {
        if s.is_null() { return }
        drop(CString::from_raw(s))
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Context {
    pub condition: Value,
    pub override_with_keys: [String; 1],
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Config {
    contexts: Vec<Context>,
    overrides: Map<String, Value>,
    default_configs: Map<String, Value>,
}

#[derive(Clone)]
pub struct Client {
    tenant: String,
    reqw: Data<reqwest::RequestBuilder>,
    polling_interval: Duration,
    last_modified: Data<RwLock<DateTime<Utc>>>,
    config: Data<RwLock<Config>>,
}

fn clone_reqw(reqw: &RequestBuilder) -> Result<RequestBuilder, String> {
    reqw.try_clone()
        .ok_or_else(|| "Unable to clone reqw".to_string())
}

fn get_last_modified(resp: &Response) -> Option<DateTime<Utc>> {
    resp.headers().get("last-modified").and_then(|header_val| {
        let header_str = header_val.to_str().ok()?;
        DateTime::parse_from_rfc2822(header_str)
            .map(|datetime| datetime.with_timezone(&Utc))
            .map_err(|e| {
                log::error!("Failed to parse date: {e}");
            })
            .ok()
    })
}

impl Client {
    pub async fn new(
        tenant: String,
        update_config_periodically: bool,
        polling_interval: Duration,
        hostname: String,
    ) -> Result<Self, String> {
        let reqw_client = reqwest::Client::builder().build().map_err_to_string()?;
        let cac_endpoint = format!("{hostname}/config");
        let reqw = reqw_client
            .get(cac_endpoint)
            .header("x-tenant", tenant.to_string());

        let reqwc = clone_reqw(&reqw)?;
        let resp = reqwc.send().await.map_err_to_string()?;
        let last_modified_at = get_last_modified(&resp);
        let config = resp.json::<Config>().await.map_err_to_string()?;

        let client = Client {
            tenant,
            reqw: Data::new(reqw),
            polling_interval,
            last_modified: Data::new(RwLock::new(
                last_modified_at.unwrap_or(DateTime::<Utc>::from(UNIX_EPOCH)),
            )),
            config: Data::new(RwLock::new(config)),
        };
        if update_config_periodically {
            client.clone().start_polling_update().await;
        }
        Ok(client)
    }

    async fn fetch(&self) -> Result<reqwest::Response, String> {
        let last_modified = self.last_modified.read().map_err_to_string()?.to_rfc2822();
        let reqw = clone_reqw(&self.reqw)?.header("If-Modified-Since", last_modified);
        let resp = reqw.send().await.map_err_to_string()?;
        match resp.status() {
            StatusCode::NOT_MODIFIED => {
                return Err(String::from(format!(
                    "{} CAC: skipping update, remote not modified",
                    self.tenant
                )));
            }
            StatusCode::OK => log::info!(
                "{}",
                format!("{} CAC: new config received, updating", self.tenant)
            ),
            x => return Err(format!("{} CAC: fetch failed, status: {}", self.tenant, x)),
        };
        Ok(resp)
    }

    async fn update_cac(&self) -> Result<String, String> {
        let fetched_config = self.fetch().await?;
        let mut config = self.config.write().map_err_to_string()?;
        let mut last_modified = self.last_modified.write().map_err_to_string()?;
        let last_modified_at = get_last_modified(&fetched_config);
        *config = fetched_config.json::<Config>().await.map_err_to_string()?;
        if let Some(val) = last_modified_at {
            *last_modified = val;
        }
        Ok(format!("{}: CAC updated successfully", self.tenant))
    }

    pub async fn start_polling_update(self) {
        let local_set = LocalSet::new();
        // Spawn a local task within the LocalSet
        local_set.spawn_local(async move {
            let mut interval = interval(self.polling_interval);
            loop {
                log::info!("yipiu");
                println!("Async task started! 3");
                interval.tick().await;
                self.update_cac().await.unwrap_or_else(identity);
                println!("Async task started! 4");
            }
        });
    }

    pub fn get_config(&self) -> Result<Config, String> {
        self.config.read().map(|c| c.clone()).map_err_to_string()
    }

    pub fn get_last_modified<E>(&'static self) -> Result<DateTime<Utc>, String> {
        self.last_modified.read().map(|t| *t).map_err_to_string()
    }

    pub fn eval(
        &self,
        query_data: Map<String, Value>,
    ) -> Result<Map<String, Value>, String> {
        let cac = self.config.read().map_err_to_string()?;
        eval::eval_cac(
            cac.default_configs.to_owned(),
            &cac.contexts,
            &cac.overrides,
            &query_data,
        )
    }
}

#[derive(Deref, DerefMut)]
pub struct ClientFactory(RwLock<HashMap<String, Arc<Client>>>);
impl ClientFactory {
    pub async fn create_client(
        &self,
        tenant: String,
        update_config_periodically: bool,
        polling_interval: Duration,
        hostname: String,
    ) -> Result<Arc<Client>, String> {
        let mut factory = match self.write() {
            Ok(factory) => factory,
            Err(e) => {
                log::error!("CAC_CLIENT_FACTORY: failed to acquire write lock {}", e);
                return Err("CAC_CLIENT_FACTORY: Failed to create client".to_string());
            }
        };

        if let Some(client) = factory.get(&tenant) {
            return Ok(client.clone());
        }

        let client = Arc::new(
            Client::new(
                tenant.to_string(),
                update_config_periodically,
                polling_interval,
                hostname,
            )
            .await?,
        );
        factory.insert(tenant.to_string(), client.clone());
        return Ok(client.clone());
    }

    pub fn get_client(&self, tenant: String) -> Result<Arc<Client>, String> {
        let factory = match self.read() {
            Ok(factory) => factory,
            Err(e) => {
                log::error!("CAC_CLIENT_FACTORY: failed to acquire read lock {}", e);
                return Err("CAC_CLIENT_FACTORY: Failed to acquire client.".to_string());
            }
        };

        match factory.get(&tenant) {
            Some(client) => Ok(client.clone()),
            None => Err("No such tenant found".to_string()),
        }
    }
}

use once_cell::sync::Lazy;
pub static CLIENT_FACTORY: Lazy<ClientFactory> =
    Lazy::new(|| ClientFactory(RwLock::new(HashMap::new())));

pub use eval::eval_cac;
