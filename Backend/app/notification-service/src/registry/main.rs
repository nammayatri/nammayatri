use axum::Json;
use hyper::HeaderMap;
use notification_service::notification::{
    notification_stream_client::NotificationStreamClient, Notification,
};
use prometheus::{HistogramOpts, HistogramVec, IntCounterVec, Opts, Registry};
use redis::{Client, Commands};
use serde::Deserialize;
use tonic::{metadata::MetadataValue, Request};
use tracing::error;

#[allow(clippy::expect_used)]
pub static CLIENT_MESSAGE_COLLECTOR: once_cell::sync::Lazy<IntCounterVec> =
    once_cell::sync::Lazy::new(|| {
        IntCounterVec::new(
            Opts::new("client_message", "Client Messages"),
            &["clientid", "messageid"],
        )
        .expect("client message collector metric couldn't be created")
    });
#[allow(clippy::expect_used)]
pub static CLIENT_MESSAGE_STATUS_COLLECTOR: once_cell::sync::Lazy<HistogramVec> =
    once_cell::sync::Lazy::new(|| {
        HistogramVec::new(
            HistogramOpts::new("client_message_status", "Client Messages Status"),
            &["clientid", "messageid", "status"],
        )
        .expect("client message collector metric couldn't be created")
    });
pub static REGISTRY: once_cell::sync::Lazy<Registry> = once_cell::sync::Lazy::new(Registry::new);

fn register_custom_metrics() {
    #[allow(clippy::expect_used)]
    REGISTRY
        .register(Box::new(CLIENT_MESSAGE_COLLECTOR.clone()))
        .expect("CLIENT_MESSAGE_COLLECTOR could not be registered");

    #[allow(clippy::expect_used)]
    REGISTRY
        .register(Box::new(CLIENT_MESSAGE_STATUS_COLLECTOR.clone()))
        .expect("CLIENT_MESSAGE_STATUS_COLLECTOR could not be registered");
}

async fn metrics_handler() -> Result<String, String> {
    use prometheus::Encoder;
    let encoder = prometheus::TextEncoder::new();

    let mut buffer = Vec::new();
    if let Err(error) = encoder.encode(&REGISTRY.gather(), &mut buffer) {
        error!(%error, "could not encode custom metrics");
    };
    let mut res = match String::from_utf8(buffer.clone()) {
        Ok(v) => v,
        Err(error) => {
            error!(%error, "custom metrics could not be converted from bytes");
            String::default()
        }
    };
    buffer.clear();

    let mut buffer = Vec::new();
    if let Err(error) = encoder.encode(&prometheus::gather(), &mut buffer) {
        error!(%error, "could not encode prometheus metrics");
    };
    let res_custom = match String::from_utf8(buffer.clone()) {
        Ok(v) => v,
        Err(error) => {
            error!(%error, "prometheus metrics could not be converted from bytes");
            String::default()
        }
    };
    buffer.clear();

    res.push_str(&res_custom);
    Ok(res)
}

#[derive(Debug, Deserialize, Clone, Default)]
#[serde(default)]
struct NotificationRequest {
    device_token: String,
    notification_type: String,
    data: String,
    ttl: i32,
}

async fn notification_handler(
    headers: HeaderMap,
    Json(req): Json<NotificationRequest>,
) -> Result<String, String> {
    let client_version = headers
        .get("x-client-version")
        .expect("[x-client-version] is missing in headers")
        .to_str()
        .unwrap();
    let bundle_version = headers
        .get("x-bundle-version")
        .expect("[x-bundle-version] is missing in headers")
        .to_str()
        .unwrap();

    let redis_client = Client::open("redis://127.0.0.1/").unwrap();

    let mut redis_conn = redis_client.get_connection().unwrap();

    let server_ip: String = redis_conn.get(&req.device_token).unwrap();

    let mut server = NotificationStreamClient::connect(server_ip).await.unwrap();

    let notification = Notification {
        device_token: req.device_token,
        notification_type: req.notification_type,
        data: req.data,
        ttl: req.ttl,
    };

    let mut request = Request::new(notification);
    request.metadata_mut().insert(
        "x-client-version",
        MetadataValue::try_from(client_version).unwrap(),
    );
    request.metadata_mut().insert(
        "x-bundle-version",
        MetadataValue::try_from(bundle_version).unwrap(),
    );

    server.send_notification(request).await.unwrap();

    Ok(String::from("AckResponse"))
}

#[derive(Debug, Deserialize, Clone, Default)]
#[serde(default)]
struct RegisterRequest {
    device_token: String,
    server_ip: String,
    ttl: usize
}

async fn register_handler(Json(req): Json<RegisterRequest>) -> Result<String, String> {
    let redis_client = Client::open("redis://127.0.0.1/").unwrap();
    let mut redis_conn = redis_client.get_connection().unwrap();
    let _result: redis::Value = redis_conn.set_ex(req.device_token, req.server_ip, req.ttl).unwrap();

    Ok(String::from("AckResponse"))
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let app = axum::Router::new()
        .route("/metrics", axum::routing::get(metrics_handler))
        .route("/notification", axum::routing::post(notification_handler))
        .route("/register", axum::routing::post(register_handler))
        .into_make_service();

    axum::Server::bind(&"0.0.0.0:5050".parse().unwrap())
        .serve(app)
        .await
        .unwrap();

    let _guard = notification_service::setup_tracing(std::env!("CARGO_BIN_NAME"));

    register_custom_metrics();

    Ok(())
}
