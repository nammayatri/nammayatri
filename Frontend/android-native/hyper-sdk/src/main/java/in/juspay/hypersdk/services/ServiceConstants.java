package in.juspay.hypersdk.services;

final class ServiceConstants {
    static final String ASSET_METADATA_FILE_NAME = "asset_metadata.json";
    static final String ATTR_LAST_CHECKED = "lastChecked";
    static final String ATTR_ZIPHASH_IN_DISK = "zipHashInDisk";
    static final String KEY_REMOTE_ASSET_TTL = "REMOTE_ASSET_TTL_MILLISECONDS";

    static final long DEF_REMOTE_ASSET_TTL = 60 * 60 * 1000; // 1 hour
}
