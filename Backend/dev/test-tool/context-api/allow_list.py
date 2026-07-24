"""Allow-list config for /api/db/update and /api/db/select.

Each spec is keyed by the camelCase table name used in the API. The server uses
this to enforce:
  - which tables are addressable at all,
  - which column is the table's primary key (the only legal WHERE for /api/db/update),
  - which columns may be SET in /api/db/update,
  - which columns may be SELECT'd or filtered in /api/db/select.

Expanding the surface is a deliberate edit to this file. Keep it narrow.
"""


_SPECS = {
    "purchasedPass": {
        "schema": "atlas_app",
        "pg_table": "purchased_pass",
        "kv_table_name": "purchasedPass",
        "pk": "id",
        "updatable": {"status", "validTill", "startTime", "endTime"},
        "selectable": {"id", "status", "validTill", "startTime", "endTime", "riderId", "passType"},
        "filter": {"id", "riderId"},
    },
    "riderConfig": {
        "schema": "atlas_app",
        "pg_table": "rider_config",
        "kv_table_name": "riderConfig",
        "pk": "merchantOperatingCityId",
        "updatable": {
            "passStatusUpdateBatchSize",
            "passExpiryReminderDays",
            "remindEverydayUntilPassExpiry",
            "passExpiryReminderBatchSize",
            # cancellation-fee test toggles
            "immediateCaptureRiderCancellationFee",
            "immediateCaptureDriverCancellationFee",
            "settleCancellationFeeBeforeNextRide",
        },
        "selectable": {
            "merchantOperatingCityId",
            "passStatusUpdateBatchSize",
            "passExpiryReminderDays",
            "remindEverydayUntilPassExpiry",
            "passExpiryReminderBatchSize",
            "timeDiffFromUtc",
            "immediateCaptureRiderCancellationFee",
            "immediateCaptureDriverCancellationFee",
            "settleCancellationFeeBeforeNextRide",
        },
        "filter": {"merchantOperatingCityId"},
    },
    "transporterConfig": {
        "schema": "atlas_driver_offer_bpp",
        "pg_table": "transporter_config",
        "kv_table_name": "transporterConfig",
        "pk": "merchantOperatingCityId",
        "updatable": {
            # cancellation-fee test toggles
            "canAddCancellationFee",
            "cancellationFeePaymentMethodExceptions",
        },
        "selectable": {
            "merchantOperatingCityId",
            "canAddCancellationFee",
            "cancellationFeePaymentMethodExceptions",
        },
        "filter": {"merchantOperatingCityId"},
    },
    "merchantPushNotification": {
        "schema": "atlas_app",
        "pg_table": "merchant_push_notification",
        "kv_table_name": "merchantPushNotification",
        "pk": "id",
        "updatable": {"title", "body"},
        "selectable": {"id", "key", "title", "body", "merchantOperatingCityId"},
        "filter": {"id", "key", "merchantOperatingCityId"},
    },
    "schedulerJob": {
        "schema": "atlas_app",
        "pg_table": "scheduler_job",
        "kv_table_name": "schedulerJob",
        "pk": "id",
        "updatable": {"scheduledAt", "status"},
        "selectable": {
            "id",
            "jobType",
            "jobData",
            "scheduledAt",
            "status",
            "shardId",
            "parentJobId",
            "merchantId",
            "merchantOperatingCityId",
        },
        "filter": {"id", "jobType", "parentJobId", "status"},
    },
    "booking": {
        "schema": "atlas_app",
        "pg_table": "booking",
        "kv_table_name": "booking",
        "pk": "id",
        "updatable": {"status"},
        "selectable": {"id", "status", "riderId"},
        "filter": {"id", "riderId"},
    },
}


def get(table):
    return _SPECS.get(table)


def is_updatable(spec, column):
    return column in spec.get("updatable", set())


def is_selectable(spec, column):
    cols = spec.get("selectable", set())
    return column in cols or column == spec.get("pk")


def is_filter_column(spec, column):
    cols = spec.get("filter", set())
    return column in cols or column == spec.get("pk")
