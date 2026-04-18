CREATE TABLE app_monitor.json_logic_transactions (
    `transactionId` String,
    `domain` String,
    `timestamp` DateTime,
    `inputData` String,
    `logic` String,
    `outputData` String,
    `caller_app` String,
    `entityTransactionId` Nullable(String) DEFAULT NULL
) ENGINE = MergeTree()
ORDER BY (timestamp, domain, transactionId);
