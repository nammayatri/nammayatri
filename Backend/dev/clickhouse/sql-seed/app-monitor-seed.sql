CREATE TABLE app_monitor.json_logic_transactions (
    `transactionId` String,
    `domain` String,
    `timestamp` DateTime,
    `inputData` String,
    `logic` String,
    `outputData` String,
    `caller_app` String
) ENGINE = MergeTree()
ORDER BY (timestamp, domain, transactionId);
