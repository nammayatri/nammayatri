-- {"api":"PostTdsDistributionBatch","migration":"capability","param":"finance.tds_distribution.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.tds_distribution.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/TDS_DISTRIBUTION/POST_TDS_DISTRIBUTION_BATCH' ) ON CONFLICT DO NOTHING;

-- {"api":"PostTdsDistributionBatchValidate","migration":"capability","param":"finance.tds_distribution.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.tds_distribution.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/TDS_DISTRIBUTION/POST_TDS_DISTRIBUTION_BATCH_VALIDATE' ) ON CONFLICT DO NOTHING;

-- {"api":"GetTdsDistributionBatch","migration":"capability","param":"finance.tds_distribution.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.tds_distribution.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/TDS_DISTRIBUTION/GET_TDS_DISTRIBUTION_BATCH' ) ON CONFLICT DO NOTHING;

-- {"api":"GetTdsDistributionBatchFiles","migration":"capability","param":"finance.tds_distribution.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.tds_distribution.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/TDS_DISTRIBUTION/GET_TDS_DISTRIBUTION_BATCH_FILES' ) ON CONFLICT DO NOTHING;

-- {"api":"PostTdsDistributionBatchCancel","migration":"capability","param":"finance.tds_distribution.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.tds_distribution.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/TDS_DISTRIBUTION/POST_TDS_DISTRIBUTION_BATCH_CANCEL' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostTdsDistributionBatchConfirm","migration":"capability","param":"finance.tds_distribution.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.tds_distribution.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/TDS_DISTRIBUTION/POST_TDS_DISTRIBUTION_BATCH_CONFIRM' ) ON CONFLICT DO NOTHING;

-- {"api":"GetTdsDistributionBatchRecords","migration":"capability","param":"finance.tds_distribution.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.tds_distribution.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/TDS_DISTRIBUTION/GET_TDS_DISTRIBUTION_BATCH_RECORDS' ) ON CONFLICT DO NOTHING;

-- {"api":"PostTdsDistributionBatchRetryFailed","migration":"capability","param":"finance.tds_distribution.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.tds_distribution.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/TDS_DISTRIBUTION/POST_TDS_DISTRIBUTION_BATCH_RETRY_FAILED' ) ON CONFLICT DO NOTHING;

-- {"api":"GetTdsDistributionBatches","migration":"capability","param":"finance.tds_distribution.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.tds_distribution.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/TDS_DISTRIBUTION/GET_TDS_DISTRIBUTION_BATCHES' ) ON CONFLICT DO NOTHING;

-- {"api":"GetTdsDistributionSummary","migration":"capability","param":"finance.tds_distribution.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.tds_distribution.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/TDS_DISTRIBUTION/GET_TDS_DISTRIBUTION_SUMMARY' ) ON CONFLICT DO NOTHING;

-- {"api":"GetTdsDistributionPersonCertificates","migration":"capability","param":"finance.tds_distribution.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.tds_distribution.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/TDS_DISTRIBUTION/GET_TDS_DISTRIBUTION_PERSON_CERTIFICATES' ) ON CONFLICT DO NOTHING;

-- {"api":"GetTdsDistributionRecordDownloadUrl","migration":"capability","param":"finance.tds_distribution.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.tds_distribution.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/TDS_DISTRIBUTION/GET_TDS_DISTRIBUTION_RECORD_DOWNLOAD_URL' ) ON CONFLICT DO NOTHING;

-- {"api":"PostTdsDistributionRecordRetry","migration":"capability","param":"finance.tds_distribution.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.tds_distribution.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/TDS_DISTRIBUTION/POST_TDS_DISTRIBUTION_RECORD_RETRY' ) ON CONFLICT DO NOTHING;
