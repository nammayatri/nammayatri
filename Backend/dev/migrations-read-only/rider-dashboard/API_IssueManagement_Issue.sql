

-- {"api":"PutIssueUpdate","migration":"endpoint","param":"IssueAPI IssueUpdateEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_ISSUE_MANAGEMENT/ISSUE/PUT_ISSUE_UPDATE'
  WHERE endpoint = 'IssueAPI IssueUpdateEndpoint';

-- {"api":"PostIssueComment","migration":"endpoint","param":"IssueAPI IssueAddCommentEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_COMMENT'
  WHERE endpoint = 'IssueAPI IssueAddCommentEndpoint';

-- {"api":"PostIssueCategoryCreate","migration":"endpoint","param":"IssueAPI CreateIssueCategoryEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CATEGORY_CREATE'
  WHERE endpoint = 'IssueAPI CreateIssueCategoryEndpoint';

-- {"api":"PostIssueCategoryUpdate","migration":"endpoint","param":"IssueAPI UpdateIssueCategoryEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CATEGORY_UPDATE'
  WHERE endpoint = 'IssueAPI UpdateIssueCategoryEndpoint';

-- {"api":"PostIssueOptionCreate","migration":"endpoint","param":"IssueAPI CreateIssueOptionEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_OPTION_CREATE'
  WHERE endpoint = 'IssueAPI CreateIssueOptionEndpoint';

-- {"api":"PostIssueOptionUpdate","migration":"endpoint","param":"IssueAPI UpdateIssueOptionEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_OPTION_UPDATE'
  WHERE endpoint = 'IssueAPI UpdateIssueOptionEndpoint';

-- {"api":"PostIssueMessageUpsert","migration":"endpoint","param":"IssueAPI UpsertIssueMessageEndpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_MESSAGE_UPSERT'
  WHERE endpoint = 'IssueAPI UpsertIssueMessageEndpoint';

------- SQL updates -------

-- {"api":"PostIssueKaptureCreate","migration":"endpoint","param":"IssueAPI CreateIssueReportV2Endpoint","schema":"atlas_bap_dashboard"}
UPDATE atlas_bap_dashboard.transaction
  SET endpoint = 'RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_KAPTURE_CREATE'
  WHERE endpoint = 'IssueAPI CreateIssueReportV2Endpoint';

------- SQL updates -------

-- {"api":"GetIssueApiIntegrationList","migration":"capability","param":"system-config.customer_issue_config.read","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.customer_issue_config.read', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_API_INTEGRATION_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"PostIssueApiIntegrationUpsert","migration":"capability","param":"system-config.customer_issue_config.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.customer_issue_config.write', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_API_INTEGRATION_UPSERT' ) ON CONFLICT DO NOTHING;

-- {"api":"PostIssueApiIntegrationDelete","migration":"capability","param":"system-config.customer_issue_config.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.customer_issue_config.write', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_API_INTEGRATION_DELETE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostIssueApiIntegrationTest","migration":"capability","param":"system-config.customer_issue_config.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.customer_issue_config.write', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_API_INTEGRATION_TEST' ) ON CONFLICT DO NOTHING;

-- {"api":"GetIssueFlowSimulate","migration":"capability","param":"system-config.customer_issue_config.read","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.customer_issue_config.read', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_FLOW_SIMULATE' ) ON CONFLICT DO NOTHING;
