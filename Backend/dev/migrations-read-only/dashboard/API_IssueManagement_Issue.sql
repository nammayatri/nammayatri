

-- {"api":"PutIssueUpdate","migration":"endpoint","param":"IssueAPI IssueUpdateEndpoint","schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/PUT_ISSUE_UPDATE'
  WHERE endpoint = 'IssueAPI IssueUpdateEndpoint';

-- {"api":"PostIssueComment","migration":"endpoint","param":"IssueAPI IssueAddCommentEndpoint","schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_COMMENT'
  WHERE endpoint = 'IssueAPI IssueAddCommentEndpoint';

-- {"api":"PostIssueCategoryCreate","migration":"endpoint","param":"IssueAPI CreateIssueCategoryEndpoint","schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CATEGORY_CREATE'
  WHERE endpoint = 'IssueAPI CreateIssueCategoryEndpoint';

-- {"api":"PostIssueCategoryUpdate","migration":"endpoint","param":"IssueAPI UpdateIssueCategoryEndpoint","schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CATEGORY_UPDATE'
  WHERE endpoint = 'IssueAPI UpdateIssueCategoryEndpoint';

-- {"api":"PostIssueOptionCreate","migration":"endpoint","param":"IssueAPI CreateIssueOptionEndpoint","schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_OPTION_CREATE'
  WHERE endpoint = 'IssueAPI CreateIssueOptionEndpoint';

-- {"api":"PostIssueOptionUpdate","migration":"endpoint","param":"IssueAPI UpdateIssueOptionEndpoint","schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_OPTION_UPDATE'
  WHERE endpoint = 'IssueAPI UpdateIssueOptionEndpoint';

-- {"api":"PostIssueMessageUpsert","migration":"endpoint","param":"IssueAPI UpsertIssueMessageEndpoint","schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_MESSAGE_UPSERT'
  WHERE endpoint = 'IssueAPI UpsertIssueMessageEndpoint';

------- SQL updates -------

-- {"api":"GetIssueApiIntegrationList","migration":"capability","param":"system-config.customer_issue_config.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.customer_issue_config.read', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_API_INTEGRATION_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"PostIssueApiIntegrationUpsert","migration":"capability","param":"system-config.customer_issue_config.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.customer_issue_config.write', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_API_INTEGRATION_UPSERT' ) ON CONFLICT DO NOTHING;

-- {"api":"PostIssueApiIntegrationDelete","migration":"capability","param":"system-config.customer_issue_config.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.customer_issue_config.write', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_API_INTEGRATION_DELETE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostIssueApiIntegrationTest","migration":"capability","param":"system-config.customer_issue_config.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.customer_issue_config.write', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_API_INTEGRATION_TEST' ) ON CONFLICT DO NOTHING;

-- {"api":"GetIssueFlowSimulate","migration":"capability","param":"system-config.customer_issue_config.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'system-config.customer_issue_config.read', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_FLOW_SIMULATE' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"GetIGMIssueTrail","migration":"capability","param":"city-operations.driver_issue.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.driver_issue.read', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/GET_IGM_ISSUE_TRAIL' ) ON CONFLICT DO NOTHING;

-- {"api":"PostIGMIssueTriggerActionUpdate","migration":"capability","param":"city-operations.driver_issue.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.driver_issue.write', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_IGM_ISSUE_TRIGGER_ACTION_UPDATE' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"GetIssueTicketDetails","migration":"capability","param":"city-operations.customer_issue.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.customer_issue.read', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_TICKET_DETAILS' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"GetIssueTicketDetails","migration":"capability","param":"city-operations.driver_issue.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.driver_issue.read', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_TICKET_DETAILS' ) ON CONFLICT DO NOTHING;
