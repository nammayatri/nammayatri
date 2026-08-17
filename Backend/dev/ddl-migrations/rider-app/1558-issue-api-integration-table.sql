-- Declared HTTP lookups that in-house chat flows execute mid-conversation
-- (dashboard "API integrations"). headers_json / response_fields_json are JSON
-- blobs mirroring IssueManagement.Common.Dashboard.Issue.{ApiHeaderSpec,ResponseFieldSpec}.
-- kind = External (full URL + author headers) | InternalRider | InternalDriver
-- (endpoint path executed against the app's own /dashboard mount with the
-- config dashboard token attached).
CREATE TABLE IF NOT EXISTS atlas_app.issue_api_integration (
  id character(36) PRIMARY KEY,
  merchant_id character(36) NOT NULL,
  name text NOT NULL,
  description text,
  kind text NOT NULL DEFAULT 'External',
  method text NOT NULL,
  url_template text NOT NULL,
  headers_json text,
  body_template text,
  timeout_ms integer NOT NULL DEFAULT 3000,
  response_fields_json text NOT NULL,
  is_active boolean NOT NULL DEFAULT true,
  created_at timestamp with time zone NOT NULL DEFAULT now(),
  updated_at timestamp with time zone NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS idx_issue_api_integration_merchant_id ON atlas_app.issue_api_integration (merchant_id);
