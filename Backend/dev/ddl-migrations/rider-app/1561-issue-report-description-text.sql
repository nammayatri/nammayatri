-- Widen issue_report.description so long descriptions do not stall the drainer with a 22001 error.
ALTER TABLE atlas_app.issue_report ALTER COLUMN description TYPE text;
