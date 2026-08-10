UPDATE atlas_app.issue_option
SET label = 'REOPEN_TICKET'
WHERE option = 'I need more help'
  AND (label IS NULL OR label = '');
