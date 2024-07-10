ALTER TABLE atlas_app.issue_message DROP CONSTRAINT issue_message_reference_category_id_fkey;

ALTER TABLE atlas_app.issue_message
ADD CONSTRAINT issue_message_reference_category_id_fkey
FOREIGN KEY (reference_category_id)
REFERENCES atlas_app.issue_category(id);

-- QUERIES FOR PROD AND MASTER (Fixing Priorities of Messages)
UPDATE atlas_app.issue_message SET priority = 2 WHERE id = 'nr1ufdqu-w3di-0jc3-xa3x-992a6sxuo1l2';
UPDATE atlas_app.issue_message SET priority = 2 WHERE id = '85zfqnuv-b92v-lycg-25i5-5kinvp9yqvvy';