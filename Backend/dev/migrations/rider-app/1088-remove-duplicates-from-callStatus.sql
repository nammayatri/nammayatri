DELETE FROM
 atlas_app.call_status dup_sid
USING  atlas_app.call_status dist_sid
WHERE dup_sid.created_at > dist_sid.created_at
AND dup_sid.call_id = dist_sid.call_id;

ALTER TABLE atlas_app.call_status ADD CONSTRAINT unique_call_sid UNIQUE (call_id);
