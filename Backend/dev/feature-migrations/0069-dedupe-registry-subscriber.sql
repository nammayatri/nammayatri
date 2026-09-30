-- Published dumps map the old/new rider-app BAP domains (and both MOBILITY gateways) to the same (subscriber_id, unique_key_id); keep only the newest row so registry lookups return one subscriber.
DELETE FROM atlas_registry.subscriber s
USING atlas_registry.subscriber t
WHERE s.subscriber_id = t.subscriber_id
  AND s.unique_key_id = t.unique_key_id
  AND (s.created, s.ctid) < (t.created, t.ctid);
