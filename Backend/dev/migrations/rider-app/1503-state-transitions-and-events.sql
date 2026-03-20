UPDATE atlas_app.finance_state_transition
SET from_state = UPPER(from_state)
WHERE from_state != UPPER(from_state);

UPDATE atlas_app.finance_state_transition
SET to_state = UPPER(to_state)
WHERE to_state != UPPER(to_state);

UPDATE atlas_app.finance_state_transition
SET event = UPPER(event)
WHERE event != UPPER(event);
