-- One-time backfill of two_fa_mode after dev/ddl-migrations adds the (NULL-able) column.
-- Previously-exempt persons/roles (two_fa_exempt = true) adopt the MOBILE_OTP second factor under
-- the always-on model; everyone else takes AUTHENTICATOR_APP.
--
-- Rerun-safe by construction: it only touches rows whose two_fa_mode IS NULL, so once a row has any
-- value -- from this backfill, from the column's steady-state population, or from a later admin
-- change via /user/twoFaMode -- a re-run of this migration can never overwrite it. two_fa_exempt is
-- read only, never mutated.
UPDATE atlas_dashboard.person
  SET two_fa_mode = CASE WHEN two_fa_exempt = true THEN 'MOBILE_OTP' ELSE 'AUTHENTICATOR_APP' END
  WHERE two_fa_mode IS NULL;

UPDATE atlas_dashboard.role
  SET two_fa_mode = CASE WHEN two_fa_exempt = true THEN 'MOBILE_OTP' ELSE 'AUTHENTICATOR_APP' END
  WHERE two_fa_mode IS NULL;
