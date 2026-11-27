-- Two facts about a login session that both application servers need. They were Redis keys, but the
-- servers need not share a Redis; they do share this database.
--   is_otp_session: the session was opened by a phone OTP, so it is rechecked on every request.
--   selected_fleet_ids: the fleets the session has chosen to work on; NULL means all the person holds.
ALTER TABLE atlas_dashboard.registration_token
  ADD COLUMN IF NOT EXISTS is_otp_session boolean NOT NULL DEFAULT false,
  ADD COLUMN IF NOT EXISTS selected_fleet_ids text[];
