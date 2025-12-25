WITH PersonStats AS (
  SELECT T1.id,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  now()
  FROM atlas_app.person AS T1
)
INSERT INTO atlas_app.person_stats (
    person_id,
    user_cancelled_rides,
    driver_cancelled_rides,
    completed_rides,
    weekend_rides,
    weekday_rides,
    off_peak_rides,
    evening_peak_rides,
    morning_peak_rides,
    weekend_peak_rides,
    updated_at
)
  (SELECT * FROM PersonStats);
