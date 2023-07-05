CREATE TABLE atlas_app.person_stats (
    person_id character(36) NOT NULL PRIMARY KEY REFERENCES atlas_app.person (id),
    user_cancelled_rides integer NOT NULL,
    driver_cancelled_rides integer NOT NULL,
    completed_rides integer NOT NULL,
    weekend_rides integer NOT NULL,
    weekday_rides integer NOT NULL,
    off_peak_rides integer NOT NULL,
    evening_peak_rides integer NOT NULL,
    morning_peak_rides integer NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

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
    updated_at
)
  (SELECT * FROM PersonStats);