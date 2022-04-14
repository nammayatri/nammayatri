CREATE TABLE atlas_transporter.job (
  id character varying(255) PRIMARY KEY,
  job_type character varying(255) NOT NULL,
  job_data character varying(255) NOT NULL,
  scheduled_at timestamp NOT NULL,
  maximum_delay int,
  created_at timestamp NOT NULL,
  updated_at timestamp NOT NULL,
  max_errors int NOT NULL,
  curr_errors int NOT NULL,
  status character varying(255) NOT NULL
);
