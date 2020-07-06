-- This is coppied from
-- https://bitbucket.org/juspay/passetto-hs/src/0c3162aec9569d25355b13c0de1efb1c23cc6f74/service/pgsql/create_schema.sql#create_schema.sql
-- and should be keeped synced with it.

-- SCHEMA: Passetto

CREATE SCHEMA IF NOT EXISTS "Passetto"
AUTHORIZATION passetto;

-- Table: Passetto.Master
CREATE TABLE IF NOT EXISTS "Passetto"."Master"
(
"key" bytea NOT NULL,
"updatedat" timestamp with time zone default now()
)

TABLESPACE pg_default;

ALTER TABLE "Passetto"."Master"
OWNER to passetto;

-- Table: Passetto.Keys
CREATE TABLE IF NOT EXISTS "Passetto"."Keys"
(
"id" serial PRIMARY KEY,
"encryptedkeypair" bytea NOT NULL,
"createdat" timestamp with time zone default now()
)

TABLESPACE pg_default;

ALTER TABLE "Passetto"."Keys"
OWNER to passetto;
