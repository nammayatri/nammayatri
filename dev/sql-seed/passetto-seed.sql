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

INSERT INTO "Passetto"."Master" (key) values (decode('00000000000000302d476de40bb08e3d1b42bf232b7aae8a98609345cb5bc479e8a587065535806cc40c2113d333003202cb13f1f707583e0000000000000018bfe4d57672c4ca94a8ea6899b51c717578a518e346ad35fb00000000000000107df9a9cc38ee3e56f00a85c18e9e4a50', 'hex'));
INSERT INTO "Passetto"."Keys" (encryptedKeyPair) values(decode('0000000000000060876de70043e785ec2ab412a5e1cb10e5dd1ebcbf3f2342b8cfd58d56ae4b26e8843dbb181c4966ffb17c5023b1e8781d44c45f62bc83f5b54eb2541ae1cbea13dbe1ebe1d6fd981b1c24bb3d5726480682e64467d16124eb05818ef6e00d1f910000000000000018f7a67d7cd4578a2fa81555ac32b1caa41003770cc3a11be0', 'hex'));
INSERT INTO "Passetto"."Keys" (encryptedKeyPair) values(decode('00000000000000604a15b9e19523d67e2e92563988988d05370ee9351aee801b3b39a7189697af47e4c05c30801fd20166294ed4cc8ac4196316fbbe19e918597141a2a3331ac0aa6bba41cc6a27a9f637678d4bc3227d053f47c5eb8df2899069c768e0c3ad0f3e0000000000000018e5772b2fbd2c1cef9a2fd90d4abb823e3cb29600b538aaac', 'hex'));
INSERT INTO "Passetto"."Keys" (encryptedKeyPair) values(decode('000000000000006055def0dc30180d82833b2ca2491e17e4f38730d539b30145d17eae8544a41c4eda977d8c14af80f1a7ccd2dba076210869d5de2b884006f9f096363c1061ce98098d95d44d8d0e429191292b0a210468989cef34da4f6d4ce28f8b7e3c56a1d30000000000000018fbe28c341465b1de7871732e88767a0087364319446a3d9c', 'hex'));
