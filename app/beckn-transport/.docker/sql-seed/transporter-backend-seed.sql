-- MySQL dump 10.13  Distrib 8.0.18, for Linux (x86_64)
--
-- Host: localhost    Database: atlasdb
-- ------------------------------------------------------
-- Server version	8.0.18
GRANT ALL PRIVILEGES ON atlas_transporter.* TO 'atlas'@'%';


DROP TABLE IF EXISTS `organization`;

CREATE TABLE `organization` (
  `id` char(36) NOT NULL,
  `name` varchar(255) NULL,
  `gstin` varchar(255) NULL,
  `status` varchar(255) NULL,
  `type` varchar(255) NULL,
  `verified` boolean NOT NULL,
  `location_id` varchar(255) NULL,
  `description` TEXT NULL,
  `mobile_number` TEXT NULL,
  `from_time` TEXT NULL,
  `to_time` TEXT NULL,
  `api_key` TEXT NULL,
  `callback_url` TEXT NULL,
  `head_count` integer NULL,
  `created_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  `updated_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  PRIMARY KEY (`id`)
);

DROP TABLE IF EXISTS `person`;

CREATE TABLE `person` (
  `id` char(36) NOT NULL,
  `first_name` varchar(255) NULL,
  `middle_name` varchar(255) NULL,
  `last_name` varchar(255) NULL,
  `full_name` varchar(255) NULL,
  `role` varchar(255) NOT NULL,
  `gender` varchar(255) NOT NULL,
  `identifier_type` varchar(255) NULL,
  `email` varchar(255) NULL,
  `mobile_number` varchar(255) NULL,
  `mobile_country_code` varchar(255) NULL,
  `identifier` varchar(255) NULL,
  `rating` varchar(255) NULL,
  `verified` boolean NOT NULL,
  `udf1` varchar(255) NULL,
  `udf2` varchar(255) NULL,
  `status` varchar(255) NOT NULL,
  `organization_id` varchar(255) NULL,
  `device_token` varchar(255) NULL,
  `location_id` varchar(255) NULL,
  `description` varchar(255) NULL,
  `created_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  `updated_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  PRIMARY KEY (`id`)
);

DROP TABLE IF EXISTS `registration_token`;
CREATE TABLE `registration_token` (
  `id` char(36) NOT NULL,
  `auth_medium` varchar(255) NOT NULL,
  `auth_type` varchar(255) NOT NULL,
  `auth_value_hash` varchar(1024) NOT NULL,
  `token` varchar(1024) NOT NULL,
  `verified` boolean NOT NULL,
  `auth_expiry` integer NOT NULL,
  `token_expiry` integer NOT NULL,
  `attempts` integer NOT NULL,
  `entity_id` char(36) NOT NULL,
  `entity_type` char(36) NOT NULL,
  `info` TEXT NULL,
  `created_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  `updated_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  PRIMARY KEY (`id`),
  INDEX (`entity_id`),
  INDEX (`entity_type`)
);

DROP TABLE IF EXISTS `location`;
CREATE TABLE `location` (
  `id` char(36) NOT NULL
  , `location_type` varchar(255) NULL
  , `lat` double NULL
  , `long` double NULL
  , `ward` varchar(255) NULL
  , `district` varchar(255) NULL
  , `city` varchar(255) NULL
  , `state` varchar(255) NULL
  , `country` varchar(255) NULL
  , `pincode` varchar(255) NULL
  , `address` varchar(255) NULL
  , `bound` varchar(255) NULL
  , `created_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP()
  , `updated_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP()
  , PRIMARY KEY (`id`)
  , INDEX (`city`)
  , INDEX (`state`)
);