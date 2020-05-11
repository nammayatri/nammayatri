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

DROP TABLE IF EXISTS `case`;
CREATE TABLE `case` (
  `id` char(36) NOT NULL,
  `name` varchar(255) NULL,
  `description` varchar(1024) NULL,
  `short_id` varchar(36) NOT NULL,
  `industry` varchar(1024) NOT NULL,
  `type` varchar(255) NOT NULL,
  `exchange_type` varchar(255) NOT NULL,
  `status` varchar(255) NOT NULL,
  `start_time` datetime NOT NULL,
  `end_time` datetime NULL,
  `valid_till` datetime NOT NULL,
  `provider` varchar(255) NULL,
  `provider_type` varchar(255) NULL,
  `requestor` varchar(255)  NULL,
  `requestor_type` varchar(255) NULL,
  `parent_case_id` varchar(255) NULL,
  `from_location_id` varchar(36) NULL,
  `to_location_id` varchar(36) NULL,
  `udf1` varchar(255) NULL,
  `udf2` varchar(255) NULL,
  `udf3` varchar(255) NULL,
  `udf4` varchar(255) NULL,
  `udf5` varchar(255) NULL,
  `info` TEXT NULL,
  `created_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  `updated_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  PRIMARY KEY (`id`),
  INDEX (`short_id`),
  INDEX (`provider`),
  INDEX (`requestor`)
);

DROP TABLE IF EXISTS `case_product`;
CREATE TABLE `case_product` (
  `id` char(36) NOT NULL,
  `case_id` varchar(255) NOT NULL,
  `product_id` varchar(255) NOT NULL,
  `quantity` integer NOT NULL,
  `price` DECIMAL(8,2) NOT NULL,
  `status` varchar(255) NOT NULL,
  `info` TEXT NULL,
  `created_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  `updated_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  PRIMARY KEY (`id`),
  INDEX (`case_id`),
  INDEX (`product_id`)
);

DROP TABLE IF EXISTS `product`;
CREATE TABLE `product` (
  `id` char(36) NOT NULL,
  `name` varchar(255) NULL,
  `description` varchar(1024) NULL,
  `industry` varchar(1024) NOT NULL,
  `type` varchar(255) NOT NULL,
  `status` varchar(255) NOT NULL,
  `start_time` datetime NOT NULL,
  `end_time` datetime NULL,
  `valid_till` datetime NOT NULL,
  `price` DECIMAL(10,2) NOT NULL,
  `rating` varchar(255) NULL,
  `review` varchar(255)  NULL,
  `udf1` varchar(255) NULL,
  `udf2` varchar(255) NULL,
  `udf3` varchar(255) NULL,
  `udf4` varchar(255) NULL,
  `udf5` varchar(255) NULL,
  `info` TEXT NULL,
  `from_location_id` varchar(255) NULL,
  `to_location_id` varchar(255) NULL,
  `organization_id` varchar(255) NOT NULL,
  `created_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  `updated_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  PRIMARY KEY (`id`),
  INDEX (`organization_id`)
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

-- INSERT INTO organization (id, name, gstin, status, type, verified, location_id, description, mobile_number, from_time, to_time, api_key, callback_url, head_count, created_at, updated_at) VALUES ('1',"juspay",null, "PENDING_VERIFICATION", "TRANSPORTER", false, null, null, null, null,null,"iamfromjuspay",null,null,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP);