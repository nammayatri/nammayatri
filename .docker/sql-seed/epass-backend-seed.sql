-- MySQL dump 10.13  Distrib 8.0.18, for Linux (x86_64)
--
-- Host: localhost    Database: atlasdb
-- ------------------------------------------------------
-- Server version	8.0.18
GRANT ALL PRIVILEGES ON atlas_epass.* TO 'atlas'@'%';


DROP TABLE IF EXISTS `customer`;
DROP TABLE IF EXISTS `customer_detail`;
DROP TABLE IF EXISTS `organization`;
DROP TABLE IF EXISTS `pass`;
DROP TABLE IF EXISTS `pass_application`;
DROP TABLE IF EXISTS `user`;
DROP TABLE IF EXISTS `location`;
DROP TABLE IF EXISTS `registration_token`;
DROP TABLE IF EXISTS `quota`;
DROP TABLE IF EXISTS `allocated_quota`;
DROP TABLE IF EXISTS `blacklist`;




CREATE TABLE `customer` (
  `id` char(36) NOT NULL,
  `name` varchar(255) NULL,
  `organization_id` char(36) NULL,
  `tenant_organization_id` char(36) NULL,
  `verified` boolean NOT NULL,
  `role` varchar(255) NOT NULL,
  `info` TEXT NULL,
  `created_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  `updated_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  PRIMARY KEY (`id`),
  INDEX (`organization_id`)
);

CREATE TABLE `customer_detail` (
  `id` char(36) NOT NULL,
  `customer_id` char(36) NOT NULL,
  `unique_identifier` varchar(255) NULL,
  `identifier_type` varchar(255) NULL,
  `value` TEXT NULL,
  `verified` boolean NOT NULL,
  `primary_identifier` boolean NOT NULL,
  `info` TEXT NULL,
  `created_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  `updated_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  PRIMARY KEY (`id`),
  INDEX (`customer_id`),
  INDEX (`unique_identifier`),
  INDEX (`identifier_type`)

);

CREATE TABLE `organization` (
  `id` char(36) NOT NULL,
  `name` varchar(255) NULL,
  `gstin` varchar(255) NULL,
  `status` varchar(255) NULL,
  `verified` boolean NOT NULL,
  `location_type` varchar(255) NULL,
  `lat` double NULL,
  `long` double NULL,
  `ward` varchar(255) NULL,
  `district` varchar(255) NULL,
  `city` varchar(255) NULL,
  `state` varchar(255) NULL,
  `country` varchar(255) NULL,
  `pincode` integer NULL,
  `address` varchar(1024) NULL,
  `bound` TEXT NULL,
  `info` TEXT NULL,
  `created_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  `updated_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  PRIMARY KEY (`id`),
  INDEX (`ward`),
  INDEX (`district`),
  INDEX (`city`),
  INDEX (`pincode`)
);

CREATE TABLE `pass` (
  `id` char(36) NOT NULL,
  `customer_id` char(36) NULL,
  `organization_id` char(36) NULL,
  `tenant_organization_id` char(36) NULL,
  `short_id` char(36) NOT NULL,
  `status` varchar(255) NULL,
  `from_date` datetime NOT NULL,
  `to_date` datetime NOT NULL,
  `pass_type` varchar(255) NULL,
  `pass_application_id` char(36) NOT NULL,
  `created_by` char(36) NOT NULL,
  `from_location_type` varchar(255) NULL,
  `from_lat` double NULL,
  `from_long` double NULL,
  `from_ward` varchar(255) NULL,
  `from_district` varchar(255) NULL,
  `from_city` varchar(255) NULL,
  `from_state` varchar(255) NULL,
  `from_country` varchar(255) NULL,
  `from_pincode` integer NULL,
  `from_address` varchar(1024) NULL,
  `from_bound` TEXT NULL,
  `to_location_type` varchar(255) NULL,
  `to_lat` double NULL,
  `to_long` double NULL,
  `to_ward` varchar(255) NULL,
  `to_district` varchar(255) NULL,
  `to_city` varchar(255) NULL,
  `to_state` varchar(255) NULL,
  `to_country` varchar(255) NULL,
  `to_pincode` integer NULL,
  `to_address` varchar(1024) NULL,
  `to_bound` TEXT NULL,
  `info` TEXT NULL,
  `created_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  `updated_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  PRIMARY KEY (`id`),
  INDEX (`customer_id`),
  INDEX (`organization_id`),
  INDEX (`pass_application_id`),
  INDEX (`pass_type`),
  INDEX (`status`),
  INDEX (`short_id`),
  INDEX (`from_ward`),
  INDEX (`from_district`),
  INDEX (`from_city`),
  INDEX (`from_pincode`),
  INDEX (`to_ward`),
  INDEX (`to_district`),
  INDEX (`to_city`),
  INDEX (`to_pincode`)
);


CREATE TABLE `pass_application` (
  `id` char(36) NOT NULL,
  `customer_id` char(36) NULL,
  `organization_id` char(36) NULL,
  `tenant_organization_id` char(36) NULL,
  `pass_type` varchar(255) NULL,
  `purpose` varchar(255) NULL,
  `from_date` datetime NOT NULL,
  `to_date` datetime NOT NULL,
  `count` integer NOT NULL,
  `approved_count` integer NOT NULL,
  `status` varchar(255) NULL,
  `assigned_to` char(36) NULL,
  `created_by` char(36) NULL,
  `remarks` varchar(255) NULL,
  `from_location_type` varchar(255) NULL,
  `from_lat` double NULL,
  `from_long` double NULL,
  `from_ward` varchar(255) NULL,
  `from_district` varchar(255) NULL,
  `from_city` varchar(255) NULL,
  `from_state` varchar(255) NULL,
  `from_country` varchar(255) NULL,
  `from_pincode` integer NULL,
  `from_address` varchar(1024) NULL,
  `from_bound` TEXT NULL,
  `to_location_type` varchar(255) NULL,
  `to_lat` double NULL,
  `to_long` double NULL,
  `to_ward` varchar(255) NULL,
  `to_district` varchar(255) NULL,
  `to_city` varchar(255) NULL,
  `to_state` varchar(255) NULL,
  `to_country` varchar(255) NULL,
  `to_pincode` integer NULL,
  `to_address` varchar(1024) NULL,
  `to_bound` TEXT NULL,
  `info` TEXT NULL,
  `created_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  `updated_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  PRIMARY KEY (`id`),
  INDEX (`customer_id`),
  INDEX (`assigned_to`),
  INDEX (`pass_type`),
  INDEX (`status`),
  INDEX (`from_ward`),
  INDEX (`from_district`),
  INDEX (`from_city`),
  INDEX (`from_pincode`),
  INDEX (`to_ward`),
  INDEX (`to_district`),
  INDEX (`to_city`),
  INDEX (`to_pincode`)
);

CREATE TABLE `user` (
  `id` char(36) NOT NULL,
  `organization_id` char(36) NOT NULL,
  `tenant_organization_id` char(36) NULL,
  `name` varchar(255) NOT NULL,
  `username` varchar(255) NOT NULL,
  `mobile_number` varchar(1024) NOT NULL,
  `email` varchar(255) NOT NULL,
  `role` varchar(255) NOT NULL,
  `status` varchar(255) NOT NULL,
  `verified` boolean NOT NULL,
  `info` TEXT NULL,
  `created_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  `updated_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  PRIMARY KEY (`id`),
  INDEX (`organization_id`),
  INDEX (`username`)
);


CREATE TABLE `location` (
  `id` char(36) NOT NULL,
  `location_type` varchar(255) NULL,
  `lat` double NULL,
  `long` double NULL,
  `ward` varchar(255) NULL,
  `district` varchar(255) NULL,
  `city` varchar(255) NULL,
  `state` varchar(255) NULL,
  `country` varchar(255) NULL,
  `pincode` integer NULL,
  `address` varchar(1024) NULL,
  `bound` TEXT NULL,
  `info` TEXT NULL,
  `created_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  `updated_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  PRIMARY KEY (`id`),
  INDEX (`location_type`)
);

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

CREATE TABLE `quota` (
  `id` char(36) NOT NULL,
  `quota_type` varchar(255) NOT NULL,
  `max_allowed` integer NOT NULL,
  `tenant_organization_id` char(36) NULL,
  `entity_id` char(36) NOT NULL,
  `entity_type` varchar(255) NOT NULL,
  `start_time` datetime NOT NULL,
  `end_time` datetime NOT NULL,
  `info` TEXT NULL,
  `created_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  `updated_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  PRIMARY KEY (`id`),
  INDEX (`entity_id`),
  INDEX (`entity_type`),
  INDEX (`start_time`),
  INDEX (`end_time`)
);

CREATE TABLE `allocated_quota` (
  `id` char(36) NOT NULL,
  `quota_id` char(36) NOT NULL,
  `allocated_count` integer NOT NULL,
  `version` integer NOT NULL,
  `start_time` datetime NOT NULL,
  `end_time` datetime NOT NULL,
  `info` TEXT NULL,
  `created_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  `updated_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  PRIMARY KEY (`id`),
  INDEX (`quota_id`),
  INDEX (`start_time`),
  INDEX (`end_time`)
);

CREATE TABLE `blacklist` (
  `id` char(36) NOT NULL,
  `blacklisted_by` char(36) NOT NULL,
  `tenant_organization_id` char(36) NULL,
  `remarks` varchar(255) NOT NULL,
  `entity_id` char(36) NOT NULL,
  `entity_type` varchar(255) NOT NULL,
  `start_time` datetime NOT NULL,
  `end_time` datetime NOT NULL,
  `info` TEXT NULL,
  `created_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  `updated_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  PRIMARY KEY (`id`),
  INDEX (`entity_id`),
  INDEX (`entity_type`),
  INDEX (`start_time`),
  INDEX (`end_time`),
  INDEX (`tenant_organization_id`),
  INDEX (`blacklisted_by`)
);

CREATE TABLE `tag` (
  `id` char(36) NOT NULL,
  `created_by` char(36) NOT NULL,
  `created_by_entity_type` varchar(255) NOT NULL,
  `tag_type` varchar(255) NOT NULL,
  `value` varchar(255) NOT NULL,
  `created_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  `updated_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  PRIMARY KEY (`id`),
  INDEX (`tag_type`),
  INDEX (`value`)
);

CREATE TABLE `entity_tag` (
  `id` char(36) NOT NULL,
  `tagged_by` char(36) NOT NULL,
  `tagged_by_entity_type` varchar(255) NOT NULL,
  `entity_id` char(36) NOT NULL,
  `entity_type` varchar(255) NOT NULL,
  `tag_id` char(36) NOT NULL,
  `created_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  `updated_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  PRIMARY KEY (`id`),
  INDEX (`entity_id`),
  INDEX (`entity_type`),
  INDEX (`tag_id`)
);


CREATE TABLE `document` (
  `id` char(36) NOT NULL,
  `file_url` varchar(1024) NOT NULL,
  `filename` varchar(255) NOT NULL,
  `format` varchar(255) NOT NULL,
  `size` integer NOT NULL,
  `file_hash` varchar(1024) NOT NULL,
  `info` TEXT NULL,
  `created_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  `updated_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  PRIMARY KEY (`id`)
);

CREATE TABLE `entity_document` (
  `id` char(36) NOT NULL,
  `entity_id` char(36) NOT NULL,
  `entity_type` varchar(255) NOT NULL,
  `document_id` char(36) NOT NULL,
  `document_type` varchar(255) NOT NULL,
  `created_by` char(36) NOT NULL,
  `created_by_entity_type` varchar(255) NOT NULL,
  `verified` boolean NOT NULL,
  `verified_by` char(36) NULL,
  `verified_by_entity_type` varchar(255) NULL,
  `info` TEXT NULL,
  `created_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  `updated_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  PRIMARY KEY (`id`),
  INDEX (`entity_id`),
  INDEX (`entity_type`),
  INDEX (`document_id`)
);

CREATE TABLE `comment` (
  `id` char(36) NOT NULL,
  `primary_entity_id` char(36) NOT NULL,
  `primary_entity_type` varchar(255) NOT NULL,
  `commented_by` char(36) NOT NULL,
  `commented_by_entity_type` varchar(255) NOT NULL,
  `value` varchar(1024) NOT NULL,
  `created_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  `updated_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  PRIMARY KEY (`id`),
  INDEX (`primary_entity_id`),
  INDEX (`primary_entity_type`)
);
