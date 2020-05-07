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

DROP TABLE IF EXISTS `customer`;

CREATE TABLE `customer` (
  `id` char(36) NOT NULL,
  `reference_id` varchar(36) NOT NULL,
  `name` varchar(255) NOT NULL,
  `mobile_number` varchar(1024) NOT NULL,
  `info` Text NULL,
  `created_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  `updated_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  PRIMARY KEY (`id`),
  INDEX (`mobile_number`),
  INDEX (`reference_id`)
)

DROP TABLE IF EXISTS `driver`;

CREATE TABLE `driver`(
  `id` char(36) NOT NULL,
  `name` varchar(255) NOT NULL,
  `mobile_number` varchar(1024) NULL,
  `gender` varchar(32) NOT NULL,
  `experience` varchar(255) NULL,
  `rating` varchar(255) NULL,
  `no_of_trips` integer NULL,
  `description` Text NULL,
  `created_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  `updated_at` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  PRIMARY KEY (`id`),
)