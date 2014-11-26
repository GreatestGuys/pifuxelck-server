-- -----------------------------------------------------------------------------
-- Pifuxel Schema v1
--
-- This script will blow away existing tables and create new ones in there
-- place. To use:
--
--    mysql -h $HOST -u $USER --password $DB < schema.sql


DROP TABLE IF EXISTS `Accounts`;
CREATE TABLE `Accounts` (
  `id`                  int(11)     NOT NULL AUTO_INCREMENT,
  `key_exponent`        blob        NOT NULL,
  `key_modulus`         blob        NOT NULL,
  `display_name`        varchar(32) NOT NULL,
  `hashed_phone_number` char(64),
  PRIMARY KEY (`id`)
);
