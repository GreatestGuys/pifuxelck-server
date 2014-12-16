-- -----------------------------------------------------------------------------
-- Pifuxel Schema v1
--
-- This script will blow away existing tables and create new ones in there
-- place. To use:
--
--    mysql -h $HOST -u $USER --password $DB < schema.sql

DROP TABLE IF EXISTS Turns;
DROP TABLE IF EXISTS Games;

DROP TABLE IF EXISTS Sessions;
DROP TABLE IF EXISTS LoginChallenges;
DROP TABLE IF EXISTS Accounts;


CREATE TABLE Accounts (
  id                  INT(11)     NOT NULL AUTO_INCREMENT,
  key_exponent        BLOB        NOT NULL,
  key_modulus         BLOB        NOT NULL,
  display_name        VARCHAR(32) NOT NULL UNIQUE,
  hashed_phone_number CHAR(64),
  PRIMARY KEY (id)
);


CREATE TABLE LoginChallenges (
  id                  INT(11)     NOT NULL AUTO_INCREMENT,
  challenge           BLOB        NOT NULL,
  account_id          INT(11)     NOT NULL,
  created_at          TIMESTAMP   NOT NULL,

  PRIMARY KEY (id),
  FOREIGN KEY (account_id) REFERENCES Accounts (id)
);


CREATE TABLE Sessions (
  id                  INT(11)     NOT NULL AUTO_INCREMENT,
  auth_token          TEXT        NOT NULL,
  account_id          INT(11)     NOT NULL,
  created_at          TIMESTAMP   NOT NULL,

  PRIMARY KEY (id),
  FOREIGN KEY (account_id) REFERENCES Accounts (id)
);


CREATE TABLE Games (
  id                  INT(11)     NOT NULL AUTO_INCREMENT,
  completed_at        TIMESTAMP,

  PRIMARY KEY (id)
);


CREATE TABLE Turns (
  id                  INT(11)     NOT NULL AUTO_INCREMENT,
  account_id          INT(11)     NOT NULL,
  game_id             INT(11)     NOT NULL,
  is_complete         BOOLEAN     NOT NULL,
  is_drawing          BOOLEAN     NOT NULL,
  label               TEXT,
  drawing             TEXT,

  PRIMARY KEY (id),
  FOREIGN KEY (account_id) REFERENCES Accounts (id),
  FOREIGN KEY (game_id)    REFERENCES Games (id)
);
