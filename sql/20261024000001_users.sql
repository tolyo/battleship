-- +terlan Up
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TABLE IF NOT EXISTS users (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4 (),
    username VARCHAR(32) UNIQUE NOT NULL,
    email VARCHAR(255) UNIQUE NOT NULL,
    password_hash TEXT NOT NULL,
    rating INT DEFAULT 1200, -- starting rating like chess
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE OR REPLACE FUNCTION encrypt_password_trigger ()
RETURNS TRIGGER
AS $$
BEGIN
    NEW.password_hash := crypt(NEW.password_hash, gen_salt('bf'));
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Replacing the trigger lets existing installations adopt Terlan history.
DROP TRIGGER IF EXISTS before_insert_encrypt_password ON users;

CREATE TRIGGER before_insert_encrypt_password
BEFORE INSERT ON users FOR EACH ROW
EXECUTE FUNCTION encrypt_password_trigger ();

-- +terlan Down
DROP TRIGGER IF EXISTS before_insert_encrypt_password ON users;

DROP FUNCTION IF EXISTS encrypt_password_trigger ();

DROP TABLE IF EXISTS users;
