-- +goose Up
-- +goose StatementBegin

CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TABLE users
(
    id                  UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    username            VARCHAR(32) UNIQUE NOT NULL,
    email               VARCHAR(255) UNIQUE NOT NULL,
    password_hash       TEXT NOT NULL,
    rating              INT DEFAULT 1200, -- starting rating like chess
    created_at          TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
-- +goose StatementEnd

-- +goose StatementBegin
CREATE OR REPLACE FUNCTION encrypt_password_trigger()
RETURNS TRIGGER AS $$
BEGIN
    NEW.password_hash := crypt(NEW.password_hash, gen_salt('bf'));
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;
-- +goose StatementEnd

-- +goose StatementBegin
CREATE TRIGGER before_insert_encrypt_password
BEFORE INSERT ON users
FOR EACH ROW
EXECUTE FUNCTION encrypt_password_trigger();
-- +goose StatementEnd

-- +goose Down
DROP TRIGGER IF EXISTS before_insert_encrypt_password ON users;
DROP FUNCTION IF EXISTS  encrypt_password_trigger();
DROP TABLE IF EXISTS users;