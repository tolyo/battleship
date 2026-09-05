-- +goose Up
-- +goose StatementBegin
CREATE OR REPLACE FUNCTION register_user (
    username_param TEXT,
    email_param TEXT,
    password_param TEXT
)
RETURNS UUID
LANGUAGE 'plpgsql'
AS $$
DECLARE
    user_instance users%ROWTYPE;
    new_user_id UUID;
BEGIN
    SELECT *
    INTO user_instance
    FROM users
    WHERE username = username_param
        OR email = email_param;

    IF FOUND THEN
        RAISE EXCEPTION 'user already registered';
    END IF;

    INSERT INTO users (username, email, password_hash)
    VALUES (username_param, email_param, password_param)
    RETURNING id INTO new_user_id;

    RETURN new_user_id;
END;
$$;

-- +goose StatementEnd
-- +goose Down
DROP FUNCTION register_user (TEXT, TEXT, TEXT);
