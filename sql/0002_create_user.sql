-- +goose Up

-- +goose StatementBegin
CREATE OR REPLACE FUNCTION
    register_user(
        username_param text,
        email_param text,
        password_param text
    )
    RETURNS UUID
    LANGUAGE 'plpgsql'
AS $$
DECLARE
    user_instance users%ROWTYPE;
    new_user_id UUID;
BEGIN
    SELECT * FROM users 
    WHERE username = username_param
    OR email = email_param
    INTO user_instance;

    IF FOUND THEN 
        RAISE EXCEPTION 'user already registered';
    END IF;

    INSERT INTO users (username, email, password_hash)
    VALUES (username_param, email_param, password_param)
    RETURNING id into new_user_id;

    RETURN new_user_id;
END;
$$;
-- +goose StatementEnd

-- +goose Down
DROP FUNCTION register_user(TEXT, TEXT, TEXT);