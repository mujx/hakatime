BEGIN;
    CREATE TABLE IF NOT EXISTS users (
        username TEXT PRIMARY KEY,
        hashed_password BYTEA NOT NULL,
        salt_used BYTEA NOT NULL
    );

    CREATE TABLE IF NOT EXISTS auth_tokens (
        token TEXT PRIMARY KEY,
        owner TEXT REFERENCES users(username),
        token_expiry TIMESTAMP,
        last_usage TIMESTAMP
    );

    CREATE TABLE IF NOT EXISTS refresh_tokens (
        refresh_token TEXT PRIMARY KEY,
        owner TEXT REFERENCES users(username),
        token_expiry TIMESTAMP
    );

    CREATE TABLE IF NOT EXISTS projects (
        name TEXT PRIMARY KEY,
        description TEXT,
        owner TEXT REFERENCES users(username),
        dependencies TEXT [],
        repository TEXT
    );

    CREATE TABLE IF NOT EXISTS heartbeats (
        id SERIAL PRIMARY KEY,
        editor TEXT,
        plugin TEXT,
        platform TEXT,
        machine TEXT,
        sender TEXT REFERENCES users(username),
        user_agent TEXT,
        branch TEXT,
        category TEXT,
        cursorpos TEXT,
        dependencies TEXT [],
        entity TEXT NOT NULL,
        is_write BOOL,
        language TEXT,
        lineno INT,
        file_lines INT,
        project TEXT REFERENCES projects(name),
        ty TEXT NOT NULL,
        time_sent TIMESTAMP NOT NULL
    );

COMMIT;
