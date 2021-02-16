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
    name TEXT NOT NULL,
    description TEXT,
    owner TEXT REFERENCES users(username),
    dependencies TEXT [],
    repository TEXT,
    PRIMARY KEY (name, owner)
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
    project TEXT,
    ty TEXT NOT NULL,
    time_sent TIMESTAMP NOT NULL,
    CONSTRAINT unique_heartbeats UNIQUE (entity, sender, time_sent),
    CONSTRAINT heartbeats_sender_project_fkey FOREIGN KEY (sender, project) REFERENCES projects (owner, name)
);

CREATE INDEX IF NOT EXISTS datetime_idx ON heartbeats (time_sent);

CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE IF NOT EXISTS badges (
    link_id uuid UNIQUE DEFAULT UUID_GENERATE_V4 (),
    username text REFERENCES users (username),
    project text,
    PRIMARY KEY (username, project),
    CONSTRAINT badges_username_project_fkey FOREIGN KEY (username, project) REFERENCES projects (owner, name)
);

COMMIT;
