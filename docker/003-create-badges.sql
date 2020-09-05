CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE IF NOT EXISTS badges (
    link_id uuid UNIQUE DEFAULT UUID_GENERATE_V4 (),
    username text REFERENCES users (username),
    project text REFERENCES projects (name),
    PRIMARY KEY (username, project)
);

