CREATE TABLE IF NOT EXISTS tags (
    id uuid UNIQUE DEFAULT UUID_GENERATE_V4 (),
    name text PRIMARY KEY
);

CREATE TABLE IF NOT EXISTS project_tags (
    project_name text NOT NULL,
    project_owner text NOT NULL,
    tag_id uuid REFERENCES tags (id),

    CONSTRAINT project_tags_pname_powner_fkey FOREIGN KEY (project_owner, project_name) REFERENCES projects (owner, name),
    PRIMARY KEY (project_name, project_owner, tag_id)
);
