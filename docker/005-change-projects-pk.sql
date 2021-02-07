/*
 * Use composite PK on projects, to multiple users with the same project name.
 */

BEGIN;

ALTER TABLE badges
    DROP CONSTRAINT badges_project_fkey;

ALTER TABLE heartbeats
    DROP CONSTRAINT heartbeats_project_fkey;

ALTER TABLE projects
    DROP CONSTRAINT projects_pkey,
    ADD PRIMARY KEY (name, owner);

ALTER TABLE badges
    ADD CONSTRAINT badges_username_project_fkey
    FOREIGN KEY (username, project) REFERENCES projects (owner, name);

ALTER TABLE heartbeats
    ADD CONSTRAINT heartbeats_sender_project_fkey
    FOREIGN KEY (sender, project) REFERENCES projects (owner, name);

COMMIT;

