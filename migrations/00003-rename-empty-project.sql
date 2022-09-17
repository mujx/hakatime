ALTER TABLE badges DROP CONSTRAINT badges_username_project_fkey;
ALTER TABLE heartbeats DROP CONSTRAINT heartbeats_sender_project_fkey;
ALTER TABLE project_tags DROP CONSTRAINT project_tags_pname_powner_fkey;

ALTER TABLE badges ADD CONSTRAINT badges_username_project_fkey FOREIGN KEY (username, project) REFERENCES projects (owner, name) ON UPDATE CASCADE;
ALTER TABLE heartbeats ADD CONSTRAINT heartbeats_sender_project_fkey FOREIGN KEY (sender, project) REFERENCES projects (owner, name) ON UPDATE CASCADE;
ALTER TABLE project_tags ADD CONSTRAINT project_tags_pname_powner_fkey FOREIGN KEY (project_owner, project_name) REFERENCES projects (owner, name) ON UPDATE CASCADE;

UPDATE projects SET name = 'Unknown project' where name = '';
