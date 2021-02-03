ALTER TABLE heartbeats
    ADD CONSTRAINT unique_heartbeats UNIQUE (entity, sender, time_sent);

