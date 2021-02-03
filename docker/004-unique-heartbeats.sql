ALTER TABLE heartbeats
    ADD CONSTRAINT unique_heartbeats
    UNIQUE (
      branch,
      entity,
      machine,
      sender,
      time_sent,
      user_agent
    );

