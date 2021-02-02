DELETE FROM heartbeats a USING heartbeats b
WHERE a.id < b.id
    AND a.time_sent = b.time_sent
    AND a.project = b.project
    AND a.branch = b.branch
    AND a.machine = b.machine
    AND a.sender = b.sender
    AND a.entity = b.entity
    AND a.user_agent = b.user_agent;

