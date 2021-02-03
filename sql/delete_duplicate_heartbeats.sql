DELETE FROM heartbeats a USING heartbeats b
WHERE a.id < b.id
    AND a.time_sent = b.time_sent
    AND a.sender = b.sender
    AND a.entity = b.entity;

