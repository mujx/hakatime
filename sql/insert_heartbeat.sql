INSERT INTO heartbeats
(
    editor,
    plugin,
    platform,
    machine,
    sender,
    user_agent,
    branch,
    category,
    cursorpos,
    dependencies,
    entity,
    is_write,
    language,
    lineno,
    file_lines,
    project,
    ty,
    time_sent
)

VALUES ( $1, $2, $3, $4, $5,
         $6, $7, $8, $9, $10,
         $11, $12, $13, CAST($14 AS INT), $15,
         $16, $17, $18 )

RETURNING "id";
