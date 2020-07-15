WITH stats AS (
    SELECT
        total_stats.day,
        coalesce(total_stats.project, 'Other') AS project,
        coalesce(total_stats.language, 'Other') AS
        LANGUAGE,
        coalesce(total_stats.editor, 'Other') AS editor,
        coalesce(total_stats.branch, 'Other') AS branch,
        total_stats.platform,
        total_stats.machine,
        total_stats.entity,
        CAST(sum(extract(epoch FROM previous_diff) + 0) AS int8) AS total_seconds
    FROM (
        SELECT
            time_sent::date + interval '0h' AS day,
            heartbeats.project,
            heartbeats.language,
            heartbeats.editor,
            heartbeats.branch,
            heartbeats.entity,
            heartbeats.machine,
            heartbeats.platform,
            (time_sent - (lag(time_sent) OVER (ORDER BY time_sent))) AS previous_diff
        FROM
            heartbeats
        WHERE
            sender = $1
            AND time_sent >= $2
            AND time_sent <= $3
        ORDER BY
            time_sent) total_stats
    WHERE
        extract(epoch FROM previous_diff) <= ($4 * 60)
    GROUP BY
        total_stats.day,
        total_stats.project,
        total_stats.language,
        total_stats.editor,
        total_stats.branch,
        total_stats.entity,
        total_stats.machine,
        total_stats.platform
    ORDER BY
        total_stats.day
)
SELECT
    *,
    coalesce(CAST(1.0 * total_seconds / nullif (sum(total_seconds) OVER (), 0) AS numeric), 0) AS pct,
    coalesce(CAST(1.0 * total_seconds / nullif (sum(total_seconds) OVER (PARTITION BY day), 0) AS numeric), 0) AS daily_pct
FROM
    stats
