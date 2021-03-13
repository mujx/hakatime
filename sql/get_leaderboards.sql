SELECT
    coalesce(total_stats.project, 'Other') AS project,
    coalesce(total_stats.language, 'Other') AS "language",
    total_stats.sender,
    CAST(sum(extract(epoch FROM previous_diff) + 0) AS int8) AS total_seconds
FROM (
    SELECT
        time_sent::date + interval '0h' AS day,
        heartbeats.project,
        heartbeats.language,
        heartbeats.sender,
        (time_sent - (lag(time_sent) OVER (ORDER BY time_sent))) AS previous_diff
    FROM
        heartbeats
    WHERE
        time_sent >= $1
        AND time_sent <= $2
    ORDER BY
        time_sent) total_stats
WHERE
    extract(epoch FROM previous_diff) <= (15 * 60)
GROUP BY
    total_stats.project,
    total_stats.language,
    total_stats.sender
ORDER BY
    total_stats.language
