SELECT
    coalesce(CAST(SUM(total_seconds) AS bigint), 0) as total_time
FROM (
    SELECT
        time_sent,
        CAST(extract(epoch FROM previous_diff) AS int8) AS total_seconds
    FROM (
        SELECT
            time_sent,
            (time_sent - (lag(time_sent) OVER (ORDER BY time_sent))) AS previous_diff
        FROM
            heartbeats
        WHERE
            sender = $1 AND time_sent >= (current_date + interval '0' day) AND time_sent < (current_date + interval '1' day)
        ORDER BY
            time_sent) inner_table
    WHERE
        extract(epoch FROM previous_diff) <= (15 * 60)
    GROUP BY
        time_sent,
        previous_diff
    ORDER BY
        time_sent) as result
