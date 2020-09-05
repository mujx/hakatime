SELECT
    sum(_b.total_seconds)
FROM (
    SELECT
        _a.day,
        CAST(sum(extract(minute FROM previous_diff) * 60 + extract(second FROM previous_diff)) AS int8) AS total_seconds
    FROM (
        SELECT
            time_sent::date + interval '0h' AS day,
            (time_sent - (lag(time_sent) OVER (ORDER BY time_sent))) AS previous_diff
        FROM
            heartbeats
        WHERE
            sender = $1
            AND project = $3
            AND time_sent >= (now() - interval '1' day * $2)
        ORDER BY
            time_sent) _a
    WHERE
        extract(epoch FROM previous_diff) <= (15 * 60)
    GROUP BY
        _a.day
    ORDER BY
        _a.day) _b
