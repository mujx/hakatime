WITH input_table AS (
    SELECT
        *
    FROM
        unnest($1, $2, $3, $4) AS input_table (username,
            project_name,
            min_date,
            max_date))
SELECT
    CAST(SUM(total_seconds) AS bigint)
FROM (
    SELECT
        time_sent,
        min_date,
        max_date,
        CAST(extract(epoch FROM previous_diff) AS int8) AS total_seconds
    FROM (
        SELECT
            time_sent,
            min_date,
            max_date,
            (time_sent - (lag(time_sent) OVER (ORDER BY time_sent))) AS previous_diff
        FROM
            heartbeats,
            input_table
        WHERE
            sender = input_table.username
            AND project = input_table.project_name
            AND time_sent > input_table.min_date
            AND time_sent < input_table.max_date
        ORDER BY
            time_sent) inner_table
    WHERE
        extract(epoch FROM previous_diff) <= (15 * 60)
    GROUP BY
        time_sent,
        previous_diff,
        min_date,
        max_date
    ORDER BY
        time_sent) outter_table
GROUP BY
    min_date,
    max_date
