SELECT DISTINCT
    LANGUAGE,
    project,
    min(time_sent) OVER (PARTITION BY group_id) AS start_date,
    max(time_sent) OVER (PARTITION BY group_id) AS end_date
FROM (
    SELECT
        *,
        sum(stats_2.new_grp) OVER (ORDER BY stats_2.time_sent) AS group_id
    FROM (
        SELECT
            LANGUAGE,
            project,
            time_sent,
            previous_diff,
            (
                CASE WHEN lag(
                    LANGUAGE) OVER (ORDER BY time_sent) IS NULL
                    AND lag(project) OVER (ORDER BY time_sent) IS NULL THEN
                    0
                WHEN
                LANGUAGE =
                lag(
                LANGUAGE) OVER (ORDER BY time_sent)
                    AND project = lag(project) OVER (ORDER BY time_sent)
                    AND previous_diff <= ($4 * 60) THEN
                    0
                ELSE
                    1
                END) new_grp
        FROM (
        SELECT
            coalesce(
            LANGUAGE, 'Other') AS
            LANGUAGE,
            coalesce(project, 'Other') AS project,
            time_sent,
            extract(epoch FROM coalesce((time_sent - (lag(time_sent) OVER (ORDER BY time_sent))), '0 minutes')) AS previous_diff
        FROM
            heartbeats
        WHERE
            sender = $1
            AND time_sent > $2
            AND time_sent < $3
        GROUP BY
            project,
            LANGUAGE,
            time_sent
        ORDER BY
            time_sent) stats) stats_2) stats_3
GROUP BY
    group_id,
    LANGUAGE,
    project,
    time_sent
ORDER BY
    start_date
