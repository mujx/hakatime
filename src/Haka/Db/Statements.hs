{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Haka.Db.Statements where

import Contravariant.Extras.Contrazip (contrazip3, contrazip4, contrazip5)
import Data.Aeson as A
import Data.FileEmbed
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.Vector as V
import Haka.Types
  ( BadgeRow (..),
    EntityType (..),
    HeartbeatPayload (..),
    LeaderboardRow (..),
    ProjectStatRow (..),
    RegisteredUser (..),
    StatRow (..),
    StoredApiToken (..),
    TimelineRow (..),
    TokenData (..),
  )
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import Hasql.Statement
import PostgreSQL.Binary.Data (UUID)
import Text.RawString.QQ (r)

vectorEncoder :: E.Value b -> E.Params (V.Vector b)
vectorEncoder =
  E.param
    . E.nonNullable
    . E.array
    . E.dimension foldl'
    . E.element
    . E.nonNullable

updateTokenUsage :: Statement Text ()
updateTokenUsage = Statement query params D.noResult True
  where
    query :: ByteString
    query =
      [r|
      UPDATE auth_tokens
      SET last_usage = now()::timestamp
      WHERE token = $1
    |]

    params :: E.Params Text
    params = E.param (E.nonNullable E.text)

updateTokenMetadata :: Statement (Text, Text, Text) ()
updateTokenMetadata = Statement query params D.noResult True
  where
    query :: ByteString
    query = [r| UPDATE auth_tokens SET token_name = $3 WHERE token = $1 AND owner = $2; |]

    params :: E.Params (Text, Text, Text)
    params =
      contrazip3
        (E.param (E.nonNullable E.text))
        (E.param (E.nonNullable E.text))
        (E.param (E.nonNullable E.text))

listApiTokens :: Statement Text [StoredApiToken]
listApiTokens = Statement query params result True
  where
    query :: ByteString
    query =
      [r| 
      select
        token,
        last_usage::timestamp,
        token_name,
        token_description
      from
        auth_tokens
      where
        owner = $1 and
        token_expiry is null
      |]

    params :: E.Params Text
    params = E.param (E.nonNullable E.text)

    storedApiToken :: D.Row StoredApiToken
    storedApiToken =
      StoredApiToken
        <$> (D.column . D.nonNullable) D.text
        <*> (D.column . D.nullable) D.timestamptz
        <*> (D.column . D.nullable) D.text
        <*> (D.column . D.nullable) D.text

    result :: D.Result [StoredApiToken]
    result = D.rowList storedApiToken

createAPIToken :: Statement (Text, Text) ()
createAPIToken = Statement query params D.noResult True
  where
    params :: E.Params (Text, Text)
    params = (fst >$< E.param (E.nonNullable E.text)) <> (snd >$< E.param (E.nonNullable E.text))
    query :: ByteString
    query = [r| INSERT INTO auth_tokens(owner, token) values($1, $2); |]

createAccessTokens :: Int64 -> Statement TokenData ()
createAccessTokens refreshTokenExpiryHours = Statement query params D.noResult True
  where
    params :: E.Params TokenData
    params =
      (tknOwner >$< E.param (E.nonNullable E.text))
        <> (tknToken >$< E.param (E.nonNullable E.text))
        <> (tknRefreshToken >$< E.param (E.nonNullable E.text))
        <> (const refreshTokenExpiryHours >$< E.param (E.nonNullable E.int8))
    query :: ByteString
    query =
      [r|
      WITH x AS (
        INSERT INTO auth_tokens(
          owner,
          token,
          token_expiry
        ) values($1, $2, NOW() + interval '30 minutes')
      )
      INSERT INTO refresh_tokens(
        owner,
        refresh_token,
        token_expiry
      ) values($1, $3, NOW() + interval '1' hour * $4);
      |]

deleteExpiredTokens :: Statement TokenData ()
deleteExpiredTokens = Statement query params D.noResult True
  where
    params :: E.Params TokenData
    params =
      (tknOwner >$< E.param (E.nonNullable E.text))
        <> (tknToken >$< E.param (E.nonNullable E.text))
        <> (tknRefreshToken >$< E.param (E.nonNullable E.text))
    query :: ByteString
    query =
      [r|
      WITH x AS (
        DELETE FROM auth_tokens WHERE owner = $1 AND token_expiry < NOW()
      )

      DELETE FROM refresh_tokens WHERE owner = $1 AND token_expiry < NOW();
      |]

deleteRefreshToken :: Statement Text Int64
deleteRefreshToken = Statement query params D.rowsAffected True
  where
    params :: E.Params Text
    params = E.param (E.nonNullable E.text)
    query :: ByteString
    query = [r| DELETE FROM refresh_tokens WHERE refresh_token = $1; |]

deleteAuthToken :: Statement Text Int64
deleteAuthToken = Statement query params D.rowsAffected True
  where
    params :: E.Params Text
    params = E.param (E.nonNullable E.text)
    query :: ByteString
    query = [r| DELETE FROM auth_tokens WHERE token = $1; |]

doubleToUTCTime :: Real a => a -> UTCTime
doubleToUTCTime d = posixSecondsToUTCTime $ realToFrac d

insertToken :: Statement (Text, Text) ()
insertToken =
  Statement
    query
    ( (fst >$< E.param (E.nonNullable E.text))
        <> (snd >$< E.param (E.nonNullable E.text))
    )
    D.noResult
    True
  where
    query :: ByteString
    query =
      [r|
        INSERT INTO auth_tokens 
        (
          token,
          owner
        )
        VALUES ( $1, $2 );
      |]

getUserByName :: Statement Text (Maybe RegisteredUser)
getUserByName = Statement query (E.param (E.nonNullable E.text)) userDecoder True
  where
    query :: ByteString
    query = [r| SELECT * FROM users WHERE username = $1;|]
    userDecoder :: D.Result (Maybe RegisteredUser)
    userDecoder = D.rowMaybe user
      where
        user :: D.Row RegisteredUser
        user =
          RegisteredUser
            <$> (D.column . D.nonNullable) D.text
            <*> (D.column . D.nonNullable) D.bytea
            <*> (D.column . D.nonNullable) D.bytea

getTotalActivityTime :: Statement (Text, Int64, Text) (Maybe Int64)
getTotalActivityTime = Statement query params result True
  where
    params :: E.Params (Text, Int64, Text)
    params =
      contrazip3
        (E.param (E.nonNullable E.text))
        (E.param (E.nonNullable E.int8))
        (E.param (E.nonNullable E.text))
    query :: ByteString
    query = $(embedFile "sql/get_total_project_time.sql")
    result :: D.Result (Maybe Int64)
    result = D.rowMaybe $ (D.column . D.nonNullable) D.int8

insertUser :: Statement RegisteredUser ()
insertUser = Statement query params D.noResult True
  where
    params :: E.Params RegisteredUser
    params =
      ( username
          >$< E.param (E.nonNullable E.text)
      )
        <> (hashedPassword >$< E.param (E.nonNullable E.bytea))
        <> (saltUsed >$< E.param (E.nonNullable E.bytea))
    query :: ByteString
    query =
      [r|
        INSERT INTO users 
        (
          username,
          hashed_password,
          salt_used
        )
        VALUES ( $1, $2, $3 );
      |]

isUserAvailable :: Statement Text (Maybe RegisteredUser)
isUserAvailable = Statement query (E.param (E.nonNullable E.text)) userDecoder True
  where
    query :: ByteString
    query = [r| SELECT * FROM users WHERE username = $1 |]

    userDecoder :: D.Result (Maybe RegisteredUser)
    userDecoder = D.rowMaybe user
      where
        user :: D.Row RegisteredUser
        user =
          RegisteredUser
            <$> (D.column . D.nonNullable) D.text
            <*> (D.column . D.nonNullable) D.bytea
            <*> (D.column . D.nonNullable) D.bytea

getUserByToken :: Statement (Text, UTCTime) (Maybe Text)
getUserByToken =
  Statement
    query
    ( (fst >$< E.param (E.nonNullable E.text))
        <> (snd >$< E.param (E.nonNullable E.timestamptz))
    )
    (D.rowMaybe ((D.column . D.nonNullable) D.text))
    True
  where
    -- NOTE: On auth tokens the expiry date might not be set.
    -- The tokens created by the CLI do not expire.
    query :: ByteString
    query =
      [r| 
        SELECT owner FROM auth_tokens 
        WHERE  token = $1 
        AND    COALESCE(
                token_expiry, 
                (NOW() + interval '1 hours')::timestamp without time zone
               ) > $2 ; 
      |]

getUserByRefreshToken :: Statement (Text, UTCTime) (Maybe Text)
getUserByRefreshToken =
  Statement
    query
    ( (fst >$< E.param (E.nonNullable E.text))
        <> (snd >$< E.param (E.nonNullable E.timestamptz))
    )
    (D.rowMaybe ((D.column . D.nonNullable) D.text))
    True
  where
    query :: ByteString
    query =
      [r| 
      SELECT owner FROM refresh_tokens 
      WHERE 
        refresh_token = $1 AND token_expiry > $2
      ; 
    |]

insertProject :: Statement (Text, Text) ()
insertProject = Statement query params D.noResult True
  where
    params :: E.Params (Text, Text)
    params = (fst >$< E.param (E.nonNullable E.text)) <> (snd >$< E.param (E.nonNullable E.text))
    query :: ByteString
    query =
      [r|
        INSERT INTO projects (owner, name) VALUES ( $1, $2 ) ON CONFLICT DO NOTHING;
      |]

createBadgeLink :: Statement (Text, Text) UUID
createBadgeLink = Statement query params result True
  where
    params :: E.Params (Text, Text)
    params = (fst >$< E.param (E.nonNullable E.text)) <> (snd >$< E.param (E.nonNullable E.text))
    result :: D.Result UUID
    result = D.singleRow (D.column (D.nonNullable D.uuid))
    query :: ByteString
    query =
      [r|
        INSERT INTO badges(username, project) values($1, $2)
        ON CONFLICT (username, project) DO UPDATE SET username=EXCLUDED.username
        RETURNING link_id;
     |]

getBadgeLinkInfo :: Statement UUID BadgeRow
getBadgeLinkInfo = Statement query params result True
  where
    params :: E.Params UUID
    params = E.param (E.nonNullable E.uuid)
    result :: D.Result BadgeRow
    result =
      D.singleRow
        ( BadgeRow <$> (D.column . D.nonNullable) D.text
            <*> (D.column . D.nonNullable) D.text
        )
    query :: ByteString
    query = [r| SELECT username, project FROM badges WHERE link_id = $1; |]

intToText :: E.Value Int64
intToText = E.enum show

insertHeartBeat :: Statement HeartbeatPayload Int64
insertHeartBeat = Statement query params result True
  where
    result :: D.Result Int64
    result = D.singleRow (D.column (D.nonNullable D.int8))
    query :: ByteString
    query = $(embedFile "sql/insert_heartbeat.sql")
    params :: E.Params HeartbeatPayload
    params =
      (editor >$< E.param (E.nullable E.text))
        <> (plugin >$< E.param (E.nullable E.text))
        <> (platform >$< E.param (E.nullable E.text))
        <> (machine >$< E.param (E.nullable E.text))
        <> (sender >$< E.param (E.nullable E.text))
        <> (user_agent >$< E.param (E.nonNullable E.text))
        <> (branch >$< E.param (E.nullable E.text))
        <> (category >$< E.param (E.nullable E.text))
        <> (cursorpos >$< E.param (E.nullable intToText))
        <> ( dependencies
               >$< E.param
                 ( E.nullable
                     ( E.array (E.dimension foldl' (E.element (E.nonNullable E.text)))
                     )
                 )
           )
        <> (entity >$< E.param (E.nonNullable E.text))
        <> (is_write >$< E.param (E.nullable E.bool))
        <> (language >$< E.param (E.nullable E.text))
        <> (lineno >$< E.param (E.nullable E.int8))
        <> (file_lines >$< E.param (E.nullable E.int8))
        <> (project >$< E.param (E.nullable E.text))
        <> (ty >$< E.param (E.nonNullable entityValue))
        <> ((doubleToUTCTime . time_sent) >$< E.param (E.nonNullable E.timestamptz))
    entityValue :: E.Value EntityType
    entityValue = E.enum entityText
      where
        entityText FileType = "file"
        entityText AppType = "app"
        entityText DomainType = "domain"

getProjectStats :: Statement (Text, Text, UTCTime, UTCTime, Int64) [ProjectStatRow]
getProjectStats = Statement query params result True
  where
    query :: ByteString
    query = $(embedFile "sql/get_projects_stats.sql")
    params :: E.Params (Text, Text, UTCTime, UTCTime, Int64)
    params =
      contrazip5
        (E.param (E.nonNullable E.text))
        (E.param (E.nonNullable E.text))
        (E.param (E.nonNullable E.timestamptz))
        (E.param (E.nonNullable E.timestamptz))
        (E.param (E.nonNullable E.int8))
    projStateRow :: D.Row ProjectStatRow
    projStateRow =
      ProjectStatRow
        <$> (D.column . D.nonNullable) D.timestamptz
        <*> (D.column . D.nonNullable) D.text
        <*> (D.column . D.nonNullable) D.text
        <*> (D.column . D.nonNullable) D.text
        <*> (D.column . D.nonNullable) D.text
        <*> (D.column . D.nonNullable) D.int8
        <*> (D.column . D.nonNullable) D.numeric
        <*> (D.column . D.nonNullable) D.numeric
    result :: D.Result [ProjectStatRow]
    result = D.rowList projStateRow

getTagStats :: Statement (Text, Text, UTCTime, UTCTime, Int64) [ProjectStatRow]
getTagStats = Statement query params result True
  where
    query :: ByteString
    query = $(embedFile "sql/get_tag_stats.sql")
    params :: E.Params (Text, Text, UTCTime, UTCTime, Int64)
    params =
      contrazip5
        (E.param (E.nonNullable E.text))
        (E.param (E.nonNullable E.text))
        (E.param (E.nonNullable E.timestamptz))
        (E.param (E.nonNullable E.timestamptz))
        (E.param (E.nonNullable E.int8))
    projStateRow :: D.Row ProjectStatRow
    projStateRow =
      ProjectStatRow
        <$> (D.column . D.nonNullable) D.timestamptz
        <*> (D.column . D.nonNullable) D.text
        <*> (D.column . D.nonNullable) D.text
        <*> (D.column . D.nonNullable) D.text
        <*> (D.column . D.nonNullable) D.text
        <*> (D.column . D.nonNullable) D.int8
        <*> (D.column . D.nonNullable) D.numeric
        <*> (D.column . D.nonNullable) D.numeric
    result :: D.Result [ProjectStatRow]
    result = D.rowList projStateRow

statRowDecoder :: D.Row StatRow
statRowDecoder =
  StatRow
    <$> (D.column . D.nonNullable) D.timestamptz
    <*> (D.column . D.nonNullable) D.text
    <*> (D.column . D.nonNullable) D.text
    <*> (D.column . D.nonNullable) D.text
    <*> (D.column . D.nonNullable) D.text
    <*> (D.column . D.nonNullable) D.text
    <*> (D.column . D.nonNullable) D.text
    <*> (D.column . D.nonNullable) D.text
    <*> (D.column . D.nonNullable) D.int8
    <*> (D.column . D.nonNullable) D.numeric
    <*> (D.column . D.nonNullable) D.numeric

getUserActivityByTag :: Statement (Text, UTCTime, UTCTime, Text, Int64) [StatRow]
getUserActivityByTag = Statement query params result True
  where
    query :: ByteString
    query = $(embedFile "sql/get_user_activity_by_tags.sql")
    params :: E.Params (Text, UTCTime, UTCTime, Text, Int64)
    params =
      contrazip5
        (E.param (E.nonNullable E.text))
        (E.param (E.nonNullable E.timestamptz))
        (E.param (E.nonNullable E.timestamptz))
        (E.param (E.nonNullable E.text))
        (E.param (E.nonNullable E.int8))
    result :: D.Result [StatRow]
    result = D.rowList statRowDecoder

getUserActivity :: Statement (Text, UTCTime, UTCTime, Int64) [StatRow]
getUserActivity = Statement query params result True
  where
    query :: ByteString
    query = $(embedFile "sql/get_user_activity.sql")
    params :: E.Params (Text, UTCTime, UTCTime, Int64)
    params =
      contrazip4
        (E.param (E.nonNullable E.text))
        (E.param (E.nonNullable E.timestamptz))
        (E.param (E.nonNullable E.timestamptz))
        (E.param (E.nonNullable E.int8))
    result :: D.Result [StatRow]
    result = D.rowList statRowDecoder

getTimeline :: Statement (Text, UTCTime, UTCTime, Int64) [TimelineRow]
getTimeline = Statement query params result True
  where
    query :: ByteString
    query = $(embedFile "sql/get_timeline.sql")
    params :: E.Params (Text, UTCTime, UTCTime, Int64)
    params =
      contrazip4
        (E.param (E.nonNullable E.text))
        (E.param (E.nonNullable E.timestamptz))
        (E.param (E.nonNullable E.timestamptz))
        (E.param (E.nonNullable E.int8))
    tRow :: D.Row TimelineRow
    tRow =
      TimelineRow
        <$> (D.column . D.nonNullable) D.text
        <*> (D.column . D.nonNullable) D.text
        <*> (D.column . D.nonNullable) D.timestamptz
        <*> (D.column . D.nonNullable) D.timestamptz
    result :: D.Result [TimelineRow]
    result = D.rowList tRow

deleteFailedJobs :: Statement A.Value Int64
deleteFailedJobs = Statement query params D.rowsAffected True
  where
    params :: E.Params A.Value
    params = E.param (E.nonNullable E.json)
    query :: ByteString
    query = [r| DELETE FROM payloads WHERE value::text = $1::text; |]

getJobStatus :: Statement A.Value (Maybe Text)
getJobStatus = Statement query params (D.rowMaybe ((D.column . D.nonNullable) D.text)) True
  where
    params :: E.Params A.Value
    params = E.param (E.nonNullable E.json)
    query :: ByteString
    query = [r| SELECT state FROM payloads WHERE value::text = $1::text; |]

insertTags :: Statement (V.Vector Text) (V.Vector UUID)
insertTags = Statement query params result True
  where
    query :: ByteString
    query =
      [r|
      INSERT INTO tags (name) SELECT * FROM unnest ($1)
      ON CONFLICT (name) DO UPDATE SET name=EXCLUDED.name RETURNING id;
    |]
    result :: D.Result (V.Vector UUID)
    result = D.rowVector (D.column (D.nonNullable D.uuid))
    params :: E.Params (V.Vector Text)
    params = vectorEncoder E.text

deleteExistingTags :: Statement (Text, Text) ()
deleteExistingTags = Statement query params D.noResult True
  where
    query :: ByteString
    query = [r| DELETE FROM project_tags WHERE project_name = $1 AND project_owner = $2; |]
    params :: E.Params (Text, Text)
    params = (fst >$< E.param (E.nonNullable E.text)) <> (snd >$< E.param (E.nonNullable E.text))

addTagsToProject :: Statement (V.Vector (Text, Text, UUID)) Int64
addTagsToProject = Statement query params decoder True
  where
    query :: ByteString
    query =
      [r|
      INSERT INTO project_tags (project_name, project_owner, tag_id) SELECT * FROM unnest ($1, $2, $3)
      ON CONFLICT DO NOTHING;;
    |]
    params :: E.Params (V.Vector (Text, Text, UUID))
    params = contramap V.unzip3 $ contrazip3 (vectorEncoder E.text) (vectorEncoder E.text) (vectorEncoder E.uuid)
    decoder = D.rowsAffected

checkProjectOwner :: Statement (Text, Text) (Maybe Text)
checkProjectOwner = Statement query params (D.rowMaybe ((D.column . D.nonNullable) D.text)) True
  where
    params :: E.Params (Text, Text)
    params = (fst >$< E.param (E.nonNullable E.text)) <> (snd >$< E.param (E.nonNullable E.text))
    query :: ByteString
    query = [r| SELECT name FROM projects WHERE name = $1 AND owner = $2; |]

checkTagOwner :: Statement (Text, Text) (Maybe Text)
checkTagOwner = Statement query params (D.rowMaybe ((D.column . D.nonNullable) D.text)) True
  where
    params :: E.Params (Text, Text)
    params = (fst >$< E.param (E.nonNullable E.text)) <> (snd >$< E.param (E.nonNullable E.text))
    query :: ByteString
    query =
      [r|
      SELECT name
      FROM tags
      INNER JOIN project_tags ON tags.id = project_tags.tag_id
      WHERE name = $1 AND project_owner = $2
      LIMIT 1;
    |]

{- HLINT ignore "Reduce duplication" -}
getTags :: Statement (Text, Text) (V.Vector Text)
getTags = Statement query params result True
  where
    result :: D.Result (V.Vector Text)
    result = D.rowVector (D.column (D.nonNullable D.text))
    params :: E.Params (Text, Text)
    params = (fst >$< E.param (E.nonNullable E.text)) <> (snd >$< E.param (E.nonNullable E.text))
    query :: ByteString
    query =
      [r|
      SELECT
          name
      FROM
          project_tags
          INNER JOIN tags ON project_tags.tag_id = tags.id
      WHERE
          project_name = $1
          AND project_owner = $2;
      |]

getAllTags :: Statement Text (V.Vector Text)
getAllTags = Statement query params result True
  where
    result :: D.Result (V.Vector Text)
    result = D.rowVector (D.column (D.nonNullable D.text))
    params :: E.Params Text
    params = E.param (E.nonNullable E.text)
    query :: ByteString
    query =
      [r|
      SELECT
          DISTINCT name
      FROM
          project_tags
          INNER JOIN tags ON project_tags.tag_id = tags.id
      WHERE
          project_owner = $1;
      |]

getAllProjects :: Statement (Text, UTCTime, UTCTime) (V.Vector Text)
getAllProjects = Statement query params result True
  where
    fst' (x, _, _) = x
    snd' (_, x, _) = x
    thd (_, _, x) = x
    result :: D.Result (V.Vector Text)
    result = D.rowVector (D.column (D.nonNullable D.text))
    params :: E.Params (Text, UTCTime, UTCTime)
    params =
      (fst' >$< E.param (E.nonNullable E.text))
        <> (snd' >$< E.param (E.nonNullable E.timestamptz))
        <> (thd >$< E.param (E.nonNullable E.timestamptz))
    query :: ByteString
    query =
      [r|
        SELECT
            name
        FROM
            projects
            INNER JOIN heartbeats ON heartbeats.project = projects.name
                AND heartbeats.sender = projects.owner
        WHERE
            heartbeats.sender = $1
            AND heartbeats.time_sent >= $2
            AND heartbeats.time_sent <= $3
        GROUP BY projects.name
        ORDER BY COUNT(*) DESC;
      |]

getLeaderboards :: Statement (UTCTime, UTCTime) [LeaderboardRow]
getLeaderboards = Statement query params result True
  where
    result :: D.Result [LeaderboardRow]
    result = D.rowList leaderRow
    query :: ByteString
    query = $(embedFile "sql/get_leaderboards.sql")
    leaderRow :: D.Row LeaderboardRow
    leaderRow =
      LeaderboardRow
        <$> (D.column . D.nonNullable) D.text
        <*> (D.column . D.nonNullable) D.text
        <*> (D.column . D.nonNullable) D.text
        <*> (D.column . D.nonNullable) D.int8
    params :: E.Params (UTCTime, UTCTime)
    params =
      (fst >$< E.param (E.nonNullable E.timestamptz))
        <> (snd >$< E.param (E.nonNullable E.timestamptz))

getTotalTimeBetween :: Statement (V.Vector (Text, Text, UTCTime, UTCTime)) [Int64]
getTotalTimeBetween = Statement query params result True
  where
    result :: D.Result [Int64]
    result = D.rowList $ (D.column . D.nonNullable) D.int8
    query :: ByteString
    query = $(embedFile "sql/get_time_between.sql")
    params :: E.Params (V.Vector (Text, Text, UTCTime, UTCTime))
    params =
      contramap V.unzip4 $
        contrazip4
          (vectorEncoder E.text)
          (vectorEncoder E.text)
          (vectorEncoder E.timestamptz)
          (vectorEncoder E.timestamptz)

getTotalTimeToday :: Statement Text Int64
getTotalTimeToday = Statement query params result True
  where
    result :: D.Result Int64
    result = D.singleRow (D.column (D.nonNullable D.int8))
    params :: E.Params Text
    params = E.param (E.nonNullable E.text)
    query :: ByteString
    query = $(embedFile "sql/get_time_today.sql")
