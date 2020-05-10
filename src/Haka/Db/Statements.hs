{-# LANGUAGE QuasiQuotes #-}

module Haka.Db.Statements
  ( insertHeartBeat,
    createAPIToken,
    insertProject,
    getUserByToken,
    getUserByRefreshToken,
    deleteRefreshToken,
    deleteAuthToken,
    getUserActivity,
    getProjectStats,
    insertUser,
    getUserByName,
    insertToken,
    createAccessTokens,
    deleteExpiredTokens,
  )
where

import Contravariant.Extras.Contrazip (contrazip4, contrazip5)
import qualified Data.ByteString as Bs
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Haka.Types
  ( EntityType (..),
    HeartbeatPayload (..),
    ProjectStatRow (..),
    RegisteredUser (..),
    StatRow (..),
    TokenData (..),
  )
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import Hasql.Statement
import Text.RawString.QQ (r)

createAPIToken :: Statement (Text, Text) ()
createAPIToken = Statement query params D.noResult True
  where
    params :: E.Params (Text, Text)
    params = (fst >$< E.param (E.nonNullable E.text)) <> (snd >$< E.param (E.nonNullable E.text))
    query :: Bs.ByteString
    query = [r| INSERT INTO auth_tokens(owner, token) values($1, $2); |]

createAccessTokens :: Statement TokenData ()
createAccessTokens = Statement query params D.noResult True
  where
    params :: E.Params TokenData
    params =
      (tknOwner >$< E.param (E.nonNullable E.text))
        <> (tknToken >$< E.param (E.nonNullable E.text))
        <> (tknRefreshToken >$< E.param (E.nonNullable E.text))
    query :: Bs.ByteString
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
      ) values($1, $3, NOW() + interval '60 minutes');
      |]

deleteExpiredTokens :: Statement TokenData ()
deleteExpiredTokens = Statement query params D.noResult True
  where
    params :: E.Params TokenData
    params =
      (tknOwner >$< E.param (E.nonNullable E.text))
        <> (tknToken >$< E.param (E.nonNullable E.text))
        <> (tknRefreshToken >$< E.param (E.nonNullable E.text))
    query :: Bs.ByteString
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
    query :: Bs.ByteString
    query = [r| DELETE FROM refresh_tokens WHERE refresh_token = $1; |]

deleteAuthToken :: Statement Text Int64
deleteAuthToken = Statement query params D.rowsAffected True
  where
    params :: E.Params Text
    params = E.param (E.nonNullable E.text)
    query :: Bs.ByteString
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
    query :: Bs.ByteString
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
    query :: Bs.ByteString
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
    query :: Bs.ByteString
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
    query :: Bs.ByteString
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
    query :: Bs.ByteString
    query =
      [r| 
      SELECT owner FROM refresh_tokens 
      WHERE 
        refresh_token = $1 AND token_expiry > $2
      ; 
    |]

insertProject :: Statement Text ()
insertProject = Statement query (E.param (E.nonNullable E.text)) D.noResult True
  where
    query :: Bs.ByteString
    query =
      [r|
        INSERT INTO projects (name) VALUES ( $1 ) ON CONFLICT DO NOTHING;
      |]

insertHeartBeat :: Statement HeartbeatPayload Int64
insertHeartBeat = Statement query params result True
  where
    result :: D.Result Int64
    result = D.singleRow (D.column (D.nonNullable D.int8))
    query :: Bs.ByteString
    query =
      [r|
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
                  $11, $12, $13, $14, $15,
                  $16, $17, $18 )
        RETURNING "id";
      |]
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
        <> (cursorpos >$< E.param (E.nullable E.int8))
        <> ( dependencies
               >$< E.param
                 ( E.nullable
                     ( E.array (E.dimension foldl (E.element (E.nonNullable E.text)))
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
    query :: Bs.ByteString
    query =
      [r|
        with stats as (
          select
            total_stats.day,
            total_stats.dayofweek,
            total_stats.hourofday,
            coalesce(total_stats.language, 'Other') as language,
            total_stats.entity,
            cast(
              sum(
                extract(minute from previous_diff) * 60 +
                extract(second from previous_diff)
              ) as int8
            ) as total_seconds
          from (
            select
              (cast(extract(dow from (time_sent::date + interval '0h')) as int8))::text as dayofweek,
              (cast(extract(hour from time_sent) as int8))::text as hourofday,
              time_sent::date + interval '0h' as day,
              heartbeats.language,
              heartbeats.entity,
              (time_sent - (lag(time_sent) over (order by time_sent))) as previous_diff
          from heartbeats
            where
              sender = $1 and
              project = $2 and
              time_sent >= $3 and time_sent <= $4
            order by time_sent
          ) total_stats

          where extract(minute from previous_diff) <= $5
          group by total_stats.day,
                   total_stats.dayofweek,
                   total_stats.hourofday,
                   total_stats.language,
                   total_stats.entity
          order by total_stats.day
          )

          select
            *,
            cast(
              1.0 * total_seconds / nullif(sum(total_seconds) over (),0) as numeric
            ) as pct,
            cast (
              1.0 * total_seconds / nullif(sum(total_seconds) over (partition by day), 0) as numeric
            ) as daily_pct
        from stats;
      |]
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

getUserActivity :: Statement (Text, UTCTime, UTCTime, Int64) [StatRow]
getUserActivity = Statement query params result True
  where
    query :: Bs.ByteString
    query =
      [r|
      with stats as (
        select
          total_stats.day,
          coalesce(total_stats.project, 'Other') as project,
          coalesce(total_stats.language, 'Other') as language,
          coalesce(total_stats.editor, 'Other') as editor,
          coalesce(total_stats.branch, 'Other') as branch,
          total_stats.platform,
          total_stats.machine,
          total_stats.entity,
          cast(
            sum(
              extract(minute from previous_diff) * 60 +
              extract(second from previous_diff)
            ) as int8
          ) as total_seconds
        from (
          select
            time_sent::date + interval '0h' as day,
            heartbeats.project,
            heartbeats.language,
            heartbeats.editor,
            heartbeats.branch,
            heartbeats.entity,
            heartbeats.machine,
            heartbeats.platform,
            (time_sent - (lag(time_sent) over (order by time_sent))) as previous_diff
          from heartbeats
            where
              sender = $1 and
              time_sent >= $2 and time_sent <= $3
            order by time_sent
        ) total_stats
        where extract(minute from previous_diff) <= $4
        group by total_stats.day,
                total_stats.project,
                total_stats.language,
                total_stats.editor,
                total_stats.branch,
                total_stats.entity,
                total_stats.machine,
                total_stats.platform
        order by total_stats.day
        )

        select
          *,
          cast(
            1.0 * total_seconds / nullif(sum(total_seconds) over (), 0) as numeric
          ) as pct,
          cast(
            1.0 * total_seconds / nullif(sum(total_seconds) over (partition by day), 0) as numeric
          ) as daily_pct
        from stats
      |]
    params :: E.Params (Text, UTCTime, UTCTime, Int64)
    params =
      contrazip4
        (E.param (E.nonNullable E.text))
        (E.param (E.nonNullable E.timestamptz))
        (E.param (E.nonNullable E.timestamptz))
        (E.param (E.nonNullable E.int8))
    statRow :: D.Row StatRow
    statRow =
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
    result :: D.Result [StatRow]
    result = D.rowList statRow
