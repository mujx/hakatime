{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Haka.Db.Statements
  ( insertHeartBeat,
    createAPIToken,
    updateTokenUsage,
    listApiTokens,
    insertProject,
    getTimeline,
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
import Data.FileEmbed
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
    StoredApiToken (..),
    TimelineRow (..),
    TokenData (..),
  )
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import Hasql.Statement
import Text.RawString.QQ (r)

updateTokenUsage :: Statement Text ()
updateTokenUsage = Statement query params D.noResult True
  where
    query :: Bs.ByteString
    query = [r|
      UPDATE auth_tokens
      SET last_usage = now()::timestamp
      WHERE token = $1
    |]

    params :: E.Params Text
    params = E.param (E.nonNullable E.text)

listApiTokens :: Statement Text [StoredApiToken]
listApiTokens = Statement query params result True
  where
    query :: Bs.ByteString
    query =
      [r| 
      select
        token, last_usage::timestamp
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

    result :: D.Result [StoredApiToken]
    result = D.rowList storedApiToken

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

getUserActivity :: Statement (Text, UTCTime, UTCTime, Int64) [StatRow]
getUserActivity = Statement query params result True
  where
    query :: Bs.ByteString
    query = $(embedFile "sql/get_user_activity.sql")
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

getTimeline :: Statement (Text, UTCTime, UTCTime, Int64) [TimelineRow]
getTimeline = Statement query params result True
  where
    query :: Bs.ByteString
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
