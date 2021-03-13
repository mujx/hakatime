{-# LANGUAGE OverloadedStrings #-}

module Haka.Utils
  ( toStrError,
    getRefreshToken,
    genDateRange,
    passwordInput,
    defaultLimit,
    randomToken,
    toBase64,
    EditorInfo (..),
    userAgentInfo,
    rollingGroupBy,
    countDuration,
    fmtDate,
    removeAWeek,
    removeAMonth,
    removeAYear,
    addAWeek,
    addAMonth,
  )
where

import Control.Exception (bracket_)
import Data.ByteString.Base64 (encode)
import Data.Text (splitOn)
import Data.Time (addDays, diffDays)
import Data.Time.Clock (UTCTime (..))
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import Hasql.Pool (UsageError (..))
import qualified Hasql.Session as S
import qualified Relude.Unsafe as Unsafe
import System.IO (hFlush, hGetEcho, hSetEcho, putChar)
import Web.Cookie

defaultLimit :: Int64
defaultLimit = 15

fmtDate :: UTCTime -> String
fmtDate = formatTime defaultTimeLocale "%FT%T%QZ"

-- TODO: Replace List with another data structure.
--

-- | List.groupBy implementation where the predicate is computed using the last element of
-- the current group instead of the first.
rollingGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
rollingGroupBy predicate xs = go predicate xs [] []
  where
    go _ [] curr total = reverse $ curr : total
    go pFn (m : ms) [] total = go pFn ms [m] total
    go pFn (m : ms) curr@[y] total
      | pFn m y = go pFn ms [y, m] total
      | otherwise = go pFn ms [m] (curr : total)
    go pFn (m : ms) curr total
      | pFn m (Unsafe.last curr) = go pFn ms (curr ++ [m]) total
      | otherwise = go pFn ms [m] (curr : total)

-- | Given a set of timestamps & a cut-off value determined the total number of minutes counted.
--
-- >>> countDuration [1, 2, 3, 10, 21, 22, 33, 100, 104, 109] 5
-- >>> 12
countDuration :: [Int] -> Int -> Int
countDuration points interval =
  sum $ map countDiff $ groupByDiff points
  where
    groupByDiff = rollingGroupBy (\x y -> abs (y - x) <= interval)
    countDiff [] = 0
    countDiff [_] = 0
    countDiff (x : xs) = abs (x - Unsafe.last xs)

data EditorInfo = EditorInfo
  { editor :: Maybe Text,
    plugin :: Maybe Text,
    platform :: Maybe Text
  }
  deriving (Show)

-- | Parse the user agent string & extract the editor & plugin name/version pair.
userAgentInfo :: Text -> EditorInfo
userAgentInfo userAgent =
  EditorInfo
    { editor = editor',
      plugin = plugin',
      platform = platform'
    }
  where
    tokens = splitOn " " userAgent
    platform' = nth 1 tokens
    editor' = nth 3 tokens
    plugin' = nth 4 tokens

-- | Safer version of !!
nth :: Int -> [a] -> Maybe a
nth _ [] = Nothing
nth 0 (x : _) = Just x
nth n (_ : xs) = nth (n - 1) xs

randomToken :: IO Text
randomToken = UUID.toText <$> nextRandom

toBase64 :: Text -> Text
toBase64 = decodeUtf8 . encode . encodeUtf8

-- / Show a password prompt to the user.
passwordInput :: String -> IO Text
passwordInput prompt = do
  putStr prompt
  hFlush stdout
  passwd <- withEcho False getLine
  putChar '\n'
  return $ toText passwd
  where
    withEcho :: Bool -> IO a -> IO a
    withEcho echo action = do
      old <- hGetEcho stdin
      bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

-- | Convert a `UsageError` to a user friendly `Text` representation.
toStrError :: UsageError -> Text
toStrError (ConnectionError (Just e)) = decodeUtf8 e
toStrError (ConnectionError Nothing) = "Failed to connect to the database"
toStrError
  ( SessionError
      ( S.QueryError
          _
          _
          (S.ResultError (S.ServerError _ msg (Just details) _))
        )
    ) = decodeUtf8 $ msg <> ": " <> details
toStrError
  ( SessionError
      ( S.QueryError
          _
          _
          (S.ResultError (S.ServerError _ msg Nothing _))
        )
    ) = decodeUtf8 msg
toStrError
  ( SessionError
      ( S.QueryError
          _
          _
          (S.ResultError (S.UnexpectedResult err))
        )
    ) = err
toStrError err = toText (show err :: String)

getRefreshToken :: ByteString -> Maybe Text
getRefreshToken cookies =
  let value = listToMaybe $ map snd $ filter (\(k, _) -> k == "refresh_token") (parseCookies cookies)
   in case value of
        Just v -> Just $ decodeUtf8 v
        Nothing -> Nothing

genDateRange :: UTCTime -> UTCTime -> [UTCTime]
genDateRange t0 t1 =
  [ UTCTime
      { utctDay = addDays d (utctDay t0),
        utctDayTime = 0
      }
    | d <- [0 .. diffDays (utctDay t1) (utctDay t0) + 1]
  ]

removeAWeek, removeAMonth, removeAYear, addAWeek, addAMonth :: UTCTime -> UTCTime
removeAWeek t = UTCTime {utctDay = addDays (-7) (utctDay t), utctDayTime = 0}
removeAMonth t = UTCTime {utctDay = addDays (-30) (utctDay t), utctDayTime = 0}
removeAYear t = UTCTime {utctDay = addDays (-365) (utctDay t), utctDayTime = 0}
addAWeek t = UTCTime {utctDay = addDays 7 (utctDay t), utctDayTime = 0}
addAMonth t = UTCTime {utctDay = addDays 30 (utctDay t), utctDayTime = 0}
