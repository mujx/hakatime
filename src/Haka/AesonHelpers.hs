module Haka.AesonHelpers
  ( noPrefixOptions,
    untagged,
    convertReservedWords,
  )
where

import qualified Data.Aeson as A
import Data.Aeson.Types (Options (..), defaultOptions)
import qualified Data.Char as C

noPrefix :: String -> String
noPrefix = firstToLower . dropWhile C.isLower
  where
    firstToLower [] = []
    firstToLower (x : xs) = C.toLower x : xs

noPrefixOptions :: Options
noPrefixOptions = defaultOptions {fieldLabelModifier = noPrefix}

untagged :: Options
untagged = defaultOptions {sumEncoding = A.UntaggedValue}

convertReservedWords :: Options
convertReservedWords = defaultOptions {fieldLabelModifier = convertFieldNames}
  where
    convertFieldNames :: String -> String
    convertFieldNames "ty" = "type"
    convertFieldNames "time_sent" = "time"
    convertFieldNames "file_lines" = "lines"
    convertFieldNames t = t
