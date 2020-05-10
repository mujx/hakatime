{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Haka.Users
  ( createUser,
    mkUser,
    createToken,
    validatePassword,
  )
where

import qualified Crypto.Error as CErr
import qualified Crypto.KDF.Argon2 as Argon2
import qualified Crypto.Random.Entropy as Entropy
import qualified Data.ByteString as Bs
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Haka.Db.Sessions as Sessions
import Haka.Types (RegisteredUser (..))
import qualified Haka.Utils as Utils
import qualified Hasql.Pool as HasqlPool

hashOutputLen :: Int
hashOutputLen = 64

hashSaltLen :: Int
hashSaltLen = 64

argonHash :: Bs.ByteString -> T.Text -> CErr.CryptoFailable Bs.ByteString
argonHash salt password =
  Argon2.hash Argon2.defaultOptions (encodeUtf8 password) salt hashOutputLen

mkUser :: T.Text -> T.Text -> IO (Either CErr.CryptoError RegisteredUser)
mkUser name pass = do
  salt <- Entropy.getEntropy hashSaltLen
  case argonHash salt pass of
    CErr.CryptoFailed e -> pure $ Left e
    CErr.CryptoPassed v ->
      pure $ Right $
        RegisteredUser
          { username = name,
            hashedPassword = v,
            saltUsed = salt
          }

validatePassword :: RegisteredUser -> T.Text -> T.Text -> Either CErr.CryptoError Bool
validatePassword savedUser name password =
  if name /= username savedUser
    then Right False
    else case argonHash (saltUsed savedUser) password of
      CErr.CryptoFailed e -> Left e
      CErr.CryptoPassed v -> Right (hashedPassword savedUser == v)

-- / Insert the user's credentials.
createUser :: HasqlPool.Pool -> RegisteredUser -> IO (Either HasqlPool.UsageError ())
createUser pool user = HasqlPool.use pool (Sessions.insertUser user)

-- / Validate the user credentials and generate a token for it if successful.
createToken :: HasqlPool.Pool -> T.Text -> T.Text -> IO (Either T.Text T.Text)
createToken pool name pass = do
  validationResult <- HasqlPool.use pool (Sessions.validateUser validatePassword name pass)
  either (pure . Left . Utils.toStrError) genToken validationResult
  where
    genToken :: Bool -> IO (Either T.Text T.Text)
    genToken isUserValid =
      if not isUserValid
        then pure $ Left "Wrong username or password"
        else do
          -- The user has the non-encoded version of the UUID. The wakatime client
          -- will encode it to Base64 before sending it.
          token <- Utils.randomToken
          -- We save the Base64 representation of the token in the
          -- database for future comparisons.
          --
          -- TODO: Encrypt the token
          --
          tokenResult <-
            HasqlPool.use
              pool
              ( Sessions.insertToken
                  (Utils.toBase64 token)
                  name
              )
          either
            (pure . Left . Utils.toStrError)
            (\_ -> pure $ Right token)
            tokenResult
