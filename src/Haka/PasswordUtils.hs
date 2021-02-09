module Haka.PasswordUtils
  ( mkUser,
    validatePassword,
    createUser,
    createToken,
  )
where

import qualified Crypto.Error as CErr
import qualified Crypto.KDF.Argon2 as Argon2
import qualified Crypto.Random.Entropy as Entropy
import qualified Haka.Db.Sessions as Sessions
import Haka.Types (RegisteredUser (..))
import Haka.Utils (randomToken, toBase64, toStrError)
import qualified Hasql.Pool as HasqlPool

hashOutputLen :: Int
hashOutputLen = 64

hashSaltLen :: Int
hashSaltLen = 64

argonHash :: ByteString -> Text -> CErr.CryptoFailable ByteString
argonHash salt password =
  Argon2.hash Argon2.defaultOptions (encodeUtf8 password :: ByteString) salt hashOutputLen

mkUser :: Text -> Text -> IO (Either CErr.CryptoError RegisteredUser)
mkUser name passwd = do
  salt <- Entropy.getEntropy hashSaltLen
  case argonHash salt passwd of
    CErr.CryptoFailed e -> pure $ Left e
    CErr.CryptoPassed v ->
      pure $
        Right $
          RegisteredUser
            { username = name,
              hashedPassword = v,
              saltUsed = salt
            }

validatePassword :: RegisteredUser -> Text -> Text -> Either CErr.CryptoError Bool
validatePassword savedUser name password =
  if name /= username savedUser
    then Right False
    else case argonHash (saltUsed savedUser) password of
      CErr.CryptoFailed e -> Left e
      CErr.CryptoPassed v -> Right (hashedPassword savedUser == v)

-- | Insert the user's credentials.
createUser ::
  HasqlPool.Pool ->
  RegisteredUser ->
  IO (Either HasqlPool.UsageError Bool)
createUser hpool user = HasqlPool.use hpool (Sessions.insertUser user)

-- | Validate the user credentials and generate a token for it if successful.
createToken :: HasqlPool.Pool -> Text -> Text -> IO (Either Text Text)
createToken hpool name passwd = do
  validationResult <- HasqlPool.use hpool (Sessions.validateUser validatePassword name passwd)
  either (pure . Left . toStrError) genToken validationResult
  where
    genToken :: Bool -> IO (Either Text Text)
    genToken isUserValid =
      if not isUserValid
        then pure $ Left "Wrong username or password"
        else do
          -- The user has the non-encoded version of the UUID. The wakatime client
          -- will encode it to Base64 before sending it.
          token <- randomToken
          -- We save the Base64 representation of the token in the
          -- database for future comparisons.
          --
          -- TODO: Encrypt the token
          --
          tokenResult <- HasqlPool.use hpool (Sessions.insertToken (toBase64 token) name)
          whenLeft (Right token) tokenResult (pure . Left . toStrError)
