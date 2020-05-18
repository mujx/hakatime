module Haka.Cli
  ( handleCommand,
    getDbSettings,
    opts,
    CliOpts (..),
  )
where

import Data.Bits.Extras (w16)
import Data.ByteString.Char8 (pack)
import Data.Text (Text, unpack)
import Data.Version (showVersion)
import qualified Haka.Users as Users
import qualified Haka.Utils as Utils
import qualified Hasql.Connection as HasqlConn
import qualified Hasql.Pool as HasqlPool
import qualified Options.Applicative as Opt
import Paths_hakatime (version)
import System.Environment.MrEnv (envAsInt, envAsString)
import System.Exit (die)

getDbSettings :: IO HasqlConn.Settings
getDbSettings = do
  dbPort <- w16 <$> envAsInt "HAKA_DB_PORT" 5432
  dbHost <- pack <$> envAsString "HAKA_DB_HOST" "localhost"
  dbName <- pack <$> envAsString "HAKA_DB_NAME" "test"
  dbUser <- pack <$> envAsString "HAKA_DB_USER" "test"
  dbPass <- pack <$> envAsString "HAKA_DB_PASS" "test"
  return $
    HasqlConn.settings dbHost dbPort dbUser dbPass dbName

newtype TokenOpts = TokenOpts {tUsername :: Text}
  deriving (Eq, Show)

newtype UserOpts = UserOpts {cUsername :: Text}
  deriving (Eq, Show)

createToken :: Opt.Parser ServerCommand
createToken =
  CreateToken . TokenOpts
    <$> Opt.strOption
      (Opt.long "username" <> Opt.short 'u' <> Opt.help "The user that the token will be created")

createUser :: Opt.Parser ServerCommand
createUser =
  CreateUser . UserOpts
    <$> Opt.strOption
      (Opt.long "username" <> Opt.short 'u' <> Opt.help "The user to create")

data ServerCommand
  = Run
  | CreateToken TokenOpts
  | CreateUser UserOpts
  deriving (Eq, Show)

serverCommands :: Opt.Parser ServerCommand
serverCommands =
  Opt.subparser
    ( Opt.command
        "create-token"
        (Opt.info createToken (Opt.progDesc "Create a new auth token"))
        <> Opt.command
          "create-user"
          (Opt.info createUser (Opt.progDesc "Create a new user account"))
        <> Opt.command
          "run"
          (Opt.info runCmd (Opt.progDesc "Start the server"))
    )

newtype CliOpts = CliOpts
  { serverCmd :: ServerCommand
  }
  deriving (Show, Eq)

cliOpts :: Opt.Parser (ServerCommand -> CliOpts)
cliOpts = pure CliOpts

runCmd :: Opt.Parser ServerCommand
runCmd = pure Run

opts :: Opt.ParserInfo CliOpts
opts =
  Opt.info
    (Opt.helper <*> cliOpts <*> serverCommands)
    ( Opt.fullDesc
        <> Opt.progDesc
          "Wakatime server implementation"
        <> Opt.header ("hakatime :: v" <> showVersion version)
    )

handleCommand :: ServerCommand -> IO () -> IO ()
handleCommand Run action = action
handleCommand (CreateToken ops) _ = do
  dbSettings <- getDbSettings
  pass <- Utils.passwordInput "Password: "
  pool <- HasqlPool.acquire (10, 1, dbSettings)
  token <- Users.createToken pool username pass
  case token of
    Left err -> die $ unpack err
    Right val ->
      putStrLn $
        "Please save the token. You won't be able to retrieve it again.\n"
          <> unpack val
  where
    username = tUsername ops
handleCommand (CreateUser ops) _ = do
  dbSettings <- getDbSettings
  pass <- Utils.passwordInput "Set a password: "
  pool <- HasqlPool.acquire (10, 1, dbSettings)
  hashUser <- Users.mkUser username pass
  case hashUser of
    Left err -> die (show err)
    Right user -> do
      res <- Users.createUser pool user
      either
        handleError
        ( \_ -> do
            putStrLn $ "User " <> show username <> " created."
            putStrLn $ "Run \"hakatime create-token -u " <> unpack username <> "\" to generate a token."
        )
        res
  where
    username = cUsername ops
    handleError :: HasqlPool.UsageError -> IO ()
    handleError = die . unpack . Utils.toStrError
