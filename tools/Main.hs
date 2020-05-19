--
-- CLI tool that generates fake activity hearbeats.
--
import Control.Monad (replicateM)
import Data.Text (Text, pack)
import Data.Time.Clock.POSIX
import Faker
import Faker.Combinators
import qualified Faker.DateTime
import qualified Faker.Internet
import Haka.Api (heartbeatApi)
import Haka.Types
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Options.Applicative
import Servant.API
import Servant.Client
import Text.Pretty.Simple (pPrint)

fakePerson :: Fake HeartbeatPayload
fakePerson = do
  filename <-
    elements
      [ "src/app",
        "main",
        "db/test_runner",
        "data/fixtures",
        "src/controller",
        "main/component",
        "pkg/adapter/main",
        "src/init",
        "keys",
        "src/spinner",
        "ui/scrollbar",
        "src/ui/test",
        "src/ui/button",
        "api/v1/service",
        "api/v1/volume",
        "api/v1/metadata",
        "api/v1/db",
        "package",
        "test",
        "utils",
        "commons",
        "manager",
        "config",
        "resources"
      ]
  ext <-
    elements
      [ ".cpp",
        ".go",
        ".hs",
        ".json",
        ".js",
        ".ts",
        ".py",
        ".rb",
        ".yaml",
        ".rs"
      ]
  language' <-
    Just
      <$> elements
        [ "C++",
          "Docker",
          "Go",
          "Haskell",
          "JSON",
          "JavaScript",
          "Nix",
          "Python",
          "Ruby",
          "TypeScript",
          "YAML"
        ]
  project' <-
    Just
      <$> elements
        [ "hakatime",
          "my-app",
          "bootstrap",
          "jQuery",
          "Kubernetes",
          "nixpkgs",
          "zulip",
          "Pandas"
        ]
  user_agent' <- Faker.Internet.userAgentFirefox
  time_sent' <-
    fromIntegral . floor . utcTimeToPOSIXSeconds
      <$> Faker.DateTime.utcBetweenYears 2020 2020

  pure $
    HeartbeatPayload
      { sender = Just "demo",
        category = Nothing,
        editor = Just "vim",
        plugin = Nothing,
        platform = Nothing,
        user_agent = user_agent',
        branch = Just "master",
        language = language',
        project = project',
        ty = FileType,
        machine = Just "laptop",
        time_sent = time_sent',
        entity = pack $ filename ++ ext,
        cursorpos = Nothing,
        dependencies = Nothing,
        lineno = Nothing,
        file_lines = Nothing,
        is_write = Nothing
      }

run :: IO HeartbeatPayload
run = do
  let settings = setNonDeterministic defaultFakerSettings
  generateWithSettings settings fakePerson

main :: IO ()
main = runClient

sendHeartbeat :: Maybe Text -> Maybe ApiToken -> HeartbeatPayload -> ClientM HeartbeatApiResponse

sendHeartbeats :: Maybe Text -> Maybe ApiToken -> [HeartbeatPayload] -> ClientM HeartbeatApiResponse
sendHeartbeat :<|> sendHeartbeats = client heartbeatApi

data Config = Config
  { urlEndpoint :: String,
    port :: Int,
    token :: String,
    totalHeartbeats :: Int
  }

config :: Parser Config
config =
  Config
    <$> strOption
      ( long "url"
          <> help "Endpoint to send the data"
          <> metavar "ENDPOINT"
          <> showDefault
          <> value "localhost"
          <> short 'u'
      )
    <*> option
      auto
      ( long "port"
          <> help "The port to use"
          <> metavar "PORT"
          <> showDefault
          <> value 8080
          <> short 'p'
      )
    <*> strOption
      ( long "token"
          <> help "The API token to use for authentication"
          <> metavar "API_TOKEN"
          <> short 't'
      )
    <*> option
      auto
      ( long "num"
          <> help "Number of fake hearbeats to send"
          <> metavar "HEARTBEATS_NUM"
          <> showDefault
          <> value 100
          <> short 'n'
      )

opts :: ParserInfo Config
opts =
  info
    (helper <*> config)
    ( fullDesc
        <> progDesc "CLI tool to send fake data in a hakatime instance"
        <> header "haka-data"
    )

runClient :: IO ()
runClient = do
  parsedConf <- execParser opts
  beats <- replicateM (totalHeartbeats parsedConf) run
  mgr <- mkMgr parsedConf
  res <-
    runClientM
      (sendHeartbeats (Just "laptop") (Just $ ApiToken (pack $ token parsedConf)) beats)
      (mkEnv mgr parsedConf)
  case res of
    Left err -> print err
    Right r -> pPrint r
  where
    mkMgr conf =
      if port conf == 443
        then newManager tlsManagerSettings
        else newManager defaultManagerSettings
    mkEnv mgr conf =
      let url = urlEndpoint conf
          p = port conf
       in if p == 443
            then mkClientEnv mgr (BaseUrl Https url p "")
            else mkClientEnv mgr (BaseUrl Http url p "")
