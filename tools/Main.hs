--
-- CLI tool that generates fake activity hearbeats.
--
import Control.Monad (replicateM)
import Data.Text (Text, pack)
import Data.Time (addUTCTime)
import Data.Time.Clock.POSIX
import Faker
import Faker.Combinators
import qualified Faker.DateTime
import qualified Faker.Internet
import Haka.Api (heartbeatApi)
import Haka.Errors (HeartbeatApiResponse (..))
import Haka.Types
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Options.Applicative
import Servant.API
import Servant.Client
import System.Random (randomRIO)

fakeLang :: Fake Text
fakeLang =
  elements
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

fakeProject :: Fake Text
fakeProject =
  elements
    [ "hakatime",
      "my-app",
      "bootstrap",
      "jQuery",
      "Kubernetes",
      "nixpkgs",
      "zulip",
      "Pandas"
    ]

fakeFilename :: Fake Text
fakeFilename = do
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
  ext <- elements [".cpp", ".go", ".hs", ".json", ".js", ".ts", ".py", ".rb", ".yaml", ".rs"]
  pure $ pack $ filename ++ ext

generateTimeline :: IO [HeartbeatPayload]
generateTimeline = do
  let settings = setNonDeterministic defaultFakerSettings
  steps <- randomRIO (10, 80)
  start <- generateWithSettings settings (Faker.DateTime.utcBetweenYears 2020 2020)
  lang <- generateWithSettings settings fakeLang
  proj <- generateWithSettings settings fakeProject
  filename <- generateWithSettings settings fakeFilename
  user_agent' <- generateWithSettings settings Faker.Internet.userAgentFirefox
  mapM
    ( \i ->
        pure $
          HeartbeatPayload
            { sender = Just "demo",
              category = Nothing,
              editor = Just "vim",
              plugin = Nothing,
              platform = Nothing,
              user_agent = user_agent',
              branch = Just "master",
              language = Just lang,
              project = Just proj,
              ty = FileType,
              machine = Just "laptop",
              time_sent = fromIntegral $ floor $ utcTimeToPOSIXSeconds i,
              entity = filename,
              cursorpos = Nothing,
              dependencies = Nothing,
              lineno = Nothing,
              file_lines = Nothing,
              is_write = Nothing
            }
    )
    (take steps $ iterate (addUTCTime 120) start)

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
  timelineBeats <- concat <$> replicateM (totalHeartbeats parsedConf) generateTimeline
  mgr <- mkMgr parsedConf
  r <-
    runClientM
      (sendHeartbeats (Just "laptop") (Just $ ApiToken (pack $ token parsedConf)) timelineBeats)
      (mkEnv mgr parsedConf)
  case r of
    Left e -> print e
    _ -> putStrLn "Heartbeats sent succefully!"
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
