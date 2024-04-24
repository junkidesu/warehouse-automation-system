module Lib (
    startApp,
) where

import Configuration.Dotenv (defaultConfig, loadFile, onMissingFile)
import Database (initializeConnectionPool)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger, setPort)
import Network.Wai.Logger (withStdoutLogger)
import Servant
import Servant.Auth.Server (defaultCookieSettings, defaultJWTSettings)
import Swagger

api :: Proxy API
api = Proxy

startApp :: IO ()
startApp = do
    onMissingFile (loadFile defaultConfig) (putStrLn "Missing .env file")

    conns <- initializeConnectionPool

    let
        jwts = defaultJWTSettings
        app = serveWithContext api (defaultCookieSettings :. jwts :. EmptyContext) $ server conns

    withStdoutLogger $ \aplogger -> do
        let settings = setPort 3001 $ setLogger aplogger defaultSettings
        runSettings settings app
