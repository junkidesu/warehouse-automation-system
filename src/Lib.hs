module Lib (
    startApp,
) where

import Configuration.Dotenv (defaultConfig, loadFile, onMissingFile)
import Cors (myCors)
import Database (initializeConnectionPool)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger, setPort)
import Network.Wai.Logger (withStdoutLogger)
import Servant
import Servant.Auth.Server (defaultCookieSettings, defaultJWTSettings, generateKey)
import Swagger

api :: Proxy API
api = Proxy

startApp :: IO ()
startApp = do
    onMissingFile (loadFile defaultConfig) (putStrLn "Missing .env file")

    conns <- initializeConnectionPool

    key <- generateKey

    let
        jwts = defaultJWTSettings key
        app =
            myCors $
                serveWithContext api (defaultCookieSettings :. jwts :. EmptyContext) $
                    server conns jwts

    withStdoutLogger $ \aplogger -> do
        let settings = setPort 3001 $ setLogger aplogger defaultSettings
        runSettings settings app
