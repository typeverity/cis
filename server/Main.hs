module Main (main) where

import Control.Exception (bracket)
import Data.Aeson (FromJSON, ToJSON)
import Data.Function ((&))
import GHC.Generics (Generic)
import Network.Wai (Application)
import Network.Wai.Handler.Warp
    ( Port
    , defaultSettings
    , runSettings
    , setBeforeMainLoop
    , setPort
    )
import OpenTelemetry.Instrumentation.Wai (newOpenTelemetryWaiMiddleware)
import OpenTelemetry.Trace
    ( initializeGlobalTracerProvider
    , shutdownTracerProvider
    )
import Servant
    ( Capture
    , Get
    , Handler
    , JSON
    , Proxy (..)
    , Server
    , err404
    , serve
    , throwError
    , type (:<|>) (..)
    , type (:>)
    )
import System.Envy (FromEnv, decodeWithDefaults)
import Text.Printf (printf)

data Config = Config
    {port :: Port}
    deriving (Generic)

defaultConfig :: Config
defaultConfig = Config{port = 3000}

instance FromEnv Config

{- FOURMOLU_DISABLE -}

type ItemApi
       = "item" :> Get '[JSON] [Item]
    :<|> "item" :> Capture "itemId" Integer
                :> Get '[JSON] Item

{- FOURMOLU_ENABLE -}

itemApi :: Proxy ItemApi
itemApi = Proxy

main :: IO ()
main = bracket initializeGlobalTracerProvider shutdownTracerProvider $ const do
    config <- decodeWithDefaults defaultConfig
    app <- mkApp
    otelMW <- newOpenTelemetryWaiMiddleware
    app
        & otelMW
        & runSettings
            ( defaultSettings
                & setPort config.port
                & setBeforeMainLoop (putStrLn $ printf "listening on port %d" config.port)
            )

mkApp :: IO Application
mkApp = pure $ serve itemApi server

server :: Server ItemApi
server = getItems :<|> getItemById

getItems :: Handler [Item]
getItems = return [exampleItem]

getItemById :: Integer -> Handler Item
getItemById = \case
    0 -> return exampleItem
    _ -> throwError err404

exampleItem :: Item
exampleItem = Item 0 "example item"

data Item = Item
    { itemId :: Integer
    , itemText :: String
    }
    deriving (Eq, Show, Generic)

instance ToJSON Item
instance FromJSON Item
