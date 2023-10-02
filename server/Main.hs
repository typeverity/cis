module Main (main) where

import Aws.Lambda (defaultDispatcherOptions)
import Aws.Lambda.Wai (WaiLambdaProxyType (APIGateway), runWaiAsLambda)
import Control.Exception (bracket)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Network.Wai (Application)
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
import Prelude hiding (id)

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
    otelMW <- newOpenTelemetryWaiMiddleware
    runWaiAsLambda APIGateway defaultDispatcherOptions "api-gateway" $ fmap otelMW mkApp

mkApp :: IO Application
mkApp = pure $ serve itemApi server

server :: Server ItemApi
server = getItems :<|> getItemById

getItems :: Servant.Handler [Item]
getItems = pure [exampleItem]

getItemById :: Integer -> Servant.Handler Item
getItemById = \case
    0 -> pure exampleItem
    _ -> throwError err404

exampleItem :: Item
exampleItem = Item{id = 0, text = "example item"}

data Item = Item
    { id :: Integer
    , text :: String
    }
    deriving (Eq, Show, Generic)

instance ToJSON Item
instance FromJSON Item
