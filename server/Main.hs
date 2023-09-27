module Main (main) where

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
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)
import Text.Read (readEither)

{- FOURMOLU_DISABLE -}

type ItemApi
       = "item" :> Get '[JSON] [Item]
    :<|> "item" :> Capture "itemId" Integer
                :> Get '[JSON] Item

{- FOURMOLU_ENABLE -}

itemApi :: Proxy ItemApi
itemApi = Proxy

main :: IO ()
main =
    do
        port <- readPort 3000
        let settings =
                defaultSettings
                    & setPort port
                    & setBeforeMainLoop (putStrLn $ "listening on port " ++ show port)
        app <- mkApp
        runSettings settings app

readPort :: Port -> IO Port
readPort defaultPort = do
    envPort <- lookupEnv "PORT"
    maybe (pure defaultPort) parsePort envPort
  where
    parsePort portString =
        readEither portString
            & either (complain portString) pure
    complain portString err =
        defaultPort `withComplaint` printf "warning: could not parse value of PORT (`%s`), using default port %d instead: %s" portString defaultPort err

withComplaint :: a -> String -> IO a
withComplaint value msg = do
    hPutStrLn stderr msg
    pure value

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
