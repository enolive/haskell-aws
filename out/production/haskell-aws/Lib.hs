module Lib where

import           Data.Aeson
import           GHC.Generics

import           Aws.Lambda
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap

data Event = Event
    { resource       :: String
    , pathParameters :: Maybe (HashMap String String)
    }
    deriving (Generic, FromJSON)

data Response = Response
    { statusCode :: Int
    , body       :: String
    }
    deriving (Generic, ToJSON)

handler :: Event -> Context -> IO (Either String Response)
handler Event{..} context =
  pure $ Right $ Response { statusCode = 200, body = "Success" }
