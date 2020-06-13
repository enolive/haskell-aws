module Lib where

import           Aws.Lambda
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteString
import           Data.HashMap.Lazy          (HashMap)
import qualified Data.HashMap.Lazy          as HashMap
import           Data.List                  (intercalate)
import           GHC.Generics
import           Text.Read                  (readMaybe)

type PathParameters = HashMap String String

data Event = Event
    { resource       :: String
    , pathParameters :: PathParameters
    }
    deriving (Generic, FromJSON)

data Response = Response
    { statusCode :: Int
    , body       :: String
    }
    deriving (Generic, ToJSON)

handler :: Event -> Context -> IO (Either String Response)
handler Event {..} context =
  pure $ Right $ Response {statusCode = 200, body = (toBody . fmap sequenceUpTo) limit}
  where
    limit = extractLimit pathParameters >>= validateLimit

extractLimit :: PathParameters -> Maybe Int
extractLimit params = HashMap.lookup "input" params >>= readMaybe

validateLimit :: Int -> Maybe Int
validateLimit n
  | n < 0 = Nothing
  | n > 10000 = Nothing
  | otherwise = Just n

single :: Int -> String
single n = if result == "" then show n else result
  where
    result = intercalate "-" [result | (divisor, result) <- rules, n `mod` divisor == 0]
    rules = [(3, "Fizz"), (5, "Buzz")]

sequenceUpTo :: Int -> [String]
sequenceUpTo limit = single <$> [1..limit]

toBody :: ToJSON a => Maybe a -> String
toBody Nothing   = ""
toBody (Just xs) = (ByteString.unpack . encode) xs

