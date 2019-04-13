{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (FromJSON, ToJSON)
import qualified Data.ByteString.UTF8   as BSU (ByteString, fromString,
                                                toString)
import           Data.Maybe             (fromMaybe)
import           Data.Sequence          (Seq, empty)
import           Database.Redis         (ConnectInfo, connectHost, connectPort,
                                         defaultConnectInfo, hmset, runRedis)
import qualified Database.Redis         as Red (Connection, Reply, Status,
                                                checkedConnect, hget)
import           GHC.Generics
import           Web.Scotty             (get, json, param, post, scotty)

redisConnInfo :: ConnectInfo
redisConnInfo = defaultConnectInfo --{ connectHost = "172.17.0.2" }

type Vec2 = (Float, Float)

instance ToJSON   Card
instance FromJSON Card
data Card = Card { _name    :: String
                 , content  :: CardData
                 , fontSize :: Int
                 , _pos     :: Vec2
                 } deriving (Eq, Show, Generic)


type CardData = Seq CardID
type CardID   = String

-- Simple makeshift method for fast prototyping
makeCard :: String -> Card
makeCard name = Card name (empty) 16 (0, 0)

-- Redis hash-representation of a card
type RCardHash = [(BSU.ByteString, BSU.ByteString)]

-- For now ignores content
cardToHash :: Card -> RCardHash
cardToHash (Card name _ fSize pos) = [ ("name",     BSU.fromString name)
                                     , ("fontSize", BSU.fromString (show fSize))
                                     , ("pos",      BSU.fromString (show pos))
                                     ]

makeCardAndSaveIt :: Red.Connection -> String -> String -> IO (Either Red.Reply Red.Status)
makeCardAndSaveIt conn cardID = (runRedis conn) . (hmset (BSU.fromString cardID)) . cardToHash . makeCard

-- Hint: Type signs in Redis are weird. m (f (resultType))) means Either Reply resultType outside of transaction, and smth else inside the transaction. So in our code we unwrap it as Either.


main :: IO ()
main = do
    -- Run redis
    conn <- Red.checkedConnect redisConnInfo
    putStrLn " | Redis connected"
    -- Testing redis
    runRedis conn $ hmset "Random Fuck" $ cardToHash $ makeCard "lazyFuck"
    scotty 3000 $ do
        -- Just testing scotty's json capabilities here, no redis involved
        get "/:word" $ do
            cardName <- param "word"
            json $ makeCard cardName
        -- post an entry with hashed card to db, read from db the same card id and return it as json
        post "/card/:id/:name" $ do
            cardID <- param "id"
            name   <- param "name"
            liftIO $ makeCardAndSaveIt conn cardID name
            -- Read back the created card's name and give as a json result
            cardName <- liftIO $ runRedis conn $ do
                result <- Red.hget (BSU.fromString cardID) "name"
                return $ case result of (Left  reply)    -> show reply
                                        (Right maybeStr) -> BSU.toString $ fromMaybe "Card not found" maybeStr
            json $ cardName
    -- In the end - stop the redis connection
    -- Commented out cause it doesn't exist on this older hedis version
    -- Red.disconnect conn
