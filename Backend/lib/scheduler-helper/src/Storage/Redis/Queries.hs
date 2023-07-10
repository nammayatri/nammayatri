{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Storage.Redis.Queries where

import Control.Monad
import Data.Aeson as A
import Data.ByteString (ByteString)
import Data.Maybe (mapMaybe)
import Data.String.Conversions
import Data.Text (Text)
import qualified Database.Redis as Hedis
import Storage.Redis.Types
import Prelude

decodeEither :: (FromJSON b) => (Either Hedis.Reply ByteString) -> (Either String b)
decodeEither eitherVal =
  case eitherVal of
    Left err -> Left $ show err
    Right byteValue -> A.eitherDecode $ cs byteValue

decodeReadRes :: (FromJSON a, ToJSON a) => (Either Hedis.Reply (Maybe [Hedis.XReadResponse])) -> [[(Text, a)]]
decodeReadRes eitherVal = do
  case eitherVal of
    Right (Just groups) -> join $ traverse (\(Hedis.XReadResponse _ streamRecords) -> decodeStreamRecords streamRecords) groups
    _ -> []

decodeRes :: (FromJSON a, ToJSON a) => (Either Hedis.Reply [Hedis.StreamsRecord]) -> [[(Text, a)]]
decodeRes eitherVal = do
  case eitherVal of
    Left _ -> []
    Right streamRecords -> decodeStreamRecords streamRecords

decodeStreamRecords :: (FromJSON a, ToJSON a) => [Hedis.StreamsRecord] -> [[(Text, a)]]
decodeStreamRecords = map (\(Hedis.StreamsRecord _ arrVal) -> mapMaybe (\(field, value) -> (cs field,) <$> A.decode (cs value)) arrVal) -- ignoring recordId here

xAdd :: (FromJSON a, ToJSON a, FromJSON b, ToJSON b) => Hedis.Connection -> Text -> EntryId -> [(Text, a)] -> IO (Either String b)
xAdd con stream entryId values = decodeEither <$> Hedis.runRedis con (Hedis.xadd (cs stream) (cs $ show entryId) (map (\(field, value) -> (cs field, cs $ A.encode value)) values))

xReadGroupOpts :: (FromJSON a, ToJSON a) => Hedis.Connection -> Consumer -> [(Text, EntryId)] -> BlockTime -> Count -> IO [[(Text, a)]]
xReadGroupOpts con (Consumer groupName consumerName) streamIdMap (BlockTime mbBlockTime) (Count mbCount) =
  decodeReadRes <$> Hedis.runRedis con (Hedis.xreadGroupOpts (cs groupName) (cs consumerName) (map (\(stream, entryId) -> (cs stream, cs $ show entryId)) streamIdMap) (Hedis.XReadOpts mbBlockTime mbCount True))

xRange :: (FromJSON a, ToJSON a) => Hedis.Connection -> Text -> EntryId -> EntryId -> Count -> IO [[(Text, a)]]
xRange con streamName startId endId (Count mbCount) = decodeRes <$> Hedis.runRedis con (Hedis.xrange (cs streamName) (cs $ show startId) (cs $ show endId) mbCount)
