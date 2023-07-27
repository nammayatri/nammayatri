{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Domain.Types.LocationMapping where

import qualified Data.Time.Clock.POSIX as Time
import Domain.Types.Location as DLoc
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id

data LocationMapping = LocationMapping
  { id :: Id LocationMapping,
    tag :: LocationMappingTags,
    tagId :: Text,
    location :: Location,
    order :: Int,
    version :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show, Eq)

data LocationMappingTags = SearchRequest | Booking | Ride
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

locationMappingMaker ::
  ( MonadGuid m,
    MonadTime m,
    HasField "fromLocation" entity DLoc.Location,
    HasField "toLocation" entity [DLoc.Location],
    HasField "id" entity (Id entity)
  ) =>
  entity ->
  LocationMappingTags ->
  m [LocationMapping]
locationMappingMaker entity tag = do
  let searchWithIndexes = zip ([1 ..] :: [Int]) entity.toLocation
  toLocationMappers <- mapM (locationMappingInstanceMaker entity tag) searchWithIndexes
  fromLocationMapping <- locationMappingInstanceMaker entity tag (0, entity.fromLocation)
  return $ fromLocationMapping : toLocationMappers

locationMappingInstanceMaker ::
  ( MonadGuid m,
    MonadTime m,
    HasField "id" entity (Id entity)
  ) =>
  entity ->
  LocationMappingTags ->
  (Int, DLoc.Location) ->
  m LocationMapping
locationMappingInstanceMaker entity tag location = do
  locationMappingId <- generateGUID
  epochVersion <- getMappingVersion
  let locationMapping =
        LocationMapping
          { id = Id locationMappingId,
            tag,
            order = fst location,
            version = show epochVersion,
            tagId = entity.id.getId,
            location = snd location,
            ..
          }
  return locationMapping

getMappingVersion :: MonadTime m => m Text
getMappingVersion = do
  now <- getCurrentTime
  let epochVersion = round $ Time.utcTimeToPOSIXSeconds now
  let epochVersionLast5Digits = epochVersion `mod` 100000 :: Integer
  pure $ show epochVersionLast5Digits