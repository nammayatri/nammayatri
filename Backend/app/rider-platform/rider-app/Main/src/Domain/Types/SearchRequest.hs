{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.SearchRequest where

import qualified Data.Time.Clock.POSIX as Time
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.LocationMapping as DLocationMapping
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person as DP
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.Common

data SearchRequestStatus = NEW | INPROGRESS | CONFIRMED | COMPLETED | CLOSED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data SearchRequest = SearchRequest
  { id :: Id SearchRequest,
    startTime :: UTCTime,
    validTill :: UTCTime,
    riderId :: Id DP.Person,
    fromLocation :: DLoc.Location,
    toLocation :: [DLoc.Location],
    distance :: Maybe HighPrecMeters,
    maxDistance :: Maybe HighPrecMeters,
    estimatedRideDuration :: Maybe Seconds,
    device :: Maybe Text,
    merchantId :: Id DMerchant.Merchant, -- remove when searchRequest will not be used in CustomerSupport
    createdAt :: UTCTime,
    bundleVersion :: Maybe Version,
    clientVersion :: Maybe Version,
    language :: Maybe Maps.Language,
    customerExtraFee :: Maybe Money
  }
  deriving (Generic, Show)

data SearchRequestTable = SearchRequestTable
  { id :: Id SearchRequest,
    startTime :: UTCTime,
    validTill :: UTCTime,
    riderId :: Id DP.Person,
    distance :: Maybe HighPrecMeters,
    maxDistance :: Maybe HighPrecMeters,
    estimatedRideDuration :: Maybe Seconds,
    device :: Maybe Text,
    merchantId :: Id DMerchant.Merchant, -- remove when searchRequest will not be used in CustomerSupport
    createdAt :: UTCTime,
    bundleVersion :: Maybe Version,
    clientVersion :: Maybe Version,
    language :: Maybe Maps.Language,
    customerExtraFee :: Maybe Money
  }
  deriving (Generic, Show)

locationMappingMakerForSearch :: (MonadFlow m) => SearchRequest -> m [DLocationMapping.LocationMapping]
locationMappingMakerForSearch search = do
  let searchWithIndexes = zip ([1 ..] :: [Int]) search.toLocation
  toLocationMappers <- mapM (locationMappingMakerForSearchInstanceMaker search) searchWithIndexes
  fromLocationMapping <- locationMappingMakerForSearchInstanceMaker search (0, search.fromLocation)
  let d = fromLocationMapping : toLocationMappers
  return d

locationMappingMakerForSearchInstanceMaker :: (MonadFlow m) => SearchRequest -> (Int, DLoc.Location) -> m DLocationMapping.LocationMapping
locationMappingMakerForSearchInstanceMaker SearchRequest {..} location = do
  locationMappingId <- generateGUID
  let getIntEpochTime = round `fmap` Time.getPOSIXTime
  epochVersion <- liftIO getIntEpochTime
  let epochVersionLast5Digits = epochVersion `mod` 100000 :: Integer
  let locationMapping =
        DLocationMapping.LocationMapping
          { id = Id locationMappingId,
            tag = DLocationMapping.SearchRequest,
            order = fst location,
            version = show epochVersionLast5Digits,
            tagId = getId id,
            location = snd location,
            ..
          }
  return locationMapping
