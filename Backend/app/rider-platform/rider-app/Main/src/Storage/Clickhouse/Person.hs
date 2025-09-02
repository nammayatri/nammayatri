{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Clickhouse.Person where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMC
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Id

data PersonT f = PersonT
  { id :: C f (Id DP.Person),
    firstName :: C f (Maybe Text),
    lastName :: C f (Maybe Text),
    totalRatingScore :: C f (Maybe Int),
    totalRatings :: C f (Maybe Int),
    totalRidesCount :: C f (Maybe Int),
    merchantId :: C f (Id DM.Merchant),
    merchantOperatingCityId :: C f (Maybe (Id DMC.MerchantOperatingCity)),
    createdAt :: C f UTCTime,
    updatedAt :: C f UTCTime
  }
  deriving (Generic)

personTTable :: PersonT (FieldModification PersonT)
personTTable =
  PersonT
    { id = "id",
      firstName = "first_name",
      lastName = "last_name",
      totalRatingScore = "total_rating_score",
      totalRatings = "total_ratings",
      totalRidesCount = "total_rides_count",
      merchantId = "merchant_id",
      merchantOperatingCityId = "merchant_operating_city_id",
      createdAt = "created_at",
      updatedAt = "updated_at"
    }

type Person = PersonT Identity

deriving instance Show Person

$(TH.mkClickhouseInstances ''PersonT 'NO_SELECT_MODIFIER)

findTotalRidesCountByPersonId ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Id DP.Person ->
  m (Maybe Int)
findTotalRidesCountByPersonId personId = do
  personRideCount <-
    CH.findAll $
      CH.select_ (\person -> CH.notGrouped (person.totalRidesCount)) $
        CH.filter_
          (\person -> person.id CH.==. personId)
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE personTTable)
  return $ listToMaybe (catMaybes personRideCount)
