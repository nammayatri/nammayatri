{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Clickhouse.Person where

import Control.Lens ((^?), _head)
import Data.Time
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
    clientOsType :: C f (Maybe Text),
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
      clientOsType = "client_os_type",
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
  return $ catMaybes personRideCount ^? _head

-- Aggregated registration metrics result
data RegistrationMetrics = RegistrationMetrics
  { metricsClientOsType :: Maybe Text,
    metricsUserCount :: Int
  }
  deriving (Generic, Show)

-- Get aggregated registration metrics by date range using GROUP BY
getRegistrationMetricsByDateRange ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Id DMC.MerchantOperatingCity ->
  Day -> -- Target date for IST
  m [RegistrationMetrics]
getRegistrationMetricsByDateRange merchantOpCityId targetDate = do
  let istOffset = 19800 :: NominalDiffTime -- 5 hours 30 minutes in seconds
      startOfDayUTC = addUTCTime (negate istOffset) (UTCTime targetDate 0)
      endOfDayUTC = addUTCTime (86400 - istOffset) (UTCTime targetDate 0) -- 24 hours later in IST
  results <-
    CH.findAll $
      CH.select_
        ( \person ->
            CH.groupBy person.clientOsType $ \osType ->
              let userCount = CH.count_ person.id
               in (osType, userCount)
        )
        $ CH.filter_
          ( \person ->
              person.merchantOperatingCityId CH.==. Just merchantOpCityId
                CH.&&. person.createdAt >=. startOfDayUTC
                CH.&&. person.createdAt <. endOfDayUTC
                CH.&&. CH.isNotNull person.clientOsType
          )
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE personTTable)

  pure $
    map
      ( \(osType, count) ->
          RegistrationMetrics
            { metricsClientOsType = osType,
              metricsUserCount = count
            }
      )
      results
