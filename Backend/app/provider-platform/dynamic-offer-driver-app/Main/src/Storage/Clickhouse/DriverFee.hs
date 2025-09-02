{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Clickhouse.DriverFee where

import qualified "this" API.Types.Dashboard.RideBooking.Driver as Common
import qualified Data.Time.Calendar as Time
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Volunteer as DVolunteer
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Common (Centesimal)
import Kernel.Types.Id

-- see module Dashboard.ProviderPlatform.Revenue

data DriverFeeT f = DriverFeeT
  { id :: C f (Id DriverFee),
    merchantId :: C f (Id DM.Merchant),
    driverId :: C f (Maybe (Id DP.Driver)),
    status :: C f (Maybe Common.DriverFeeStatus),
    numRides :: C f (Maybe Int),
    platformFee :: C f (Maybe Centesimal),
    cgst :: C f (Maybe Centesimal),
    sgst :: C f (Maybe Centesimal),
    specialZoneAmount :: C f (Maybe Centesimal),
    govtCharges :: C f (Maybe Int),
    collectedAt :: C f CH.DateTime, -- DateTime on clickhouse side
    collectedBy :: C f (Maybe (Id DVolunteer.Volunteer)),
    updatedAt :: C f UTCTime
  }
  deriving (Generic)

deriving instance Show DriverFee

-- TODO move to TH (quietSnake)
driverFeeTTable :: DriverFeeT (FieldModification DriverFeeT)
driverFeeTTable =
  DriverFeeT
    { id = "id",
      merchantId = "merchant_id",
      driverId = "driver_id",
      status = "status",
      numRides = "num_rides",
      platformFee = "platform_fee",
      cgst = "cgst",
      sgst = "sgst",
      specialZoneAmount = "special_zone_amount",
      govtCharges = "govt_charges",
      collectedAt = "collected_at",
      collectedBy = "collected_by",
      updatedAt = "updated_at"
    }

type DriverFee = DriverFeeT Identity

$(TH.mkClickhouseInstances ''DriverFeeT 'NO_SELECT_MODIFIER)

data DriverFeeAggregated = DriverFeeAggregated
  { statusAgg :: Maybe Common.DriverFeeStatus,
    numRidesAgg :: Maybe Int,
    numDrivers :: Int,
    totalAmount :: Maybe Centesimal,
    specialZoneAmt :: Maybe Centesimal,
    date :: Maybe Time.Day,
    hour :: Maybe Int
  }
  deriving (Show)

-- up to 6 columns supported now

findAllByStatusSubSelect ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Id DM.Merchant ->
  [Common.DriverFeeStatus] ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  m [DriverFeeAggregated]
findAllByStatusSubSelect merchantId statuses mbFrom mbTo = do
  driverFeeTuple <-
    CH.findAll $
      CH.select_
        ( \(status, numRides, driverId, totalAmount, specialZoneAmount) -> do
            CH.groupBy status $ \statusAgg -> do
              let totalAmountAgg = CH.sum_ totalAmount
                  numRidesAgg = CH.sum_ numRides
                  numDriversAgg = CH.count_ (CH.distinct driverId)
                  specialZoneAmountAgg = CH.sum_ specialZoneAmount
              (statusAgg, numRidesAgg, numDriversAgg, totalAmountAgg, specialZoneAmountAgg)
        )
        $ CH.emptyFilter $
          CH.subSelect_ $
            CH.select_
              ( \driverFee -> do
                  CH.groupBy driverFee.id $ \_idAgg -> do
                    let totalAmountAgg =
                          flip CH.argMax driverFee.updatedAt $
                            driverFee.platformFee
                              CH.+. driverFee.cgst
                              CH.+. driverFee.sgst
                              CH.+. CH.unsafeCoerceNum @(Maybe Int) @(Maybe Centesimal) driverFee.govtCharges
                        numRidesAgg = flip CH.argMax driverFee.updatedAt driverFee.numRides
                        driverIdAgg = flip CH.argMax driverFee.updatedAt driverFee.driverId
                        specialZoneAmountAgg = flip CH.argMax driverFee.updatedAt driverFee.specialZoneAmount
                        statusAgg = flip CH.argMax driverFee.updatedAt driverFee.status
                    (statusAgg, numRidesAgg, driverIdAgg, totalAmountAgg, specialZoneAmountAgg)
              )
              $ CH.filter_
                ( \driverFee ->
                    driverFee.merchantId CH.==. merchantId
                      CH.&&. driverFee.status `in_` (Just <$> statuses)
                      CH.&&. CH.whenJust_ mbFrom (\from -> driverFee.collectedAt >=. CH.DateTime from)
                      CH.&&. CH.whenJust_ mbTo (\to -> driverFee.collectedAt <=. CH.DateTime to)
                )
                (CH.all_ @CH.APP_SERVICE_CLICKHOUSE driverFeeTTable)
  pure $ mkDriverFeeByStatus <$> driverFeeTuple

-- up to 6 columns supported now
-- mbCollBy = Just [] ---> condition = False
-- mbCollBy = Nothing ---> condition = True

findAllByDateSubSelect ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Id DM.Merchant ->
  [Common.DriverFeeStatus] ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Bool ->
  Maybe [Id DVolunteer.Volunteer] ->
  m [DriverFeeAggregated]
findAllByDateSubSelect merchantId statuses mbFrom mbTo dayBasis mbCollBy = do
  driverFeeTuple <-
    CH.findAll $
      CH.select_
        ( \(totalAmount, specialZoneAmount, numRides, driverId, date, hour) -> do
            CH.groupBy (date, hour) $ \(dateAgg, hourAgg) -> do
              let totalAmountAgg = CH.sum_ totalAmount
                  numRidesAgg = CH.sum_ numRides
                  numDriversAgg = CH.count_ (CH.distinct driverId)
                  specialZoneAmountAgg = CH.sum_ specialZoneAmount
              (totalAmountAgg, specialZoneAmountAgg, numRidesAgg, numDriversAgg, dateAgg, hourAgg)
        )
        $ CH.orderBy_ (\_ (_, _, _, _, date, hour) -> CH.asc (date, hour)) $
          CH.emptyFilter $
            CH.subSelect_ $
              CH.select_
                ( \driverFee -> do
                    CH.groupBy driverFee.id $ \_idAgg -> do
                      let totalAmountAgg =
                            flip CH.argMax driverFee.updatedAt $
                              driverFee.platformFee
                                CH.+. driverFee.cgst
                                CH.+. driverFee.sgst
                                CH.+. CH.unsafeCoerceNum @(Maybe Int) @(Maybe Centesimal) driverFee.govtCharges
                          specialZoneAmountAgg = flip CH.argMax driverFee.updatedAt driverFee.specialZoneAmount
                          numRidesAgg = flip CH.argMax driverFee.updatedAt driverFee.numRides
                          driverIdAgg = flip CH.argMax driverFee.updatedAt driverFee.driverId
                          dateAgg = flip CH.argMax driverFee.updatedAt $ CH.toDate driverFee.collectedAt
                          hourAgg = flip CH.argMax driverFee.updatedAt $ if dayBasis then CH.valColumn 0 else CH.toHour driverFee.collectedAt
                      (totalAmountAgg, specialZoneAmountAgg, numRidesAgg, driverIdAgg, dateAgg, hourAgg)
                )
                $ CH.filter_
                  ( \driverFee ->
                      driverFee.merchantId CH.==. merchantId
                        CH.&&. driverFee.status `in_` (Just <$> statuses)
                        CH.&&. CH.whenJust_ mbFrom (\from -> driverFee.collectedAt >=. CH.DateTime from)
                        CH.&&. CH.whenJust_ mbTo (\to -> driverFee.collectedAt <=. CH.DateTime to)
                        CH.&&. CH.whenJust_ mbCollBy (\collBy -> driverFee.collectedBy `in_` (Just <$> collBy))
                  )
                  $ CH.all_ @CH.APP_SERVICE_CLICKHOUSE driverFeeTTable
  pure $ mkDriverFeeByDate <$> driverFeeTuple

mkDriverFeeByStatus :: (Maybe Common.DriverFeeStatus, Maybe Int, Int, Maybe Centesimal, Maybe Centesimal) -> DriverFeeAggregated
mkDriverFeeByStatus (statusAgg, numRidesAgg, numDrivers, totalAmount, specialZoneAmt) = DriverFeeAggregated {date = Nothing, hour = Nothing, ..}

mkDriverFeeByDate :: (Maybe Centesimal, Maybe Centesimal, Maybe Int, Int, Time.Day, Int) -> DriverFeeAggregated
mkDriverFeeByDate (totalAmount, specialZoneAmt, numRidesAgg, numDrivers, date, hour) = DriverFeeAggregated {statusAgg = Nothing, date = Just date, hour = Just hour, ..}
