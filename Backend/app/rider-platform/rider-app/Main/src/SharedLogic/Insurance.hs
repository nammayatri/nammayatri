module SharedLogic.Insurance where

import Control.Monad.Extra (maybeM)
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Insurance as DI
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideStatus as DRide
import qualified Domain.Types.VehicleCategory as DVC
import qualified Domain.Types.VehicleVariant as DV
import Kernel.Beam.Functions (runInReplica)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Insurance.Interface.Types as Insurance
import Kernel.Prelude
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.InsuranceConfig as CQInsuranceConfig
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Insurance as QInsurance
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import qualified Tools.Insurance as TI

createInsurance :: (MonadFlow m, MonadReader r m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => DRide.Ride -> m ()
createInsurance ride = do
  unless (ride.status == DRide.INPROGRESS) $ throwError $ InvalidRequest "Insurance will be generated after ride is started!"
  booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId:-" <> ride.bookingId.getId)
  mbInsurance <- QInsurance.findById (Id.Id ride.id.getId)
  when (isNothing mbInsurance) $ do
    person <- QPerson.findById booking.riderId >>= fromMaybeM (PersonDoesNotExist $ "RiderId:-" <> booking.riderId.getId)
    let personName = (fromMaybe "" person.firstName) <> " " <> (fromMaybe "" person.lastName)
    personPhone <- mapM decrypt person.mobileNumber >>= fromMaybeM (InternalError "Person phone not found")
    let vehicleCategory = DV.castServiceTierToVehicleCategory booking.vehicleServiceTierType
    insuranceConfig <- (maybeM (pure Nothing) (\tp -> CQInsuranceConfig.getInsuranceConfig booking.merchantId booking.merchantOperatingCityId tp vehicleCategory) $ pure booking.tripCategory) >>= fromMaybeM (InternalError "Insurance config not found")
    now <- getCurrentTime
    let startTime = fromMaybe now ride.rideStartTime
        endTime = addHours (fromIntegral insuranceConfig.hours) startTime
    let insuranceRequest =
          Insurance.InsuranceRequest
            { trip =
                Insurance.Trip
                  { journey =
                      [ Insurance.Journey
                          { mode = Just $ vehicleCategoryToInsuranceMode vehicleCategory,
                            origin =
                              Insurance.Location
                                { city = insuranceConfig.city
                                },
                            person =
                              [ Insurance.Person
                                  { insured =
                                      Insurance.Insured
                                        { name = personName,
                                          phone = personPhone
                                        }
                                  },
                                Insurance.Person
                                  { insured =
                                      Insurance.Insured
                                        { name = ride.driverName,
                                          phone = ride.driverMobileNumber
                                        }
                                  }
                              ],
                            destination =
                              Insurance.Location
                                { city = insuranceConfig.city
                                }
                          }
                      ],
                    endDate = endTime,
                    bookingId = ride.id.getId,
                    startDate = startTime,
                    bookingDate = booking.createdAt
                  },
              category = "trip",
              customer =
                Insurance.Customer
                  { id = person.id.getId,
                    name = personName,
                    phone = personPhone,
                    state = insuranceConfig.state
                  },
              planType = fromMaybe 1 insuranceConfig.planType,
              referenceId = ride.id.getId,
              plan = insuranceConfig.plan,
              partnerId = insuranceConfig.partnerId
            }
    res <- TI.createInsurance booking.merchantId booking.merchantOperatingCityId insuranceRequest
    createInsuranceEntry ride booking insuranceConfig.partnerId insuranceConfig.plan (personName, personPhone) (insuranceConfig.insuredAmount, insuranceConfig.driverInsuredAmount) res
  where
    addHours :: NominalDiffTime -> UTCTime -> UTCTime
    addHours hrs time = addUTCTime (hrs * 3600) time

vehicleCategoryToInsuranceMode :: DVC.VehicleCategory -> Text
vehicleCategoryToInsuranceMode = \case
  DVC.CAR -> "cab"
  DVC.MOTORCYCLE -> "bike"
  DVC.TRAIN -> "train"
  DVC.BUS -> "bus"
  DVC.FLIGHT -> "flight"
  _ -> "others"

createInsuranceEntry :: (MonadFlow m, MonadReader r m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => DRide.Ride -> DBooking.Booking -> Text -> Text -> (Text, Text) -> (Maybe Text, Maybe Text) -> Insurance.InsuranceResponse -> m ()
createInsuranceEntry ride booking partnerId plan (personName, personPhone) (insuredAmount, driverInsuredAmount) insuranceResponse = do
  now <- getCurrentTime
  let insurance =
        DI.Insurance
          { id = Id.Id ride.id.getId,
            policyId = insuranceResponse.policyId,
            category = "trip",
            entityType = DI.RIDE,
            tripCategory = booking.tripCategory,
            certificateUrl = insuranceResponse.certificatePdf,
            partnerId = partnerId,
            plan = plan,
            startDate = insuranceResponse.startDate,
            endDate = insuranceResponse.endDate,
            DI.policyNumber = insuranceResponse.policyNumber,
            customerId = Id.Id booking.riderId.getId,
            customerName = personName,
            customerPhone = personPhone,
            driverName = Just ride.driverName,
            driverPhone = Just ride.driverMobileNumber,
            insuredAmount = insuredAmount,
            driverInsuredAmount = driverInsuredAmount,
            fairBreakup = Nothing,
            merchantId = booking.merchantId,
            merchantOperatingCityId = booking.merchantOperatingCityId,
            createdAt = now,
            updatedAt = now
          }
  QInsurance.create insurance
