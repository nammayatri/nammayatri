module Domain.Action.Internal.BlackListedDrivers where

import Domain.Action.Internal.FavouriteDrivers (GetFavouriteDriverInfoReq (..))
import Domain.Types.DriverStats
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as Ride
import qualified Domain.Types.RiderDriverCorrelation as RDCD
import EulerHS.Prelude hiding (id, map)
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt, encrypt, getDbHash)
import Kernel.Prelude hiding (whenJust)
import Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.DriverStats as QDS
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRD
import qualified Storage.Queries.RiderDriverCorrelation as RDC
import Tools.Error

data BlackListDriverReq = BlackListDriverReq
  { customerMobileNumber :: Text,
    customerMobileCountryCode :: Text,
    blackListed :: Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data BlackListedDriverResp = BlackListedDriverResp
  { id :: Text,
    driverName :: Text,
    driverPhone :: Maybe Text,
    driverRating :: Maybe Centesimal
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

blackListDriver :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => Id Merchant -> Id Ride.Ride -> Maybe Text -> BlackListDriverReq -> m APISuccess
blackListDriver merchantId rideId apiKey BlackListDriverReq {..} = do
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  unless (ride.merchantId == Just merchantId) $ throwError (RideDoesNotExist rideId.getId)
  let driverId = ride.driverId
  numberHash <- getDbHash customerMobileNumber
  rider <- B.runInReplica $ QRD.findByMobileNumberHashAndMerchant numberHash merchant.id >>= fromMaybeM (InternalError "Rider does not exist")
  mbCorrelation <- RDC.findByRiderIdAndDriverId rider.id driverId
  case mbCorrelation of
    Just _ -> RDC.updateBlackListedDriverForRider (Just blackListed) rider.id driverId
    Nothing ->
      when blackListed $ do
        now <- getCurrentTime
        encPhone <- encrypt customerMobileNumber
        mocId <- rider.merchantOperatingCityId & fromMaybeM (InternalError "No merchant operating city found for rider")
        let corr =
              RDCD.RiderDriverCorrelation
                { riderDetailId = rider.id,
                  driverId = driverId,
                  merchantId = merchant.id,
                  merchantOperatingCityId = mocId,
                  createdAt = now,
                  updatedAt = now,
                  favourite = False,
                  blackListed = Just True,
                  mobileNumber = encPhone
                }
        RDC.create corr
  pure APISuccess.Success

getBlackListedDrivers :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => Id Merchant -> Maybe Text -> GetFavouriteDriverInfoReq -> m [BlackListedDriverResp]
getBlackListedDrivers merchantId apiKey GetFavouriteDriverInfoReq {..} = do
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  numberHash <- getDbHash customerMobileNumber
  mbRider <- B.runInReplica $ QRD.findByMobileNumberHashAndMerchant numberHash merchant.id
  case mbRider of
    Just rider -> do
      correlationRes <- RDC.findBlackListedDriversForRider rider.id (Just True)
      let blackListedDrivers = map (.driverId) correlationRes
      catMaybes
        <$> forM
          blackListedDrivers
          ( \blackListedDriverId -> do
              mbPerson <- B.runInReplica $ QP.findById blackListedDriverId
              case mbPerson of
                Nothing -> pure Nothing
                Just person -> do
                  personStat <- B.runInReplica $ QDS.findById blackListedDriverId
                  Just <$> mkBlackListedDriverResp person personStat
          )
    Nothing -> pure []
  where
    mkBlackListedDriverResp :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => Person.Person -> Maybe DriverStats -> m BlackListedDriverResp
    mkBlackListedDriverResp person personStat = do
      driverPhone <- mapM decrypt person.mobileNumber
      pure $
        BlackListedDriverResp
          { id = person.id.getId,
            driverName = person.firstName,
            driverPhone = driverPhone,
            driverRating = personStat >>= (.rating)
          }

unblockDriver :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => Id Merchant -> Id Person.Person -> Maybe Text -> GetFavouriteDriverInfoReq -> m APISuccess
unblockDriver merchantId driverId apiKey GetFavouriteDriverInfoReq {..} = do
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  numberHash <- getDbHash customerMobileNumber
  rider <- B.runInReplica $ QRD.findByMobileNumberHashAndMerchant numberHash merchant.id >>= fromMaybeM (InternalError "Rider does not exist")
  mbCorrelation <- RDC.findByRiderIdAndDriverId rider.id driverId
  whenJust mbCorrelation $ \correlation ->
    when (correlation.blackListed == Just True) $
      RDC.updateBlackListedDriverForRider (Just False) rider.id driverId
  pure APISuccess.Success
