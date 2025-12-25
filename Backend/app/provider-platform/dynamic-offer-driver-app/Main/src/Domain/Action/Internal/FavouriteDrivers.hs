module Domain.Action.Internal.FavouriteDrivers where

import Domain.Types.DriverStats
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Person as Person
import EulerHS.Prelude hiding (id, map)
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt, getDbHash)
import Kernel.Prelude hiding (whenJust)
import Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as QM
import Storage.Queries.DriverStats as QDS
import Storage.Queries.Person as QP
import qualified Storage.Queries.RiderDetails as QRD
import qualified Storage.Queries.RiderDriverCorrelation as RDC
import Tools.Error

data GetFavouriteDriverInfoReq = GetFavouriteDriverInfoReq
  { customerMobileNumber :: Text,
    customerMobileCountryCode :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data FavouriteDriverResp = FavouriteDriverResp
  { driverName :: Text,
    id :: Text,
    favCount :: Int,
    driverPhone :: Maybe Text,
    driverRating :: Maybe Centesimal
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

getFavouriteDrivers :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => Id Merchant -> Maybe Text -> GetFavouriteDriverInfoReq -> m [FavouriteDriverResp]
getFavouriteDrivers merchantId apiKey GetFavouriteDriverInfoReq {..} = do
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  numberHash <- getDbHash customerMobileNumber
  mbRider <- B.runInReplica $ QRD.findByMobileNumberHashAndMerchant numberHash merchant.id -- >>= fromMaybeM (InternalError "Rider does not exist")
  case mbRider of
    Just rider -> do
      correlationRes <- RDC.findFavDriversForRider rider.id True
      let favDrivers = map (.driverId) correlationRes
      forM favDrivers $ \favDriver -> do
        person <- B.runInReplica $ QP.findById favDriver >>= fromMaybeM (PersonNotFound favDriver.getId)
        personStat <- B.runInReplica $ QDS.findById favDriver >>= fromMaybeM (InternalError $ "Driver stats not found for driverId : " <> favDriver.getId)
        mkFavouriteDriverResp person personStat
    Nothing -> pure []
  where
    mkFavouriteDriverResp :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => Person.Person -> DriverStats -> m FavouriteDriverResp
    mkFavouriteDriverResp person personStat = do
      let driverName = person.firstName
          id = person.id.getId
          favCount = personStat.favRiderCount
          driverRating = personStat.rating
      driverPhone <- mapM decrypt person.mobileNumber
      pure $ FavouriteDriverResp {..}

removeFavouriteDriver :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => Id Merchant -> Id Person.Person -> Maybe Text -> GetFavouriteDriverInfoReq -> m APISuccess
removeFavouriteDriver merchantId driverId apiKey GetFavouriteDriverInfoReq {..} = do
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  numberHash <- getDbHash customerMobileNumber
  rider <- B.runInReplica $ QRD.findByMobileNumberHashAndMerchant numberHash merchant.id >>= fromMaybeM (InternalError "Rider does not exist")
  isfavourite' <- RDC.findByPrimaryKey driverId rider.id
  whenJust isfavourite' $ \isfavourite -> do
    when (isfavourite.favourite) $ do
      RDC.updateFavouriteDriverForRider False rider.id driverId
      QDS.decFavouriteRiderCount driverId
  pure APISuccess.Success
