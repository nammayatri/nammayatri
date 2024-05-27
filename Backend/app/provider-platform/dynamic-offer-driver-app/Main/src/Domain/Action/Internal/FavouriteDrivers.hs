module Domain.Action.Internal.FavouriteDrivers where

import Domain.Types.DriverStats
import Domain.Types.Merchant (Merchant)
import Domain.Types.Person
import EulerHS.Prelude hiding (id)
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt, getDbHash)
import Kernel.Prelude
import Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Error (AuthError (AuthBlocked), GenericError (InternalError))
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as QM
import Storage.Queries.DriverStats as QDS
import Storage.Queries.Person as QP
import qualified Storage.Queries.RiderDetails as QRD

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
  merchant <- QM.findById merchantId >>= fromMaybeM (InternalError "Merchant not found")
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  numberHash <- getDbHash customerMobileNumber
  rider <- B.runInReplica $ QRD.findByMobileNumberHashAndMerchant numberHash merchant.id >>= fromMaybeM (InternalError "Rider does not exist")
  let favDrivers = rider.favDriverList
  favDriverResponse <- forM favDrivers $ \favDriver -> do
    person <- B.runInReplica $ QP.findById (Id favDriver) >>= fromMaybeM (InternalError "Person not found")
    personStat <- B.runInReplica $ QDS.findById (Id favDriver) >>= fromMaybeM (InternalError "Driver stats not found")
    mkFavouriteDriverResp person personStat
  pure favDriverResponse
  where
    mkFavouriteDriverResp :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => Person -> DriverStats -> m FavouriteDriverResp
    mkFavouriteDriverResp person personStat = do
      let driverName = person.firstName
          id = person.id.getId
          favCount = personStat.favRiderCount
          driverRating = person.rating
      driverPhone <- mapM decrypt person.mobileNumber
      pure $ FavouriteDriverResp {..}

removeFavouriteDriver :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => Id Merchant -> Text -> Maybe Text -> GetFavouriteDriverInfoReq -> m APISuccess
removeFavouriteDriver merchantId driverId apiKey GetFavouriteDriverInfoReq {..} = do
  merchant <- QM.findById merchantId >>= fromMaybeM (InternalError "Merchant not found")
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  numberHash <- getDbHash customerMobileNumber
  rider <- B.runInReplica $ QRD.findByMobileNumberHashAndMerchant numberHash merchant.id >>= fromMaybeM (InternalError "Rider does not exist")
  QRD.removeFavouriteDriver rider.id driverId
  QDS.removeFavouriteRider (Id driverId) (rider.id.getId)
  pure APISuccess.Success
