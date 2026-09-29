module Domain.Action.Internal.BlackListedDrivers where

import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Person as Person
import qualified Domain.Types.RiderDriverCorrelation as RDCD
import EulerHS.Prelude hiding (id)
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption (encrypt, getDbHash)
import Kernel.Prelude hiding (whenJust)
import Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.RiderDetails as QRD
import qualified Storage.Queries.RiderDriverCorrelation as RDC
import Tools.Error

data BlackListDriverReq = BlackListDriverReq
  { customerMobileNumber :: Text,
    customerMobileCountryCode :: Text,
    blackListed :: Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

blackListDriver :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => Id Merchant -> Id Person.Person -> Maybe Text -> BlackListDriverReq -> m APISuccess
blackListDriver merchantId driverId apiKey BlackListDriverReq {..} = do
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
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
