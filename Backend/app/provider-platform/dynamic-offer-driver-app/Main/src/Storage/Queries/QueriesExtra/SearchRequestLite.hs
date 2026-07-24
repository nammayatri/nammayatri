{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.QueriesExtra.SearchRequestLite where

import qualified Data.Text
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.SearchRequest
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Types.Version
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime)
import qualified Kernel.Utils.Version
import qualified Lib.Types.SpecialLocation
import qualified Sequelize as Se
import qualified Storage.Beam.SearchRequest as Beam

findByIdLite :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> m (Maybe SearchRequestLite)
findByIdLite id = findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByTransactionIdLite :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Prelude.Text -> m (Maybe SearchRequestLite)
findByTransactionIdLite transactionId = findOneWithKV [Se.Is Beam.transactionId $ Se.Eq transactionId]

findByTransactionIdAndMerchantIdLite :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> m (Maybe SearchRequestLite)
findByTransactionIdAndMerchantIdLite transactionId providerId = findOneWithKV [Se.And [Se.Is Beam.transactionId $ Se.Eq transactionId, Se.Is Beam.providerId $ Se.Eq (Kernel.Types.Id.getId providerId)]]

data SearchRequestLite = SearchRequestLite
  { area :: Kernel.Prelude.Maybe Lib.Types.SpecialLocation.Area,
    autoAssignEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    createdAt :: Kernel.Prelude.UTCTime,
    driverIdForSearch :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    estimatedDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    estimatedDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    id :: Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest,
    pickupZoneGateId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    providerId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    startTime :: Kernel.Prelude.UTCTime,
    transactionId :: Kernel.Prelude.Text,
    userSdkVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

type SearchRequestLiteTable = Beam.SearchRequestT Identity

instance FromTType' SearchRequestLiteTable SearchRequestLite where
  fromTType' (Beam.SearchRequestT {..}) = do
    now <- getCurrentTime
    userSdkVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> userSdkVersion)
    pure $
      Just
        SearchRequestLite
          { area = area,
            autoAssignEnabled = autoAssignEnabled,
            createdAt = createdAt,
            driverIdForSearch = Kernel.Types.Id.Id <$> driverIdForSearch,
            estimatedDistance = estimatedDistance,
            estimatedDuration = estimatedDuration,
            id = Kernel.Types.Id.Id id,
            pickupZoneGateId = pickupZoneGateId,
            providerId = Kernel.Types.Id.Id providerId,
            startTime = fromMaybe now startTime,
            transactionId = transactionId,
            userSdkVersion = userSdkVersion'
          }
