{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.QueriesExtra.SearchRequestLite where

import qualified Data.Text
import qualified Domain.Types.Extra.MerchantPaymentMethod
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.RiderPreferredOption
import qualified Domain.Types.SearchRequest
import Kernel.Beam.Functions
import qualified Kernel.External.Payment.Interface.Types
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Types.Version
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Kernel.Utils.Common
import qualified Kernel.Utils.JSON
import qualified Kernel.Utils.Version
import qualified Lib.Yudhishthira.Types
import qualified Sequelize as Se
import qualified Storage.Beam.SearchRequest as Beam
import qualified Storage.Queries.Transformers.SearchRequest

findByIdLite :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> m (Maybe SearchRequestLite)
findByIdLite id = findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findLastSearchRequestInKVLite :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe SearchRequestLite)
findLastSearchRequestInKVLite (Kernel.Types.Id.Id riderId) = findAllFromKvRedis [Se.Is Beam.riderId $ Se.Eq riderId] (Just $ Se.Desc Beam.createdAt) <&> listToMaybe

data SearchRequestLite = SearchRequestLite
  { allJourneysLoaded :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    autoAssignEnabled :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    autoAssignEnabledV2 :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    backendAppVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    backendConfigVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientBundleVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientConfigVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientDevice :: Kernel.Prelude.Maybe Kernel.Types.Version.Device,
    clientSdkVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    configInExperimentVersions :: [Lib.Yudhishthira.Types.ConfigVersionMap],
    createdAt :: Kernel.Prelude.UTCTime,
    distance :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    estimatedRideDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    id :: Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    riderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    riderPreferredOption :: Domain.Types.RiderPreferredOption.RiderPreferredOption,
    selectedPaymentInstrument :: Kernel.Prelude.Maybe Domain.Types.Extra.MerchantPaymentMethod.PaymentInstrument,
    selectedPaymentMethodId :: Kernel.Prelude.Maybe Kernel.External.Payment.Interface.Types.PaymentMethodId,
    startTime :: Kernel.Prelude.UTCTime,
    validTill :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

type SearchRequestLiteTable = Beam.SearchRequestT Identity

instance FromTType' SearchRequestLiteTable SearchRequestLite where
  fromTType' (Beam.SearchRequestT {..}) = do
    backendConfigVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> backendConfigVersion)
    clientBundleVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientBundleVersion)
    clientConfigVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientConfigVersion)
    clientSdkVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientSdkVersion)
    merchantOperatingCityId' <- Storage.Queries.Transformers.SearchRequest.backfillMOCId merchantId merchantOperatingCityId
    pure $
      Just
        SearchRequestLite
          { allJourneysLoaded = allJourneysLoaded,
            autoAssignEnabled = autoAssignEnabled,
            autoAssignEnabledV2 = autoAssignEnabledV2,
            backendAppVersion = backendAppVersion,
            backendConfigVersion = backendConfigVersion',
            clientBundleVersion = clientBundleVersion',
            clientConfigVersion = clientConfigVersion',
            clientDevice = Kernel.Utils.Version.mkClientDevice clientOsType clientOsVersion clientModelName clientManufacturer,
            clientSdkVersion = clientSdkVersion',
            configInExperimentVersions = fromMaybe [] (Kernel.Utils.JSON.valueToMaybe =<< configInExperimentVersions),
            createdAt = createdAt,
            distance = Kernel.Utils.Common.mkDistanceWithDefault distanceUnit distanceValue . Kernel.Types.Common.HighPrecMeters <$> distance,
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            estimatedRideDuration = estimatedRideDuration,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = merchantOperatingCityId',
            riderId = Kernel.Types.Id.Id riderId,
            riderPreferredOption = fromMaybe Domain.Types.RiderPreferredOption.OneWay riderPreferredOption,
            selectedPaymentInstrument = selectedPaymentInstrument,
            selectedPaymentMethodId = selectedPaymentMethodId,
            startTime = startTime,
            validTill = validTill
          }
