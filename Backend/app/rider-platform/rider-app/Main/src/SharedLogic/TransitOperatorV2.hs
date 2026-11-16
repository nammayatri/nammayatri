module SharedLogic.TransitOperatorV2
  ( TransitCtx (..),
    TransitFeedCtx (..),
    transitCtx,
    transitFeedCtx,
    transitGims,
    tms,
    tmsUnit,
    tmsConfig,
    requireRequestorId,
    findMerchantOpCity,
    findDashboardPerson,
    hashSecret,
  )
where

import qualified BecknV2.OnDemand.Enums as BecknSpec
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import qualified Data.Text.Encoding as TE
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified "lib-dashboard" Domain.Types.Person as DDashboardPerson
import Environment (Flow)
import qualified Environment as Fields
import qualified EulerHS.Types as ET
import Kernel.Beam.Functions (runInDashboardDb)
import Kernel.External.Encryption (DbHash (..), getDbHash)
import Kernel.Prelude
import qualified Kernel.Types.APISuccess as APISuccess
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Monitoring.Prometheus.Servant (SanitizedUrl)
import Lib.BmsBackend.Transit.Common (TmsCaller (..), callTms, scoped)
import Lib.BmsBackend.Types (BmsBackendConfig)
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Servant (NoContent)
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import "lib-dashboard" Storage.Beam.SchemaInstances ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))
import qualified "lib-dashboard" Storage.Queries.Person as QDashboardPerson
import qualified "lib-dashboard" Tools.Auth.Capability as DashboardCapability
import qualified Tools.MultiModal as MM
import TransactionLogs.ONDC.Flow ()

data TransitCtx = TransitCtx
  { cfg :: BmsBackendConfig,
    gtfsId :: Text,
    caller :: TmsCaller
  }

transitFeed :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Maybe Text -> Flow (DMOC.MerchantOperatingCity, DIBC.IntegratedBPPConfig, DDashboardPerson.Person)
transitFeed merchantShortId city vehicleCategory mbRequestorId = do
  requestorId <- requireRequestorId mbRequestorId
  merchantOpCity <- findMerchantOpCity merchantShortId city
  bppConfig <- SIBC.findBusIntegratedBPPConfig merchantOpCity.id vehicleCategory DIBC.MULTIMODAL
  person <- findDashboardPerson requestorId
  pure (merchantOpCity, bppConfig, person)

data TransitFeedCtx = TransitFeedCtx
  { ctx :: TransitCtx,
    bppConfig :: DIBC.IntegratedBPPConfig,
    gimsUrl :: BaseUrl
  }

transitCtx :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Maybe Text -> Flow TransitCtx
transitCtx merchantShortId city vehicleCategory mbRequestorId = do
  (merchantOpCity, bppConfig, person) <- transitFeed merchantShortId city vehicleCategory mbRequestorId
  mkTransitCtx merchantOpCity bppConfig person

transitFeedCtx :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Maybe Text -> Flow TransitFeedCtx
transitFeedCtx merchantShortId city vehicleCategory mbRequestorId = do
  (merchantOpCity, bppConfig, person) <- transitFeed merchantShortId city vehicleCategory mbRequestorId
  ctx <- mkTransitCtx merchantOpCity bppConfig person
  gimsUrl <- MM.getOTPRestServiceReq bppConfig.merchantId merchantOpCity.id
  pure TransitFeedCtx {ctx, bppConfig, gimsUrl}

mkTransitCtx :: DMOC.MerchantOperatingCity -> DIBC.IntegratedBPPConfig -> DDashboardPerson.Person -> Flow TransitCtx
mkTransitCtx merchantOpCity bppConfig person = do
  cfg <- tmsConfig bppConfig.merchantId merchantOpCity.id
  orgId <- callerOrgId person
  pure
    TransitCtx
      { cfg,
        gtfsId = bppConfig.feedKey,
        caller = TmsCaller {orgId, actorPersonId = person.id.getId}
      }

transitGims :: ShortId Merchant -> Context.City -> BecknSpec.VehicleCategory -> Maybe Text -> Flow (BaseUrl, Text)
transitGims merchantShortId city vehicleCategory mbRequestorId = do
  (merchantOpCity, bppConfig, _) <- transitFeed merchantShortId city vehicleCategory mbRequestorId
  baseUrl <- MM.getOTPRestServiceReq bppConfig.merchantId merchantOpCity.id
  pure (baseUrl, bppConfig.feedKey)

callerOrgId :: DDashboardPerson.Person -> Flow Text
callerOrgId person = case person.organizationId of
  Just orgId -> pure orgId
  Nothing -> do
    isAdmin <- runInDashboardDb $ (||) <$> DashboardCapability.isSuperAdmin person.id <*> (elem transitOrgAdmin <$> DashboardCapability.computeEffective person.id person.roleId)
    unless isAdmin $ throwError AccessDenied
    pure "*"

transitOrgAdmin :: Text
transitOrgAdmin = "transit-operations.org.admin"

requireRequestorId :: Maybe Text -> Flow Text
requireRequestorId = fromMaybeM (InvalidRequest "requestorId is required")

findMerchantOpCity :: ShortId Merchant -> Context.City -> Flow DMOC.MerchantOperatingCity
findMerchantOpCity merchantShortId city =
  CQMOC.findByMerchantShortIdAndCity merchantShortId city
    >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show city)

findDashboardPerson :: Text -> Flow DDashboardPerson.Person
findDashboardPerson personId =
  runInDashboardDb (QDashboardPerson.findById (Id personId))
    >>= fromMaybeM (PersonDoesNotExist personId)

tmsConfig :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow BmsBackendConfig
tmsConfig merchantId merchantOpCityId = do
  merchantServiceConfig <-
    getOneConfig (MerchantServiceConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, merchantId = merchantId.getId, serviceName = Just (DMSC.BmsService DMSC.Tms)}) Nothing
      >>= fromMaybeM (MerchantServiceConfigNotFound merchantOpCityId.getId "Bms" (show DMSC.Tms))
  case merchantServiceConfig.serviceConfig of
    DMSC.BmsServiceConfig cfg -> pure cfg
    _ -> throwError $ InternalError "Unknown Service Config"

tms :: (SanitizedUrl api, ToJSON a) => TransitCtx -> Text -> Proxy api -> (Maybe Text -> Maybe Text -> ET.EulerClient a) -> Flow a
tms ctx name proxy client = callTms ctx.cfg name proxy (scoped ctx.caller client)

tmsUnit :: SanitizedUrl api => TransitCtx -> Text -> Proxy api -> (Maybe Text -> Maybe Text -> ET.EulerClient NoContent) -> Flow APISuccess.APISuccess
tmsUnit ctx name proxy client = APISuccess.Success <$ tms ctx name proxy client

hashSecret :: Maybe Text -> Flow (Maybe Text)
hashSecret = traverse (fmap (TE.decodeUtf8 . convertToBase Base16 . unDbHash) . getDbHash)
