module API.Beckn.FRFS.OnSelect where

import qualified Beckn.ACL.FRFS.Init as ACLInit
import qualified Beckn.ACL.FRFS.OnSelect as ACL
import qualified Beckn.ACL.FRFS.Utils as Utils
import qualified BecknV2.FRFS.APIs as Spec
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.FRFS.Utils as Utils
import Data.Aeson (eitherDecodeStrict')
import Domain.Action.Beckn.FRFS.Common (DCategorySelect (..))
import qualified Domain.Action.Beckn.FRFS.OnSelect as DOnSelect
import qualified Domain.Types.Extra.IntegratedBPPConfig as DIBC
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import Environment
import EulerHS.Prelude (ByteString)
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import qualified SharedLogic.CallFRFSBPP as CallFRFSBPP
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.BecknConfig as CQBC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.FRFSQuote as QQuote
import qualified Storage.Queries.FRFSQuoteCategory as QQuoteCategory
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.Person as QPerson
import TransactionLogs.PushLogs

type API = Spec.OnSelectAPIBS

handler :: SignatureAuthResult -> FlowServer API
handler = onSelect

onSelect :: SignatureAuthResult -> ByteString -> FlowHandler Spec.AckResponse
onSelect _ reqBS = withFlowHandlerAPI $ do
  req <- case decodeOnSelectReq reqBS of
    Right r -> pure r
    Left err -> throwError (InvalidRequest (toText err))
  transaction_id <- req.onSelectReqContext.contextTransactionId & fromMaybeM (InvalidRequest "TransactionId not found")
  logDebug $ "Received OnSelect request" <> encodeToText req
  withTransactionIdLogTag' transaction_id $ do
    onSelectReq <- ACL.buildOnSelectReq req
    Redis.whenWithLockRedis (onSelectLockKey onSelectReq.messageId) 60 $ do
      (merchant, quote, integratedBppConfig) <- DOnSelect.validateRequest onSelectReq

      -- Check if fare caching is allowed for this provider
      let fareCachingAllowed = case integratedBppConfig.providerConfig of
            DIBC.ONDC ondcCfg -> fromMaybe False (DIBC.fareCachingAllowed ondcCfg)
            _ -> False

      if fareCachingAllowed
        then do
          fork "FRFS on_select call ONDC init" $ do
            whenJust onSelectReq.validTill $ \validity -> void $ QQuote.updateValidTillById quote.id validity
            mbBooking <- QFRFSTicketBooking.findBySearchId (Id transaction_id)
            case mbBooking of
              Just booking -> do
                quoteCategories <- QQuoteCategory.findAllByQuoteId quote.id
                merchantOperatingCity <- CQMOC.findById booking.merchantOperatingCityId >>= fromMaybeM (InternalError "MerchantOperatingCity not found")
                bapConfig <- CQBC.findByMerchantIdDomainVehicleAndMerchantOperatingCityIdWithFallback merchantOperatingCity.id merchant.id (show Spec.FRFS) (Utils.frfsVehicleCategoryToBecknVehicleCategory booking.vehicleType) >>= fromMaybeM (InternalError "Beckn Config not found")
                rider <- QPerson.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
                let mRiderName = rider.firstName <&> (\fName -> rider.lastName & maybe fName (\lName -> fName <> " " <> lName))
                mRiderNumber <- mapM decrypt rider.mobileNumber
                callONDCInit merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) booking quoteCategories integratedBppConfig
              Nothing -> logWarning $ "Booking not found for quote: " <> quote.id.getId <> ". Skipping ONDC init call."
        else do
          fork "FRFS on_select processing" $ do
            Redis.whenWithLockRedis (onSelectProcessingLockKey onSelectReq.messageId) 60 $
              DOnSelect.onSelect onSelectReq merchant quote Nothing Nothing Nothing integratedBppConfig

      fork "FRFS onSelect received pushing ondc logs" do
        void $ pushLogs "on_select" (toJSON req) merchant.id.getId "PUBLIC_TRANSPORT"
  pure Utils.ack
  where
    callONDCInit merchant merchantOperatingCity bapConfig (mRiderName, mRiderNumber) booking quoteCategories integratedBppConfig = do
      providerUrl <- booking.bppSubscriberUrl & parseBaseUrl & fromMaybeM (InvalidRequest "Invalid provider url")
      let categories =
            mapMaybe
              ( \category -> do
                  if category.selectedQuantity > 0
                    then Just $ DCategorySelect {bppItemId = category.bppItemId, quantity = category.selectedQuantity, category = category.category, price = category.offeredPrice}
                    else Nothing
              )
              quoteCategories
      let requestCity = SIBC.resolveOndcCity integratedBppConfig merchantOperatingCity.city
      bknInitReq <- ACLInit.buildInitReq (mRiderName, mRiderNumber) booking bapConfig Utils.BppData {bppId = booking.bppSubscriberId, bppUri = booking.bppSubscriberUrl} requestCity categories
      logDebug $ "FRFS InitReq from OnSelect " <> encodeToText bknInitReq
      logInfo $ "Calling ONDC init from OnSelect for booking: " <> booking.id.getId
      void $ CallFRFSBPP.init providerUrl bknInitReq merchant.id

onSelectLockKey :: Text -> Text
onSelectLockKey id = "FRFS:OnSelect:MessageId-" <> id

onSelectProcessingLockKey :: Text -> Text
onSelectProcessingLockKey id = "FRFS:OnSelect:Processing:MessageId-" <> id

decodeOnSelectReq :: ByteString -> Either String Spec.OnSelectReq
decodeOnSelectReq bs =
  case eitherDecodeStrict' bs of
    Right v -> Right v
    Left _ ->
      case Utils.unescapeQuotedJSON bs of
        Just inner ->
          eitherDecodeStrict' inner
        Nothing ->
          Left "Unable to decode OnSelectReq: invalid JSON format."
