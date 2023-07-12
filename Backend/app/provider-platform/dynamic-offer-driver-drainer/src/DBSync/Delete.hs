module DBSync.Delete where

import Config.Config as Config
import Config.Env
import Data.Either.Extra (mapLeft)
import Data.Text as T
import EulerHS.CachedSqlDBQuery as CDB
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as EL
import EulerHS.Prelude hiding (id)
-- import           Utils.Logging

import System.CPUTime
import Types.DBSync
import Types.Event as Event
import Utils.Utils

runDeleteCommands :: (DeleteDBCommand, ByteString) -> Flow (Either (MeshError, EL.KVDBStreamEntryID) EL.KVDBStreamEntryID)
runDeleteCommands (cmd, val) = do
  case cmd of
    -- DeleteDBCommand id _ _ _ _ (TxnOfferInfoDeleteOptions                  _ whereClause) -> runDelete id val whereClause ("TxnOfferInfo"                  :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (HdfcHashedNumDeleteOptions                 _ whereClause) -> runDelete id val whereClause ("HdfcHashedNum"                 :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (MerchantGatewayAccountDeleteOptions        _ whereClause) -> runDelete id val whereClause ("MerchantGatewayAccount"        :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (MerchantGatewayAccountSubInfoDeleteOptions _ whereClause) -> runDelete id val whereClause ("MerchantGatewayAccountSubInfo" :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (OrderReferenceDeleteOptions                _ whereClause) -> runDelete id val whereClause ("OrderReference"                :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (MandateDeleteOptions                       _ whereClause) -> runDelete id val whereClause ("Mandate"                       :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (GatewayTxnDataDeleteOptions                _ whereClause) -> runDelete id val whereClause ("GatewayTxnData"                :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (MerchantGatewayCardInfoDeleteOptions       _ whereClause) -> runDelete id val whereClause ("MerchantGatewayCardInfo"       :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (TxnRiskCheckDeleteOptions                  _ whereClause) -> runDelete id val whereClause ("TxnRiskCheck"                  :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (PaymentMethodDeleteOptions                 _ whereClause) -> runDelete id val whereClause ("PaymentMethod"                 :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (OfferBenefitInfoDeleteOptions              _ whereClause) -> runDelete id val whereClause ("OfferBenefitInfo"              :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (MerchantIframePreferencesDeleteOptions     _ whereClause) -> runDelete id val whereClause ("MerchantIframePreferences"     :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (ResellerAccountDeleteOptions               _ whereClause) -> runDelete id val whereClause ("ResellerAccount"               :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (UnifiedGatewayResponseDeleteOptions        _ whereClause) -> runDelete id val whereClause ("UnifiedGatewayResponse"        :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (WalletTopUpTxnDeleteOptions                _ whereClause) -> runDelete id val whereClause ("WalletTopUpTxn"                :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (IsinRoutesDeleteOptions                    _ whereClause) -> runDelete id val whereClause ("IsinRoutes"                    :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (TxnCardInfoDeleteOptions                   _ whereClause) -> runDelete id val whereClause ("TxnCardInfo"                   :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (OrderAddressDeleteOptions                  _ whereClause) -> runDelete id val whereClause ("OrderAddress"                  :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (LockerAccountDeleteOptions                 _ whereClause) -> runDelete id val whereClause ("LockerAccount"                 :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (JuspayBankCodeDeleteOptions                _ whereClause) -> runDelete id val whereClause ("JuspayBankCode"                :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (IssuerRoutesDeleteOptions                  _ whereClause) -> runDelete id val whereClause ("IssuerRoutes"                  :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (RefundDeleteOptions                        _ whereClause) -> runDelete id val whereClause ("Refund"                        :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (SecondFactorDeleteOptions                  _ whereClause) -> runDelete id val whereClause ("SecondFactor"                  :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (GatewayOutageDeleteOptions                 _ whereClause) -> runDelete id val whereClause ("GatewayOutage"                 :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (TxnDetailDeleteOptions                     _ whereClause) -> runDelete id val whereClause ("TxnDetail"                     :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (EmiPlanDeleteOptions                       _ whereClause) -> runDelete id val whereClause ("EmiPlan"                       :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (MerchantKeyDeleteOptions                   _ whereClause) -> runDelete id val whereClause ("MerchantKey"                   :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (NetworkCardFingerprintDeleteOptions        _ whereClause) -> runDelete id val whereClause ("NetworkCardFingerprint"        :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (TokenRequestorDeleteOptions                _ whereClause) -> runDelete id val whereClause ("TokenRequestor"                :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (TxnOfferDetailDeleteOptions                _ whereClause) -> runDelete id val whereClause ("TxnOfferDetail"                :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (SecondFactorResponseDeleteOptions          _ whereClause) -> runDelete id val whereClause ("SecondFactorResponse"          :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (CardBrandRoutesDeleteOptions               _ whereClause) -> runDelete id val whereClause ("CardBrandRoutes"               :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (CustomerDeleteOptions                      _ whereClause) -> runDelete id val whereClause ("Customer"                      :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (MerchantAccountDeleteOptions               _ whereClause) -> runDelete id val whereClause ("MerchantAccount"               :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (TokenBinInfoDeleteOptions                  _ whereClause) -> runDelete id val whereClause ("TokenBinInfo"                  :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (MerchantGatewayPaymentMethodDeleteOptions  _ whereClause) -> runDelete id val whereClause ("MerchantGatewayPaymentMethod"  :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (PromotionDeleteOptions                     _ whereClause) -> runDelete id val whereClause ("Promotion"                     :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (LockerTokenRequestorDeleteOptions          _ whereClause) -> runDelete id val whereClause ("LockerTokenRequestor"          :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (BankAccountDeleteOptions                   _ whereClause) -> runDelete id val whereClause ("BankAccount"                   :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (ProviderDeleteOptions                      _ whereClause) -> runDelete id val whereClause ("Provider"                      :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (GatewayCardInfoDeleteOptions               _ whereClause) -> runDelete id val whereClause ("GatewayCardInfo"               :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (PaymentGatewayResponseDeleteOptions        _ whereClause) -> runDelete id val whereClause ("PaymentGatewayResponse"        :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (MetadataDeleteOptions                      _ whereClause) -> runDelete id val whereClause ("Metadata"                      :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (ChargebackDeleteOptions                    _ whereClause) -> runDelete id val whereClause ("Chargeback"                    :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (WalletAccountDeleteOptions                 _ whereClause) -> runDelete id val whereClause ("WalletAccount"                 :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (GatewayStatusMapDeleteOptions              _ whereClause) -> runDelete id val whereClause ("GatewayStatusMap"              :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (TokenDeleteOptions                         _ whereClause) -> runDelete id val whereClause ("Token"                         :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (MerchantLockerAccountDeleteOptions         _ whereClause) -> runDelete id val whereClause ("MerchantLockerAccount"         :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (JuspayEventDeleteOptions                   _ whereClause) -> runDelete id val whereClause ("JuspayEvent"                   :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (TempCardDeleteOptions                      _ whereClause) -> runDelete id val whereClause ("TempCard"                      :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (MerchantRiskSettingsDeleteOptions          _ whereClause) -> runDelete id val whereClause ("MerchantRiskSettings"          :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (UserDeleteOptions                          _ whereClause) -> runDelete id val whereClause ("User"                          :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (CofDetailsDeleteOptions                    _ whereClause) -> runDelete id val whereClause ("CofDetails"                    :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (OrderMetadataV2DeleteOptions               _ whereClause) -> runDelete id val whereClause ("OrderMetadataV2"               :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (StoredCardDeleteOptions                    _ whereClause) -> runDelete id val whereClause ("StoredCard"                    :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (TokenCustomerDeleteOptions                 _ whereClause) -> runDelete id val whereClause ("TokenCustomer"                 :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (EnrolledPanDeleteOptions                   _ whereClause) -> runDelete id val whereClause ("EnrolledPan"                   :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (RoleDeleteOptions                          _ whereClause) -> runDelete id val whereClause ("Role"                          :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (FeatureDeleteOptions                       _ whereClause) -> runDelete id val whereClause ("Feature"                       :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (GatewayBankEmiSupportDeleteOptions         _ whereClause) -> runDelete id val whereClause ("GatewayBankEmiSupport"         :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (AuthenticationAccountDeleteOptions         _ whereClause) -> runDelete id val whereClause ("AuthenticationAccount"         :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (PaymentGatewayResponseV1DeleteOptions      _ whereClause) -> runDelete id val whereClause ("PaymentGatewayResponseV1"      :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (MerchantProviderDetailsDeleteOptions       _ whereClause) -> runDelete id val whereClause ("MerchantProviderDetails"       :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (TxnOfferDeleteOptions                      _ whereClause) -> runDelete id val whereClause ("TxnOffer"                      :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (GatewayHealthDeleteOptions                 _ whereClause) -> runDelete id val whereClause ("GatewayHealth"                 :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (RiskManagementAccountDeleteOptions         _ whereClause) -> runDelete id val whereClause ("RiskManagementAccount"         :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (CardInfoDeleteOptions                      _ whereClause) -> runDelete id val whereClause ("CardInfo"                      :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (DeviceBindingDeleteOptions                 _ whereClause) -> runDelete id val whereClause ("DeviceBinding"                 :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (NotificationDeleteOptions                  _ whereClause) -> runDelete id val whereClause ("Notification"                  :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (OrderBasketDeleteOptions                   _ whereClause) -> runDelete id val whereClause ("OrderBasket"                   :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (GatewayPaymentMethodDeleteOptions          _ whereClause) -> runDelete id val whereClause ("GatewayPaymentMethod"          :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (PaymentLinksDeleteOptions                  _ whereClause) -> runDelete id val whereClause ("PaymentLinks"                  :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (CustomerAccountDeleteOptions               _ whereClause) -> runDelete id val whereClause ("CustomerAccount"               :: Text) =<< Config.getEulerDbConf
    -- DeleteDBCommand id _ _ _ _ (AuthMappingDeleteOptions                   _ whereClause) -> runDelete id val whereClause ("AuthMapping"                   :: Text) =<< Config.getEulerDbConf
    DeleteDBCommand id _ _ _ _ (RegistrationTokenDeleteOptions _ whereClause) -> runDelete id val whereClause ("RegistrationToken" :: Text) =<< Config.getEulerPgDbConf
  where
    -- DeleteDBCommand id _ _ _ _ (OfferRedemptionDeleteOptions               _ whereClause) -> runDelete id val whereClause ("OfferRedemption"               :: Text) =<< Config.getEulerPgDbConf
    -- DeleteDBCommand id _ _ _ _ (ExternalMerchantCustomerDeleteOptions      _ whereClause) -> runDelete id val whereClause ("ExternalMerchantCustomer"      :: Text) =<< Config.getEulerPgDbConf
    -- DeleteDBCommand id _ _ _ _ (AgencyDeleteOptions                        _ whereClause) -> runDelete id val whereClause ("Agency"                        :: Text) =<< Config.getEulerPgDbConf
    -- DeleteDBCommand id _ _ _ _ (ProcessTrackerDeleteOptions                _ whereClause) -> runDelete id val whereClause ("ProcessTracker"                :: Text) =<< Config.getEulerPgDbConf
    -- DeleteDBCommand id _ _ _ _ (OffersDeleteOptions                        _ whereClause) -> runDelete id val whereClause ("Offers"                        :: Text) =<< Config.getEulerPgDbConf
    -- DeleteDBCommand id _ _ _ _ (SavedPaymentMethodDeleteOptions            _ whereClause) -> runDelete id val whereClause ("SavedPaymentMethod"            :: Text) =<< Config.getEulerPgDbConf

    runDelete id value whereClause model dbConf = do
      maxRetries <- EL.runIO getMaxRetries
      runDeleteWithRetries id value whereClause model dbConf 0 maxRetries

    runDeleteWithRetries id value whereClause model dbConf retryIndex maxRetries = do
      --   t1    <- EL.getCurrentDateInMillis
      --   cpuT1 <- EL.runIO getCPUTime
      res <- mapLeft MDBError <$> CDB.deleteAllReturning dbConf whereClause
      --   t2    <- EL.getCurrentDateInMillis
      --   cpuT2 <- EL.runIO getCPUTime
      case (res, retryIndex) of
        (Left _, y) | y < maxRetries -> do
          void $ publishDBSyncMetric $ Event.QueryExecutionFailure "Delete" model
          EL.runIO $ delay =<< getRetryDelay
          runDeleteWithRetries id value whereClause model dbConf (retryIndex + 1) maxRetries
        (Left x, _) -> do
          void $ publishDBSyncMetric $ Event.QueryExecutionFailure "Delete" model
          EL.logError (("Delete failed: " :: Text) <> T.pack (show x)) (show [("command" :: String, value)] :: Text)
          pure $ Left (x, id)
        (Right _, _) -> do
          --   EL.logInfoV ("Drainer Info" :: Text) $ createDBLogEntry model "DELETE" (t2 -t1) (cpuT2 - cpuT1) rVals
          pure $ Right id
