module Types.DBSync.Update where

import Data.Aeson as A
import Data.Aeson.Types (Parser)
import qualified Data.Text as T
import Database.Beam.Postgres (Postgres)
import EulerHS.Prelude
import Sequelize
import qualified "rider-app" Storage.Beam.AppInstalls as AppInstalls
import qualified "rider-app" Storage.Beam.BlackListOrg as BlackListOrg
import qualified "rider-app" Storage.Beam.Booking as Booking
import qualified "rider-app" Storage.Beam.Booking.BookingLocation as BookingLocation
import qualified "rider-app" Storage.Beam.BookingCancellationReason as BookingCancellationReason
import qualified "rider-app" Storage.Beam.CallStatus as CallStatus
import qualified "rider-app" Storage.Beam.CallbackRequest as CallbackRequest
import qualified "rider-app" Storage.Beam.CancellationReason as CancellationReason
import qualified "rider-app" Storage.Beam.DriverOffer as DriverOffer
import qualified "rider-app" Storage.Beam.Estimate as Estimate
import qualified "rider-app" Storage.Beam.EstimateBreakup as EstimateBreakup
import qualified "rider-app" Storage.Beam.Exophone as Exophone
import qualified "rider-app" Storage.Beam.FarePolicy.FareBreakup as FareBreakup
import qualified "rider-app" Storage.Beam.Geometry as Geometry
import qualified "rider-app" Storage.Beam.Issue as Issue
import qualified "rider-app" Storage.Beam.Maps.PlaceNameCache as PlaceNameCache
import qualified "rider-app" Storage.Beam.Merchant as Merchant
import qualified "rider-app" Storage.Beam.Merchant.MerchantMessage as MerchantMessage
import qualified "rider-app" Storage.Beam.Merchant.MerchantPaymentMethod as MerchantPaymentMethod
import qualified "rider-app" Storage.Beam.Merchant.MerchantServiceConfig as MerchantServiceConfig
import qualified "rider-app" Storage.Beam.Merchant.MerchantServiceUsageConfig as MerchantServiceUsageConfig
import qualified "rider-app" Storage.Beam.MerchantConfig as MerchantConfig
import qualified "rider-app" Storage.Beam.OnSearchEvent as OnSearchEvent
import qualified "rider-app" Storage.Beam.Payment.PaymentOrder as PaymentOrder
import qualified "rider-app" Storage.Beam.Payment.PaymentTransaction as PaymentTransaction
import qualified "rider-app" Storage.Beam.Person as Person
import qualified "rider-app" Storage.Beam.Person.PersonDefaultEmergencyNumber as PersonDefaultEmergencyNumber
import qualified "rider-app" Storage.Beam.Person.PersonFlowStatus as PersonFlowStatus
import qualified "rider-app" Storage.Beam.Quote as Quote
import qualified "rider-app" Storage.Beam.RegistrationToken as RegistrationToken
import qualified "rider-app" Storage.Beam.RentalSlab as RentalSlab
import qualified "rider-app" Storage.Beam.Ride as Ride
import qualified "rider-app" Storage.Beam.SavedReqLocation as SavedReqLocation
import qualified "rider-app" Storage.Beam.SearchRequest as SearchRequest
import qualified "rider-app" Storage.Beam.SearchRequest.SearchReqLocation as SearchReqLocation
import qualified "rider-app" Storage.Beam.Sos as Sos
import qualified "rider-app" Storage.Beam.SpecialZoneQuote as SpecialZoneQuote
import qualified "rider-app" Storage.Beam.TripTerms as TripTerms
import qualified "rider-app" Storage.Beam.Webengage as Webengage
import Utils.Parse

-- Each update option contains a list of (key, value) pairs to set during
-- the update and a list of (key, value) pairs to use as a where clause.
-- data UpdateModel
--   = TxnOfferInfoUpdate
--   | JuspayEventUpdate
--   | HdfcHashedNumUpdate
--   | MerchantGatewayAccountUpdate
--   | RuleUpdate
--   | MerchantGatewayAccountSubInfoUpdate
--   | OrderReferenceUpdate
--   | MandateUpdate
--   | GatewayTxnDataUpdate
--   | MerchantGatewayCardInfoUpdate
--   | TxnRiskCheckUpdate
--   | PaymentMethodUpdate
--   | OfferBenefitInfoUpdate
--   | MerchantIframePreferencesUpdate
--   | ResellerAccountUpdate
--   | UnifiedGatewayResponseUpdate
--   | WalletTopUpTxnUpdate
--   | IsinRoutesUpdate
--   | TxnCardInfoUpdate
--   | OrderAddressUpdate
--   | LockerAccountUpdate
--   | JuspayBankCodeUpdate
--   | IssuerRoutesUpdate
--   | RefundUpdate
--   | SecondFactorUpdate
--   | GatewayOutageUpdate
--   | TxnDetailUpdate
--   | EmiPlanUpdate
--   | MerchantKeyUpdate
--   | NetworkCardFingerprintUpdate
--   | TokenRequestorUpdate
--   | TxnOfferDetailUpdate
--   | SecondFactorResponseUpdate
--   | OfferRedemptionUpdate
--   | CardBrandRoutesUpdate
--   | CustomerUpdate
--   | MerchantAccountUpdate
--   | ExternalMerchantCustomerUpdate
--   | TokenBinInfoUpdate
--   | MerchantGatewayPaymentMethodUpdate
--   | PromotionUpdate
--   | LockerTokenRequestorUpdate
--   | BankAccountUpdate
--   | AgencyUpdate
--   | ProviderUpdate
--   | GatewayCardInfoUpdate
--   | PaymentGatewayResponseUpdate
--   | MetadataUpdate
--   | ChargebackUpdate
--   | WalletAccountUpdate
--   | GatewayStatusMapUpdate
--   | TokenUpdate
--   | MerchantLockerAccountUpdate
--   | TempCardUpdate
--   | MerchantRiskSettingsUpdate
--   | UserUpdate
--   | CofDetailsUpdate
--   | OrderMetadataV2Update
--   | StoredCardUpdate
--   | TokenCustomerUpdate
--   | EnrolledPanUpdate
--   | OffersUpdate
--   | RoleUpdate
--   | FeatureUpdate
--   | GatewayBankEmiSupportUpdate
--   | AuthenticationAccountUpdate
--   | PaymentGatewayResponseV1Update
--   | SavedPaymentMethodUpdate
--   | MerchantProviderDetailsUpdate
--   | TxnOfferUpdate
--   | GatewayHealthUpdate
--   | RiskManagementAccountUpdate
--   | CardInfoUpdate
--   | DeviceBindingUpdate
--   | NotificationUpdate
--   | OrderBasketUpdate
--   | GatewayPaymentMethodUpdate
--   | ProcessTrackerUpdate
--   | PaymentLinksUpdate
--   | CustomerAccountUpdate
--   | EntityMapUpdate
--   | FormInputUpdate
--   | IngressRuleUpdate
--   | InstallmentUpdate
--   | InstallmentRefundUpdate
--   | PaymentFormUpdate
--   | UserRoleUpdate
--   | TxnIntentDetailUpdate
--   | AuthMappingUpdate
--   deriving (Generic, Show)

data UpdateModel
  = AppInstallsUpdate
  | BlackListOrgUpdate
  | BookingUpdate
  | BookingLocationUpdate
  | BookingCancellationReasonUpdate
  | CallbackRequestUpdate
  | CallStatusUpdate
  | CancellationReasonUpdate
  | DriverOfferUpdate
  | EstimateUpdate
  | EstimateBreakupUpdate
  | ExophoneUpdate
  | FareBreakupUpdate
  | GeometryUpdate
  | IssueUpdate
  | PlaceNameCacheUpdate
  | MerchantUpdate
  | MerchantMessageUpdate
  | MerchantPaymentMethodUpdate
  | MerchantServiceConfigUpdate
  | MerchantServiceUsageConfigUpdate
  | MerchantConfigUpdate
  | OnSearchEventUpdate
  | PaymentOrderUpdate
  | PaymentTransactionUpdate
  | PersonUpdate
  | PersonDefaultEmergencyNumberUpdate
  | PersonFlowStatusUpdate
  | QuoteUpdate
  | RegistrationTokenUpdate
  | RentalSlabUpdate
  | RideUpdate
  | SavedReqLocationUpdate
  | SearchRequestUpdate
  | SearchReqLocationUpdate
  | SosUpdate
  | SpecialZoneQuoteUpdate
  | TripTermsUpdate
  | WebengageUpdate
  deriving (Generic, Show)

-- getTagUpdate :: UpdateModel -> Text
-- getTagUpdate TxnOfferInfoUpdate = "TxnOfferInfoOptions"
-- getTagUpdate JuspayEventUpdate = "JuspayEventOptions"
-- getTagUpdate HdfcHashedNumUpdate = "HdfcHashedNumOptions"
-- getTagUpdate MerchantGatewayAccountUpdate = "MerchantGatewayAccountOptions"
-- getTagUpdate RuleUpdate = "RuleOptions"
-- getTagUpdate MerchantGatewayAccountSubInfoUpdate = "MerchantGatewayAccountSubInfoOptions"
-- getTagUpdate OrderReferenceUpdate = "OrderReferenceOptions"
-- getTagUpdate MandateUpdate = "MandateOptions"
-- getTagUpdate GatewayTxnDataUpdate = "GatewayTxnDataOptions"
-- getTagUpdate MerchantGatewayCardInfoUpdate = "MerchantGatewayCardInfoOptions"
-- getTagUpdate TxnRiskCheckUpdate = "TxnRiskCheckOptions"
-- getTagUpdate PaymentMethodUpdate = "PaymentMethodOptions"
-- getTagUpdate OfferBenefitInfoUpdate = "OfferBenefitInfoOptions"
-- getTagUpdate MerchantIframePreferencesUpdate = "MerchantIframePreferencesOptions"
-- getTagUpdate ResellerAccountUpdate = "ResellerAccountOptions"
-- getTagUpdate UnifiedGatewayResponseUpdate = "UnifiedGatewayResponseOptions"
-- getTagUpdate WalletTopUpTxnUpdate = "WalletTopUpTxnOptions"
-- getTagUpdate IsinRoutesUpdate = "IsinRoutesOptions"
-- getTagUpdate TxnCardInfoUpdate = "TxnCardInfoOptions"
-- getTagUpdate OrderAddressUpdate = "OrderAddressOptions"
-- getTagUpdate LockerAccountUpdate = "LockerAccountOptions"
-- getTagUpdate JuspayBankCodeUpdate = "JuspayBankCodeOptions"
-- getTagUpdate IssuerRoutesUpdate = "IssuerRoutesOptions"
-- getTagUpdate RefundUpdate = "RefundOptions"
-- getTagUpdate SecondFactorUpdate = "SecondFactorOptions"
-- getTagUpdate GatewayOutageUpdate = "GatewayOutageOptions"
-- getTagUpdate TxnDetailUpdate = "TxnDetailOptions"
-- getTagUpdate EmiPlanUpdate = "EmiPlanOptions"
-- getTagUpdate MerchantKeyUpdate = "MerchantKeyOptions"
-- getTagUpdate NetworkCardFingerprintUpdate = "NetworkCardFingerprintOptions"
-- getTagUpdate TokenRequestorUpdate = "TokenRequestorOptions"
-- getTagUpdate TxnOfferDetailUpdate = "TxnOfferDetailOptions"
-- getTagUpdate SecondFactorResponseUpdate = "SecondFactorResponseOptions"
-- getTagUpdate OfferRedemptionUpdate = "OfferRedemptionOptions"
-- getTagUpdate CardBrandRoutesUpdate = "CardBrandRoutesOptions"
-- getTagUpdate CustomerUpdate = "CustomerOptions"
-- getTagUpdate MerchantAccountUpdate = "MerchantAccountOptions"
-- getTagUpdate ExternalMerchantCustomerUpdate = "ExternalMerchantCustomerOptions"
-- getTagUpdate TokenBinInfoUpdate = "TokenBinInfoOptions"
-- getTagUpdate MerchantGatewayPaymentMethodUpdate = "MerchantGatewayPaymentMethodOptions"
-- getTagUpdate PromotionUpdate = "PromotionOptions"
-- getTagUpdate LockerTokenRequestorUpdate = "LockerTokenRequestorOptions"
-- getTagUpdate BankAccountUpdate = "BankAccountOptions"
-- getTagUpdate AgencyUpdate = "AgencyOptions"
-- getTagUpdate ProviderUpdate = "ProviderOptions"
-- getTagUpdate GatewayCardInfoUpdate = "GatewayCardInfoOptions"
-- getTagUpdate PaymentGatewayResponseUpdate = "PaymentGatewayResponseOptions"
-- getTagUpdate MetadataUpdate = "MetadataOptions"
-- getTagUpdate ChargebackUpdate = "ChargebackOptions"
-- getTagUpdate WalletAccountUpdate = "WalletAccountOptions"
-- getTagUpdate GatewayStatusMapUpdate = "GatewayStatusMapOptions"
-- getTagUpdate TokenUpdate = "TokenOptions"
-- getTagUpdate MerchantLockerAccountUpdate = "MerchantLockerAccountOptions"
-- getTagUpdate TempCardUpdate = "TempCardOptions"
-- getTagUpdate MerchantRiskSettingsUpdate = "MerchantRiskSettingsOptions"
-- getTagUpdate UserUpdate = "UserOptions"
-- getTagUpdate CofDetailsUpdate = "CofDetailsOptions"
-- getTagUpdate OrderMetadataV2Update = "OrderMetadataV2Options"
-- getTagUpdate StoredCardUpdate = "StoredCardOptions"
-- getTagUpdate TokenCustomerUpdate = "TokenCustomerOptions"
-- getTagUpdate EnrolledPanUpdate = "EnrolledPanOptions"
-- getTagUpdate OffersUpdate = "OffersOptions"
-- getTagUpdate RoleUpdate = "RoleOptions"
-- getTagUpdate FeatureUpdate = "FeatureOptions"
-- getTagUpdate GatewayBankEmiSupportUpdate = "GatewayBankEmiSupportOptions"
-- getTagUpdate AuthenticationAccountUpdate = "AuthenticationAccountOptions"
-- getTagUpdate PaymentGatewayResponseV1Update = "PaymentGatewayResponseV1Options"
-- getTagUpdate SavedPaymentMethodUpdate = "SavedPaymentMethodOptions"
-- getTagUpdate MerchantProviderDetailsUpdate = "MerchantProviderDetailsOptions"
-- getTagUpdate TxnOfferUpdate = "TxnOfferOptions"
-- getTagUpdate GatewayHealthUpdate = "GatewayHealthOptions"
-- getTagUpdate RiskManagementAccountUpdate = "RiskManagementAccountOptions"
-- getTagUpdate CardInfoUpdate = "CardInfoOptions"
-- getTagUpdate DeviceBindingUpdate = "DeviceBindingOptions"
-- getTagUpdate NotificationUpdate = "NotificationOptions"
-- getTagUpdate OrderBasketUpdate = "OrderBasketOptions"
-- getTagUpdate GatewayPaymentMethodUpdate = "GatewayPaymentMethodOptions"
-- getTagUpdate ProcessTrackerUpdate = "ProcessTrackerOptions"
-- getTagUpdate PaymentLinksUpdate = "PaymentLinksOptions"
-- getTagUpdate CustomerAccountUpdate = "CustomerAccountOptions"
-- getTagUpdate EntityMapUpdate = "EntityMapOptions"
-- getTagUpdate FormInputUpdate = "FormInputOptions"
-- getTagUpdate IngressRuleUpdate = "IngressRuleOptions"
-- getTagUpdate InstallmentUpdate = "InstallmentOptions"
-- getTagUpdate InstallmentRefundUpdate = "InstallmentRefundOptions"
-- getTagUpdate PaymentFormUpdate = "PaymentFormOptions"
-- getTagUpdate UserRoleUpdate = "UserRoleOptions"
-- getTagUpdate TxnIntentDetailUpdate = "TxnIntentDetailOptions"
-- getTagUpdate AuthMappingUpdate = "AuthMappingOptions"

getTagUpdate :: UpdateModel -> Text
getTagUpdate AppInstallsUpdate = "AppInstallsOptions"
getTagUpdate BlackListOrgUpdate = "BlackListOrgOptions"
getTagUpdate BookingUpdate = "BookingOptions"
getTagUpdate BookingLocationUpdate = "BookingLocationOptions"
getTagUpdate BookingCancellationReasonUpdate = "BookingCancellationReasonOptions"
getTagUpdate CallbackRequestUpdate = "CallbackRequestOptions"
getTagUpdate CallStatusUpdate = "CallStatusOptions"
getTagUpdate CancellationReasonUpdate = "CancellationReasonOptions"
getTagUpdate DriverOfferUpdate = "DriverOfferOptions"
getTagUpdate EstimateUpdate = "EstimateOptions"
getTagUpdate EstimateBreakupUpdate = "EstimateBreakupOptions"
getTagUpdate ExophoneUpdate = "ExophoneOptions"
getTagUpdate FareBreakupUpdate = "FareBreakupOptions"
getTagUpdate GeometryUpdate = "GeometryOptions"
getTagUpdate IssueUpdate = "IssueOptions"
getTagUpdate PlaceNameCacheUpdate = "PlaceNameCacheOptions"
getTagUpdate MerchantUpdate = "MerchantOptions"
getTagUpdate MerchantMessageUpdate = "MerchantMessageOptions"
getTagUpdate MerchantPaymentMethodUpdate = "MerchantPaymentMethodOptions"
getTagUpdate MerchantServiceConfigUpdate = "MerchantServiceConfigOptions"
getTagUpdate MerchantServiceUsageConfigUpdate = "MerchantServiceUsageConfigOptions"
getTagUpdate MerchantConfigUpdate = "MerchantConfigOptions"
getTagUpdate OnSearchEventUpdate = "OnSearchEventOptions"
getTagUpdate PaymentOrderUpdate = "PaymentOrderOptions"
getTagUpdate PaymentTransactionUpdate = "PaymentTransactionOptions"
getTagUpdate PersonUpdate = "PersonOptions"
getTagUpdate PersonDefaultEmergencyNumberUpdate = "PersonDefaultEmergencyNumberOptions"
getTagUpdate PersonFlowStatusUpdate = "PersonFlowStatusOptions"
getTagUpdate QuoteUpdate = "QuoteOptions"
getTagUpdate RegistrationTokenUpdate = "RegistrationTokenOptions"
getTagUpdate RentalSlabUpdate = "RentalSlabOptions"
getTagUpdate RideUpdate = "RideOptions"
getTagUpdate SavedReqLocationUpdate = "SavedReqLocationOptions"
getTagUpdate SearchRequestUpdate = "SearchRequestOptions"
getTagUpdate SearchReqLocationUpdate = "SearchReqLocationOptions"
getTagUpdate SosUpdate = "SosOptions"
getTagUpdate SpecialZoneQuoteUpdate = "SpecialZoneQuoteOptions"
getTagUpdate TripTermsUpdate = "TripTermsOptions"
getTagUpdate WebengageUpdate = "WebengageOptions"

parseTagUpdate :: Text -> Parser UpdateModel
parseTagUpdate "AppInstallsOptions" = return AppInstallsUpdate
parseTagUpdate "BlackListOrgOptions" = return BlackListOrgUpdate
parseTagUpdate "BookingOptions" = return BookingUpdate
parseTagUpdate "BookingLocationOptions" = return BookingLocationUpdate
parseTagUpdate "BookingCancellationReasonOptions" = return BookingCancellationReasonUpdate
parseTagUpdate "CallbackRequestOptions" = return CallbackRequestUpdate
parseTagUpdate "CallStatusOptions" = return CallStatusUpdate
parseTagUpdate "CancellationReasonOptions" = return CancellationReasonUpdate
parseTagUpdate "DriverOfferOptions" = return DriverOfferUpdate
parseTagUpdate "EstimateOptions" = return EstimateUpdate
parseTagUpdate "EstimateBreakupOptions" = return EstimateBreakupUpdate
parseTagUpdate "ExophoneOptions" = return ExophoneUpdate
parseTagUpdate "FareBreakupOptions" = return FareBreakupUpdate
parseTagUpdate "GeometryOptions" = return GeometryUpdate
parseTagUpdate "IssueOptions" = return IssueUpdate
parseTagUpdate "PlaceNameCacheOptions" = return PlaceNameCacheUpdate
parseTagUpdate "MerchantOptions" = return MerchantUpdate
parseTagUpdate "MerchantMessageOptions" = return MerchantMessageUpdate
parseTagUpdate "MerchantPaymentMethodOptions" = return MerchantPaymentMethodUpdate
parseTagUpdate "MerchantServiceConfigOptions" = return MerchantServiceConfigUpdate
parseTagUpdate "MerchantServiceUsageConfigOptions" = return MerchantServiceUsageConfigUpdate
parseTagUpdate "MerchantConfigOptions" = return MerchantConfigUpdate
parseTagUpdate "OnSearchEventOptions" = return OnSearchEventUpdate
parseTagUpdate "PaymentOrderOptions" = return PaymentOrderUpdate
parseTagUpdate "PaymentTransactionOptions" = return PaymentTransactionUpdate
parseTagUpdate "PersonOptions" = return PersonUpdate
parseTagUpdate "PersonDefaultEmergencyNumberOptions" = return PersonDefaultEmergencyNumberUpdate
parseTagUpdate "PersonFlowStatusOptions" = return PersonFlowStatusUpdate
parseTagUpdate "QuoteOptions" = return QuoteUpdate
parseTagUpdate "RegistrationTokenOptions" = return RegistrationTokenUpdate
parseTagUpdate "RentalSlabOptions" = return RentalSlabUpdate
parseTagUpdate "RideOptions" = return RideUpdate
parseTagUpdate "SavedReqLocationOptions" = return SavedReqLocationUpdate
parseTagUpdate "SearchRequestOptions" = return SearchRequestUpdate
parseTagUpdate "SearchReqLocationOptions" = return SearchReqLocationUpdate
parseTagUpdate "SosOptions" = return SosUpdate
parseTagUpdate "SpecialZoneQuoteOptions" = return SpecialZoneQuoteUpdate
parseTagUpdate "TripTermsOptions" = return TripTermsUpdate
parseTagUpdate "WebengageOptions" = return WebengageUpdate
parseTagUpdate t = fail $ T.unpack ("Expected a UpdateModel but got '" <> t <> "'")

-- parseTagUpdate :: Text -> Parser UpdateModel
-- parseTagUpdate "TxnOfferInfoOptions" = return TxnOfferInfoUpdate
-- parseTagUpdate "JuspayEventOptions" = return JuspayEventUpdate
-- parseTagUpdate "HdfcHashedNumOptions" = return HdfcHashedNumUpdate
-- parseTagUpdate "MerchantGatewayAccountOptions" = return MerchantGatewayAccountUpdate
-- parseTagUpdate "RuleOptions" = return RuleUpdate
-- parseTagUpdate "MerchantGatewayAccountSubInfoOptions" = return MerchantGatewayAccountSubInfoUpdate
-- parseTagUpdate "OrderReferenceOptions" = return OrderReferenceUpdate
-- parseTagUpdate "MandateOptions" = return MandateUpdate
-- parseTagUpdate "GatewayTxnDataOptions" = return GatewayTxnDataUpdate
-- parseTagUpdate "MerchantGatewayCardInfoOptions" = return MerchantGatewayCardInfoUpdate
-- parseTagUpdate "TxnRiskCheckOptions" = return TxnRiskCheckUpdate
-- parseTagUpdate "PaymentMethodOptions" = return PaymentMethodUpdate
-- parseTagUpdate "OfferBenefitInfoOptions" = return OfferBenefitInfoUpdate
-- parseTagUpdate "MerchantIframePreferencesOptions" = return MerchantIframePreferencesUpdate
-- parseTagUpdate "ResellerAccountOptions" = return ResellerAccountUpdate
-- parseTagUpdate "UnifiedGatewayResponseOptions" = return UnifiedGatewayResponseUpdate
-- parseTagUpdate "WalletTopUpTxnOptions" = return WalletTopUpTxnUpdate
-- parseTagUpdate "IsinRoutesOptions" = return IsinRoutesUpdate
-- parseTagUpdate "TxnCardInfoOptions" = return TxnCardInfoUpdate
-- parseTagUpdate "OrderAddressOptions" = return OrderAddressUpdate
-- parseTagUpdate "LockerAccountOptions" = return LockerAccountUpdate
-- parseTagUpdate "JuspayBankCodeOptions" = return JuspayBankCodeUpdate
-- parseTagUpdate "IssuerRoutesOptions" = return IssuerRoutesUpdate
-- parseTagUpdate "RefundOptions" = return RefundUpdate
-- parseTagUpdate "SecondFactorOptions" = return SecondFactorUpdate
-- parseTagUpdate "GatewayOutageOptions" = return GatewayOutageUpdate
-- parseTagUpdate "TxnDetailOptions" = return TxnDetailUpdate
-- parseTagUpdate "EmiPlanOptions" = return EmiPlanUpdate
-- parseTagUpdate "MerchantKeyOptions" = return MerchantKeyUpdate
-- parseTagUpdate "NetworkCardFingerprintOptions" = return NetworkCardFingerprintUpdate
-- parseTagUpdate "TokenRequestorOptions" = return TokenRequestorUpdate
-- parseTagUpdate "TxnOfferDetailOptions" = return TxnOfferDetailUpdate
-- parseTagUpdate "SecondFactorResponseOptions" = return SecondFactorResponseUpdate
-- parseTagUpdate "OfferRedemptionOptions" = return OfferRedemptionUpdate
-- parseTagUpdate "CardBrandRoutesOptions" = return CardBrandRoutesUpdate
-- parseTagUpdate "CustomerOptions" = return CustomerUpdate
-- parseTagUpdate "MerchantAccountOptions" = return MerchantAccountUpdate
-- parseTagUpdate "ExternalMerchantCustomerOptions" = return ExternalMerchantCustomerUpdate
-- parseTagUpdate "TokenBinInfoOptions" = return TokenBinInfoUpdate
-- parseTagUpdate "MerchantGatewayPaymentMethodOptions" = return MerchantGatewayPaymentMethodUpdate
-- parseTagUpdate "PromotionOptions" = return PromotionUpdate
-- parseTagUpdate "LockerTokenRequestorOptions" = return LockerTokenRequestorUpdate
-- parseTagUpdate "BankAccountOptions" = return BankAccountUpdate
-- parseTagUpdate "AgencyOptions" = return AgencyUpdate
-- parseTagUpdate "ProviderOptions" = return ProviderUpdate
-- parseTagUpdate "GatewayCardInfoOptions" = return GatewayCardInfoUpdate
-- parseTagUpdate "PaymentGatewayResponseOptions" = return PaymentGatewayResponseUpdate
-- parseTagUpdate "MetadataOptions" = return MetadataUpdate
-- parseTagUpdate "ChargebackOptions" = return ChargebackUpdate
-- parseTagUpdate "WalletAccountOptions" = return WalletAccountUpdate
-- parseTagUpdate "GatewayStatusMapOptions" = return GatewayStatusMapUpdate
-- parseTagUpdate "TokenOptions" = return TokenUpdate
-- parseTagUpdate "MerchantLockerAccountOptions" = return MerchantLockerAccountUpdate
-- parseTagUpdate "TempCardOptions" = return TempCardUpdate
-- parseTagUpdate "MerchantRiskSettingsOptions" = return MerchantRiskSettingsUpdate
-- parseTagUpdate "UserOptions" = return UserUpdate
-- parseTagUpdate "CofDetailsOptions" = return CofDetailsUpdate
-- parseTagUpdate "OrderMetadataV2Options" = return OrderMetadataV2Update
-- parseTagUpdate "StoredCardOptions" = return StoredCardUpdate
-- parseTagUpdate "TokenCustomerOptions" = return TokenCustomerUpdate
-- parseTagUpdate "EnrolledPanOptions" = return EnrolledPanUpdate
-- parseTagUpdate "OffersOptions" = return OffersUpdate
-- parseTagUpdate "RoleOptions" = return RoleUpdate
-- parseTagUpdate "FeatureOptions" = return FeatureUpdate
-- parseTagUpdate "GatewayBankEmiSupportOptions" = return GatewayBankEmiSupportUpdate
-- parseTagUpdate "AuthenticationAccountOptions" = return AuthenticationAccountUpdate
-- parseTagUpdate "PaymentGatewayResponseV1Options" = return PaymentGatewayResponseV1Update
-- parseTagUpdate "SavedPaymentMethodOptions" = return SavedPaymentMethodUpdate
-- parseTagUpdate "MerchantProviderDetailsOptions" = return MerchantProviderDetailsUpdate
-- parseTagUpdate "TxnOfferOptions" = return TxnOfferUpdate
-- parseTagUpdate "GatewayHealthOptions" = return GatewayHealthUpdate
-- parseTagUpdate "RiskManagementAccountOptions" = return RiskManagementAccountUpdate
-- parseTagUpdate "CardInfoOptions" = return CardInfoUpdate
-- parseTagUpdate "DeviceBindingOptions" = return DeviceBindingUpdate
-- parseTagUpdate "NotificationOptions" = return NotificationUpdate
-- parseTagUpdate "OrderBasketOptions" = return OrderBasketUpdate
-- parseTagUpdate "GatewayPaymentMethodOptions" = return GatewayPaymentMethodUpdate
-- parseTagUpdate "ProcessTrackerOptions" = return ProcessTrackerUpdate
-- parseTagUpdate "PaymentLinksOptions" = return PaymentLinksUpdate
-- parseTagUpdate "CustomerAccountOptions" = return CustomerAccountUpdate
-- parseTagUpdate "EntityMapOptions" = return EntityMapUpdate
-- parseTagUpdate "FormInputOptions" = return FormInputUpdate
-- parseTagUpdate "IngressRuleOptions" = return IngressRuleUpdate
-- parseTagUpdate "InstallmentOptions" = return InstallmentUpdate
-- parseTagUpdate "InstallmentRefundOptions" = return InstallmentRefundUpdate
-- parseTagUpdate "PaymentFormOptions" = return PaymentFormUpdate
-- parseTagUpdate "UserRoleOptions" = return UserRoleUpdate
-- parseTagUpdate "TxnIntentDetailOptions" = return TxnIntentDetailUpdate
-- parseTagUpdate "AuthMappingOptions" = return AuthMappingUpdate
-- parseTagUpdate t = fail $ T.unpack ("Expected a UpdateModel but got '" <> t <> "'")

-- data DBUpdateObject
--   = TxnOfferInfoOptions UpdateModel [Set MySQL TxnOfferInfoT] (Where MySQL TxnOfferInfoT)
--   | JuspayEventOptions UpdateModel [Set MySQL JuspayEventT] (Where MySQL JuspayEventT)
--   | HdfcHashedNumOptions UpdateModel [Set MySQL HdfcHashedNumT] (Where MySQL HdfcHashedNumT)
--   | MerchantGatewayAccountOptions UpdateModel [Set MySQL MerchantGatewayAccountT] (Where MySQL MerchantGatewayAccountT)
--   | MerchantGatewayAccountSubInfoOptions UpdateModel [Set MySQL MerchantGatewayAccountSubInfoT] (Where MySQL MerchantGatewayAccountSubInfoT)
--   | OrderReferenceOptions UpdateModel [Set MySQL OrderReferenceT] (Where MySQL OrderReferenceT)
--   | MandateOptions UpdateModel [Set MySQL MandateT] (Where MySQL MandateT)
--   | GatewayTxnDataOptions UpdateModel [Set MySQL GatewayTxnDataT] (Where MySQL GatewayTxnDataT)
--   | MerchantGatewayCardInfoOptions UpdateModel [Set MySQL MerchantGatewayCardInfoT] (Where MySQL MerchantGatewayCardInfoT)
--   | TxnRiskCheckOptions UpdateModel [Set MySQL TxnRiskCheckT] (Where MySQL TxnRiskCheckT)
--   | PaymentMethodOptions UpdateModel [Set MySQL PaymentMethodT] (Where MySQL PaymentMethodT)
--   | OfferBenefitInfoOptions UpdateModel [Set MySQL OfferBenefitInfoT] (Where MySQL OfferBenefitInfoT)
--   | MerchantIframePreferencesOptions UpdateModel [Set MySQL MerchantIframePreferencesT] (Where MySQL MerchantIframePreferencesT)
--   | ResellerAccountOptions UpdateModel [Set MySQL ResellerAccountT] (Where MySQL ResellerAccountT)
--   | UnifiedGatewayResponseOptions UpdateModel [Set MySQL UnifiedGatewayResponseT] (Where MySQL UnifiedGatewayResponseT)
--   | WalletTopUpTxnOptions UpdateModel [Set MySQL WalletTopUpTxnT] (Where MySQL WalletTopUpTxnT)
--   | IsinRoutesOptions UpdateModel [Set MySQL IsinRoutesT] (Where MySQL IsinRoutesT)
--   | TxnCardInfoOptions UpdateModel [Set MySQL TxnCardInfoT] (Where MySQL TxnCardInfoT)
--   | OrderAddressOptions UpdateModel [Set MySQL OrderAddressT] (Where MySQL OrderAddressT)
--   | LockerAccountOptions UpdateModel [Set MySQL LockerAccountT] (Where MySQL LockerAccountT)
--   | JuspayBankCodeOptions UpdateModel [Set MySQL JuspayBankCodeT] (Where MySQL JuspayBankCodeT)
--   | IssuerRoutesOptions UpdateModel [Set MySQL IssuerRoutesT] (Where MySQL IssuerRoutesT)
--   | RefundOptions UpdateModel [Set MySQL RefundT] (Where MySQL RefundT)
--   | SecondFactorOptions UpdateModel [Set MySQL SecondFactorT] (Where MySQL SecondFactorT)
--   | GatewayOutageOptions UpdateModel [Set MySQL GatewayOutageT] (Where MySQL GatewayOutageT)
--   | TxnDetailOptions UpdateModel [Set MySQL TxnDetailT] (Where MySQL TxnDetailT)
--   | EmiPlanOptions UpdateModel [Set MySQL EmiPlanT] (Where MySQL EmiPlanT)
--   | MerchantKeyOptions UpdateModel [Set MySQL MerchantKeyT] (Where MySQL MerchantKeyT)
--   | NetworkCardFingerprintOptions UpdateModel [Set MySQL NetworkCardFingerprintT] (Where MySQL NetworkCardFingerprintT)
--   | TokenRequestorOptions UpdateModel [Set MySQL TokenRequestorT] (Where MySQL TokenRequestorT)
--   | TxnOfferDetailOptions UpdateModel [Set MySQL TxnOfferDetailT] (Where MySQL TxnOfferDetailT)
--   | SecondFactorResponseOptions UpdateModel [Set MySQL SecondFactorResponseT] (Where MySQL SecondFactorResponseT)
--   | CardBrandRoutesOptions UpdateModel [Set MySQL CardBrandRoutesT] (Where MySQL CardBrandRoutesT)
--   | CustomerOptions UpdateModel [Set MySQL CustomerT] (Where MySQL CustomerT)
--   | MerchantAccountOptions UpdateModel [Set MySQL MerchantAccountT] (Where MySQL MerchantAccountT)
--   | TokenBinInfoOptions UpdateModel [Set MySQL TokenBinInfoT] (Where MySQL TokenBinInfoT)
--   | MerchantGatewayPaymentMethodOptions UpdateModel [Set MySQL MerchantGatewayPaymentMethodT] (Where MySQL MerchantGatewayPaymentMethodT)
--   | PromotionOptions UpdateModel [Set MySQL PromotionT] (Where MySQL PromotionT)
--   | LockerTokenRequestorOptions UpdateModel [Set MySQL LockerTokenRequestorT] (Where MySQL LockerTokenRequestorT)
--   | BankAccountOptions UpdateModel [Set MySQL BankAccountT] (Where MySQL BankAccountT)
--   | ProviderOptions UpdateModel [Set MySQL ProviderT] (Where MySQL ProviderT)
--   | GatewayCardInfoOptions UpdateModel [Set MySQL GatewayCardInfoT] (Where MySQL GatewayCardInfoT)
--   | PaymentGatewayResponseOptions UpdateModel [Set MySQL PaymentGatewayResponseT] (Where MySQL PaymentGatewayResponseT)
--   | MetadataOptions UpdateModel [Set MySQL MetadataT] (Where MySQL MetadataT)
--   | ChargebackOptions UpdateModel [Set MySQL ChargebackT] (Where MySQL ChargebackT)
--   | WalletAccountOptions UpdateModel [Set MySQL WalletAccountT] (Where MySQL WalletAccountT)
--   | GatewayStatusMapOptions UpdateModel [Set MySQL GatewayStatusMapT] (Where MySQL GatewayStatusMapT)
--   | TokenOptions UpdateModel [Set MySQL TokenT] (Where MySQL TokenT)
--   | MerchantLockerAccountOptions UpdateModel [Set MySQL MerchantLockerAccountT] (Where MySQL MerchantLockerAccountT)
--   | TempCardOptions UpdateModel [Set MySQL TempCardT] (Where MySQL TempCardT)
--   | MerchantRiskSettingsOptions UpdateModel [Set MySQL MerchantRiskSettingsT] (Where MySQL MerchantRiskSettingsT)
--   | UserOptions UpdateModel [Set MySQL UserT] (Where MySQL UserT)
--   | CofDetailsOptions UpdateModel [Set MySQL CofDetailsT] (Where MySQL CofDetailsT)
--   | OrderMetadataV2Options UpdateModel [Set MySQL OrderMetadataV2T] (Where MySQL OrderMetadataV2T)
--   | StoredCardOptions UpdateModel [Set MySQL StoredCardT] (Where MySQL StoredCardT)
--   | TokenCustomerOptions UpdateModel [Set MySQL TokenCustomerT] (Where MySQL TokenCustomerT)
--   | EnrolledPanOptions UpdateModel [Set MySQL EnrolledPanT] (Where MySQL EnrolledPanT)
--   | RoleOptions UpdateModel [Set MySQL RoleT] (Where MySQL RoleT)
--   | FeatureOptions UpdateModel [Set MySQL FeatureT] (Where MySQL FeatureT)
--   | GatewayBankEmiSupportOptions UpdateModel [Set MySQL GatewayBankEmiSupportT] (Where MySQL GatewayBankEmiSupportT)
--   | AuthenticationAccountOptions UpdateModel [Set MySQL AuthenticationAccountT] (Where MySQL AuthenticationAccountT)
--   | PaymentGatewayResponseV1Options UpdateModel [Set MySQL PaymentGatewayResponseV1T] (Where MySQL PaymentGatewayResponseV1T)
--   | MerchantProviderDetailsOptions UpdateModel [Set MySQL MerchantProviderDetailsT] (Where MySQL MerchantProviderDetailsT)
--   | TxnOfferOptions UpdateModel [Set MySQL TxnOfferT] (Where MySQL TxnOfferT)
--   | GatewayHealthOptions UpdateModel [Set MySQL GatewayHealthT] (Where MySQL GatewayHealthT)
--   | RiskManagementAccountOptions UpdateModel [Set MySQL RiskManagementAccountT] (Where MySQL RiskManagementAccountT)
--   | CardInfoOptions UpdateModel [Set MySQL CardInfoT] (Where MySQL CardInfoT)
--   | DeviceBindingOptions UpdateModel [Set MySQL DeviceBindingT] (Where MySQL DeviceBindingT)
--   | NotificationOptions UpdateModel [Set MySQL NotificationT] (Where MySQL NotificationT)
--   | OrderBasketOptions UpdateModel [Set MySQL OrderBasketT] (Where MySQL OrderBasketT)
--   | GatewayPaymentMethodOptions UpdateModel [Set MySQL GatewayPaymentMethodT] (Where MySQL GatewayPaymentMethodT)
--   | PaymentLinksOptions UpdateModel [Set MySQL PaymentLinksT] (Where MySQL PaymentLinksT)
--   | CustomerAccountOptions UpdateModel [Set MySQL CustomerAccountT] (Where MySQL CustomerAccountT)
--   | EntityMapOptions UpdateModel [Set MySQL EntityMapT] (Where MySQL EntityMapT)
--   | FormInputOptions UpdateModel [Set MySQL FormInputT] (Where MySQL FormInputT)
--   | IngressRuleOptions UpdateModel [Set MySQL IngressRuleT] (Where MySQL IngressRuleT)
--   | InstallmentOptions UpdateModel [Set MySQL InstallmentT] (Where MySQL InstallmentT)
--   | InstallmentRefundOptions UpdateModel [Set MySQL InstallmentRefundT] (Where MySQL InstallmentRefundT)
--   | PaymentFormOptions UpdateModel [Set MySQL PaymentFormT] (Where MySQL PaymentFormT)
--   | UserRoleOptions UpdateModel [Set MySQL UserRoleT] (Where MySQL UserRoleT)
--   | TxnIntentDetailOptions UpdateModel [Set MySQL TxnIntentDetailT] (Where MySQL TxnIntentDetailT)
--   | AuthMappingOptions UpdateModel [Set MySQL AuthMappingT] (Where MySQL AuthMappingT)
--   | OffersOptions UpdateModel [Set Postgres OffersT] (Where Postgres OffersT)
--   | ExternalMerchantCustomerOptions UpdateModel [Set Postgres ExternalMerchantCustomerT] (Where Postgres ExternalMerchantCustomerT)
--   | RuleOptions UpdateModel [Set Postgres RuleT] (Where Postgres RuleT)
--   | OfferRedemptionOptions UpdateModel [Set Postgres OfferRedemptionT] (Where Postgres OfferRedemptionT)
--   | AgencyOptions UpdateModel [Set Postgres AgencyT] (Where Postgres AgencyT)
--   | SavedPaymentMethodOptions UpdateModel [Set Postgres SavedPaymentMethodT] (Where Postgres SavedPaymentMethodT)
--   | ProcessTrackerOptions UpdateModel [Set Postgres ProcessTrackerT] (Where Postgres ProcessTrackerT)

data DBUpdateObject
  = AppInstallsOptions UpdateModel [Set Postgres AppInstalls.AppInstallsT] (Where Postgres AppInstalls.AppInstallsT)
  | BlackListOrgOptions UpdateModel [Set Postgres BlackListOrg.BlackListOrgT] (Where Postgres BlackListOrg.BlackListOrgT)
  | BookingOptions UpdateModel [Set Postgres Booking.BookingT] (Where Postgres Booking.BookingT)
  | BookingLocationOptions UpdateModel [Set Postgres BookingLocation.BookingLocationT] (Where Postgres BookingLocation.BookingLocationT)
  | BookingCancellationReasonOptions UpdateModel [Set Postgres BookingCancellationReason.BookingCancellationReasonT] (Where Postgres BookingCancellationReason.BookingCancellationReasonT)
  | CallbackRequestOptions UpdateModel [Set Postgres CallbackRequest.CallbackRequestT] (Where Postgres CallbackRequest.CallbackRequestT)
  | CallStatusOptions UpdateModel [Set Postgres CallStatus.CallStatusT] (Where Postgres CallStatus.CallStatusT)
  | CancellationReasonOptions UpdateModel [Set Postgres CancellationReason.CancellationReasonT] (Where Postgres CancellationReason.CancellationReasonT)
  | DriverOfferOptions UpdateModel [Set Postgres DriverOffer.DriverOfferT] (Where Postgres DriverOffer.DriverOfferT)
  | EstimateOptions UpdateModel [Set Postgres Estimate.EstimateT] (Where Postgres Estimate.EstimateT)
  | EstimateBreakupOptions UpdateModel [Set Postgres EstimateBreakup.EstimateBreakupT] (Where Postgres EstimateBreakup.EstimateBreakupT)
  | ExophoneOptions UpdateModel [Set Postgres Exophone.ExophoneT] (Where Postgres Exophone.ExophoneT)
  | FareBreakupOptions UpdateModel [Set Postgres FareBreakup.FareBreakupT] (Where Postgres FareBreakup.FareBreakupT)
  | GeometryOptions UpdateModel [Set Postgres Geometry.GeometryT] (Where Postgres Geometry.GeometryT)
  | IssueOptions UpdateModel [Set Postgres Issue.IssueT] (Where Postgres Issue.IssueT)
  | PlaceNameCacheOptions UpdateModel [Set Postgres PlaceNameCache.PlaceNameCacheT] (Where Postgres PlaceNameCache.PlaceNameCacheT)
  | MerchantOptions UpdateModel [Set Postgres Merchant.MerchantT] (Where Postgres Merchant.MerchantT)
  | MerchantMessageOptions UpdateModel [Set Postgres MerchantMessage.MerchantMessageT] (Where Postgres MerchantMessage.MerchantMessageT)
  | MerchantPaymentMethodOptions UpdateModel [Set Postgres MerchantPaymentMethod.MerchantPaymentMethodT] (Where Postgres MerchantPaymentMethod.MerchantPaymentMethodT)
  | MerchantServiceConfigOptions UpdateModel [Set Postgres MerchantServiceConfig.MerchantServiceConfigT] (Where Postgres MerchantServiceConfig.MerchantServiceConfigT)
  | MerchantServiceUsageConfigOptions UpdateModel [Set Postgres MerchantServiceUsageConfig.MerchantServiceUsageConfigT] (Where Postgres MerchantServiceUsageConfig.MerchantServiceUsageConfigT)
  | MerchantConfigOptions UpdateModel [Set Postgres MerchantConfig.MerchantConfigT] (Where Postgres MerchantConfig.MerchantConfigT)
  | OnSearchEventOptions UpdateModel [Set Postgres OnSearchEvent.OnSearchEventT] (Where Postgres OnSearchEvent.OnSearchEventT)
  | PaymentOrderOptions UpdateModel [Set Postgres PaymentOrder.PaymentOrderT] (Where Postgres PaymentOrder.PaymentOrderT)
  | PaymentTransactionOptions UpdateModel [Set Postgres PaymentTransaction.PaymentTransactionT] (Where Postgres PaymentTransaction.PaymentTransactionT)
  | PersonOptions UpdateModel [Set Postgres Person.PersonT] (Where Postgres Person.PersonT)
  | PersonDefaultEmergencyNumberOptions UpdateModel [Set Postgres PersonDefaultEmergencyNumber.PersonDefaultEmergencyNumberT] (Where Postgres PersonDefaultEmergencyNumber.PersonDefaultEmergencyNumberT)
  | PersonFlowStatusOptions UpdateModel [Set Postgres PersonFlowStatus.PersonFlowStatusT] (Where Postgres PersonFlowStatus.PersonFlowStatusT)
  | QuoteOptions UpdateModel [Set Postgres Quote.QuoteT] (Where Postgres Quote.QuoteT)
  | RegistrationTokenOptions UpdateModel [Set Postgres RegistrationToken.RegistrationTokenT] (Where Postgres RegistrationToken.RegistrationTokenT)
  | RentalSlabOptions UpdateModel [Set Postgres RentalSlab.RentalSlabT] (Where Postgres RentalSlab.RentalSlabT)
  | RideOptions UpdateModel [Set Postgres Ride.RideT] (Where Postgres Ride.RideT)
  | SavedReqLocationOptions UpdateModel [Set Postgres SavedReqLocation.SavedReqLocationT] (Where Postgres SavedReqLocation.SavedReqLocationT)
  | SearchRequestOptions UpdateModel [Set Postgres SearchRequest.SearchRequestT] (Where Postgres SearchRequest.SearchRequestT)
  | SearchReqLocationOptions UpdateModel [Set Postgres SearchReqLocation.SearchReqLocationT] (Where Postgres SearchReqLocation.SearchReqLocationT)
  | SosOptions UpdateModel [Set Postgres Sos.SosT] (Where Postgres Sos.SosT)
  | SpecialZoneQuoteOptions UpdateModel [Set Postgres SpecialZoneQuote.SpecialZoneQuoteT] (Where Postgres SpecialZoneQuote.SpecialZoneQuoteT)
  | TripTermsOptions UpdateModel [Set Postgres TripTerms.TripTermsT] (Where Postgres TripTerms.TripTermsT)
  | WebengageOptions UpdateModel [Set Postgres Webengage.WebengageT] (Where Postgres Webengage.WebengageT)

-------------------------------- ToJSON DBUpdateObject -------------------------------------
instance ToJSON DBUpdateObject where
  toJSON = error "ToJSON not implemented for DBUpdateObject - Use getDbUpdateCommandJson instead" -- Using getDbUpdateCommandJson instead of toJSON

-- -------------------------------- FromJSON DBUpdateObject -----------------------------------
-- instance FromJSON DBUpdateObject where
--   parseJSON = A.withObject "DBUpdateObject" $ \o -> do
--     contents <- o .: "contents"
--     updateModel <- parseTagUpdate =<< (o .: "tag")
--     case updateModel of
--       TxnOfferInfoUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ TxnOfferInfoOptions updateModel updVals whereClause
--       JuspayEventUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ JuspayEventOptions updateModel updVals whereClause
--       HdfcHashedNumUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ HdfcHashedNumOptions updateModel updVals whereClause
--       MerchantGatewayAccountUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ MerchantGatewayAccountOptions updateModel updVals whereClause
--       RuleUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ RuleOptions updateModel updVals whereClause
--       MerchantGatewayAccountSubInfoUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ MerchantGatewayAccountSubInfoOptions updateModel updVals whereClause
--       OrderReferenceUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ OrderReferenceOptions updateModel updVals whereClause
--       MandateUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ MandateOptions updateModel updVals whereClause
--       GatewayTxnDataUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ GatewayTxnDataOptions updateModel updVals whereClause
--       MerchantGatewayCardInfoUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ MerchantGatewayCardInfoOptions updateModel updVals whereClause
--       TxnRiskCheckUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ TxnRiskCheckOptions updateModel updVals whereClause
--       PaymentMethodUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ PaymentMethodOptions updateModel updVals whereClause
--       OfferBenefitInfoUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ OfferBenefitInfoOptions updateModel updVals whereClause
--       MerchantIframePreferencesUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ MerchantIframePreferencesOptions updateModel updVals whereClause
--       ResellerAccountUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ ResellerAccountOptions updateModel updVals whereClause
--       UnifiedGatewayResponseUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ UnifiedGatewayResponseOptions updateModel updVals whereClause
--       WalletTopUpTxnUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ WalletTopUpTxnOptions updateModel updVals whereClause
--       IsinRoutesUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ IsinRoutesOptions updateModel updVals whereClause
--       TxnCardInfoUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ TxnCardInfoOptions updateModel updVals whereClause
--       OrderAddressUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ OrderAddressOptions updateModel updVals whereClause
--       LockerAccountUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ LockerAccountOptions updateModel updVals whereClause
--       JuspayBankCodeUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ JuspayBankCodeOptions updateModel updVals whereClause
--       IssuerRoutesUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ IssuerRoutesOptions updateModel updVals whereClause
--       RefundUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ RefundOptions updateModel updVals whereClause
--       SecondFactorUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ SecondFactorOptions updateModel updVals whereClause
--       GatewayOutageUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ GatewayOutageOptions updateModel updVals whereClause
--       TxnDetailUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ TxnDetailOptions updateModel updVals whereClause
--       EmiPlanUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ EmiPlanOptions updateModel updVals whereClause
--       MerchantKeyUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ MerchantKeyOptions updateModel updVals whereClause
--       NetworkCardFingerprintUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ NetworkCardFingerprintOptions updateModel updVals whereClause
--       TokenRequestorUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ TokenRequestorOptions updateModel updVals whereClause
--       TxnOfferDetailUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ TxnOfferDetailOptions updateModel updVals whereClause
--       SecondFactorResponseUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ SecondFactorResponseOptions updateModel updVals whereClause
--       OfferRedemptionUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ OfferRedemptionOptions updateModel updVals whereClause
--       CardBrandRoutesUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ CardBrandRoutesOptions updateModel updVals whereClause
--       CustomerUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ CustomerOptions updateModel updVals whereClause
--       MerchantAccountUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ MerchantAccountOptions updateModel updVals whereClause
--       ExternalMerchantCustomerUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ ExternalMerchantCustomerOptions updateModel updVals whereClause
--       TokenBinInfoUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ TokenBinInfoOptions updateModel updVals whereClause
--       MerchantGatewayPaymentMethodUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ MerchantGatewayPaymentMethodOptions updateModel updVals whereClause
--       PromotionUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ PromotionOptions updateModel updVals whereClause
--       LockerTokenRequestorUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ LockerTokenRequestorOptions updateModel updVals whereClause
--       BankAccountUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ BankAccountOptions updateModel updVals whereClause
--       AgencyUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ AgencyOptions updateModel updVals whereClause
--       ProviderUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ ProviderOptions updateModel updVals whereClause
--       GatewayCardInfoUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ GatewayCardInfoOptions updateModel updVals whereClause
--       PaymentGatewayResponseUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ PaymentGatewayResponseOptions updateModel updVals whereClause
--       MetadataUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ MetadataOptions updateModel updVals whereClause
--       ChargebackUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ ChargebackOptions updateModel updVals whereClause
--       WalletAccountUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ WalletAccountOptions updateModel updVals whereClause
--       GatewayStatusMapUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ GatewayStatusMapOptions updateModel updVals whereClause
--       TokenUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ TokenOptions updateModel updVals whereClause
--       MerchantLockerAccountUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ MerchantLockerAccountOptions updateModel updVals whereClause
--       TempCardUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ TempCardOptions updateModel updVals whereClause
--       MerchantRiskSettingsUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ MerchantRiskSettingsOptions updateModel updVals whereClause
--       UserUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ UserOptions updateModel updVals whereClause
--       CofDetailsUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ CofDetailsOptions updateModel updVals whereClause
--       OrderMetadataV2Update -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ OrderMetadataV2Options updateModel updVals whereClause
--       StoredCardUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ StoredCardOptions updateModel updVals whereClause
--       TokenCustomerUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ TokenCustomerOptions updateModel updVals whereClause
--       EnrolledPanUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ EnrolledPanOptions updateModel updVals whereClause
--       OffersUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ OffersOptions updateModel updVals whereClause
--       RoleUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ RoleOptions updateModel updVals whereClause
--       FeatureUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ FeatureOptions updateModel updVals whereClause
--       GatewayBankEmiSupportUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ GatewayBankEmiSupportOptions updateModel updVals whereClause
--       AuthenticationAccountUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ AuthenticationAccountOptions updateModel updVals whereClause
--       PaymentGatewayResponseV1Update -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ PaymentGatewayResponseV1Options updateModel updVals whereClause
--       SavedPaymentMethodUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ SavedPaymentMethodOptions updateModel updVals whereClause
--       MerchantProviderDetailsUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ MerchantProviderDetailsOptions updateModel updVals whereClause
--       TxnOfferUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ TxnOfferOptions updateModel updVals whereClause
--       GatewayHealthUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ GatewayHealthOptions updateModel updVals whereClause
--       RiskManagementAccountUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ RiskManagementAccountOptions updateModel updVals whereClause
--       CardInfoUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ CardInfoOptions updateModel updVals whereClause
--       DeviceBindingUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ DeviceBindingOptions updateModel updVals whereClause
--       NotificationUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ NotificationOptions updateModel updVals whereClause
--       OrderBasketUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ OrderBasketOptions updateModel updVals whereClause
--       GatewayPaymentMethodUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ GatewayPaymentMethodOptions updateModel updVals whereClause
--       ProcessTrackerUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ ProcessTrackerOptions updateModel updVals whereClause
--       PaymentLinksUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ PaymentLinksOptions updateModel updVals whereClause
--       CustomerAccountUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ CustomerAccountOptions updateModel updVals whereClause
--       EntityMapUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ EntityMapOptions updateModel updVals whereClause
--       FormInputUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ FormInputOptions updateModel updVals whereClause
--       IngressRuleUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ IngressRuleOptions updateModel updVals whereClause
--       InstallmentUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ InstallmentOptions updateModel updVals whereClause
--       InstallmentRefundUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ InstallmentRefundOptions updateModel updVals whereClause
--       PaymentFormUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ PaymentFormOptions updateModel updVals whereClause
--       UserRoleUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ UserRoleOptions updateModel updVals whereClause
--       TxnIntentDetailUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ TxnIntentDetailOptions updateModel updVals whereClause
--       AuthMappingUpdate -> do
--         (updVals, whereClause) <- parseUpdateCommandValues contents
--         return $ AuthMappingOptions updateModel updVals whereClause

instance FromJSON DBUpdateObject where
  parseJSON = A.withObject "DBUpdateObject" $ \o -> do
    contents <- o .: "contents"
    updateModel <- parseTagUpdate =<< (o .: "tag")
    case updateModel of
      AppInstallsUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ AppInstallsOptions updateModel updVals whereClause
      BlackListOrgUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ BlackListOrgOptions updateModel updVals whereClause
      BookingUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ BookingOptions updateModel updVals whereClause
      BookingLocationUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ BookingLocationOptions updateModel updVals whereClause
      BookingCancellationReasonUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ BookingCancellationReasonOptions updateModel updVals whereClause
      CallbackRequestUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ CallbackRequestOptions updateModel updVals whereClause
      CallStatusUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ CallStatusOptions updateModel updVals whereClause
      CancellationReasonUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ CancellationReasonOptions updateModel updVals whereClause
      DriverOfferUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ DriverOfferOptions updateModel updVals whereClause
      EstimateUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ EstimateOptions updateModel updVals whereClause
      EstimateBreakupUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ EstimateBreakupOptions updateModel updVals whereClause
      ExophoneUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ ExophoneOptions updateModel updVals whereClause
      FareBreakupUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ FareBreakupOptions updateModel updVals whereClause
      GeometryUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ GeometryOptions updateModel updVals whereClause
      IssueUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ IssueOptions updateModel updVals whereClause
      PlaceNameCacheUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ PlaceNameCacheOptions updateModel updVals whereClause
      MerchantUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MerchantOptions updateModel updVals whereClause
      MerchantMessageUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MerchantMessageOptions updateModel updVals whereClause
      MerchantPaymentMethodUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MerchantPaymentMethodOptions updateModel updVals whereClause
      MerchantServiceConfigUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MerchantServiceConfigOptions updateModel updVals whereClause
      MerchantServiceUsageConfigUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MerchantServiceUsageConfigOptions updateModel updVals whereClause
      MerchantConfigUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MerchantConfigOptions updateModel updVals whereClause
      OnSearchEventUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ OnSearchEventOptions updateModel updVals whereClause
      PaymentOrderUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ PaymentOrderOptions updateModel updVals whereClause
      PaymentTransactionUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ PaymentTransactionOptions updateModel updVals whereClause
      PersonUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ PersonOptions updateModel updVals whereClause
      PersonDefaultEmergencyNumberUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ PersonDefaultEmergencyNumberOptions updateModel updVals whereClause
      PersonFlowStatusUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ PersonFlowStatusOptions updateModel updVals whereClause
      QuoteUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ QuoteOptions updateModel updVals whereClause
      RegistrationTokenUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ RegistrationTokenOptions updateModel updVals whereClause
      RentalSlabUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ RentalSlabOptions updateModel updVals whereClause
      RideUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ RideOptions updateModel updVals whereClause
      SavedReqLocationUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ SavedReqLocationOptions updateModel updVals whereClause
      SearchRequestUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ SearchRequestOptions updateModel updVals whereClause
      SearchReqLocationUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ SearchReqLocationOptions updateModel updVals whereClause
      SosUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ SosOptions updateModel updVals whereClause
      SpecialZoneQuoteUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ SpecialZoneQuoteOptions updateModel updVals whereClause
      TripTermsUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ TripTermsOptions updateModel updVals whereClause
      WebengageUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ WebengageOptions updateModel updVals whereClause
