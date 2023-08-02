{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Types.DBSync.Delete where

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
import qualified "rider-app" Storage.Beam.FeedbackForm as FeedbackForm
import qualified "rider-app" Storage.Beam.Geometry as Geometry
import qualified "rider-app" Storage.Beam.HotSpotConfig as HotSpotConfig
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

-- data DeleteModel
--   = TxnOfferInfoDelete
--   | TestTableDelete
--   | HdfcHashedNumDelete
--   | MerchantGatewayAccountDelete
--   | BankInfoDelete
--   | RuleDelete
--   | MerchantProviderCofDetailsDelete
--   | MerchantCustomerCofDetailsDelete
--   | MerchantGatewayAccountSubInfoDelete
--   | MandateDelete
--   | OrderReferenceDelete
--   | MerchantGatewayCardInfoDelete
--   | GatewayTxnDataDelete
--   | PaymentMethodDelete
--   | TxnRiskCheckDelete
--   | MerchantIframePreferencesDelete
--   | OfferBenefitInfoDelete
--   | UnifiedGatewayResponseDelete
--   | ResellerAccountDelete
--   | IsinRoutesDelete
--   | WalletTopUpTxnDelete
--   | OrderAddressDelete
--   | TxnCardInfoDelete
--   | JuspayBankCodeDelete
--   | LockerAccountDelete
--   | IssuerRoutesDelete
--   | MerchantPriorityLogicDelete
--   | SecondFactorDelete
--   | RefundDelete
--   | TxnDetailDelete
--   | GatewayOutageDelete
--   | MerchantKeyDelete
--   | EmiPlanDelete
--   | NetworkCardFingerprintDelete
--   | TxnOfferDetailDelete
--   | TokenRequestorDelete
--   | EncryptionKeysDelete
--   | SecondFactorResponseDelete
--   | CardBrandRoutesDelete
--   | OfferRedemptionDelete
--   | MerchantAccountDelete
--   | CustomerDelete
--   | TokenBinInfoDelete
--   | ExternalMerchantCustomerDelete
--   | PromotionDelete
--   | MerchantGatewayPaymentMethodDelete
--   | BankAccountDelete
--   | LockerTokenRequestorDelete
--   | ProviderDelete
--   | AgencyDelete
--   | GatewayCardInfoDelete
--   | MetadataDelete
--   | PaymentGatewayResponseDelete
--   | JuspayErrorMapDelete
--   | GatewayTxnStatusMapDelete
--   | ChargebackDelete
--   | ConfigurationsDelete
--   | ProcessTrackerDelete
--   | GatewayStatusMapDelete
--   | WalletAccountDelete
--   | MerchantLockerAccountDelete
--   | TokenDelete
--   | TempCardDelete
--   | JuspayEventDelete
--   | UserDelete
--   | MerchantRiskSettingsDelete
--   | JuspayIssuerNameMappingDelete
--   | CofDetailsDelete
--   | OrderMetadataV2Delete
--   | ServiceConfigurationDelete
--   | TokenCustomerDelete
--   | StoredCardDelete
--   | EnrolledPanDelete
--   | PaymentLinksDelete
--   | OffersDelete
--   | FeatureDelete
--   | RoleDelete
--   | AuthenticationAccountDelete
--   | GatewayBankEmiSupportDelete
--   | SavedPaymentMethodDelete
--   | PaymentGatewayResponseV1Delete
--   | TxnOfferDelete
--   | MerchantProviderDetailsDelete
--   | RiskManagementAccountDelete
--   | GatewayHealthDelete
--   | DeviceBindingDelete
--   | CardInfoDelete
--   | OrderBasketDelete
--   | NotificationDelete
--   | GatewayPaymentMethodDelete
--   | CustomerAccountDelete
--   | EntityMapDelete
--   | FormInputDelete
--   | IngressRuleDelete
--   | InstallmentDelete
--   | InstallmentRefundDelete
--   | PaymentFormDelete
--   | UserRoleDelete
--   | TxnIntentDetailDelete
--   | AuthMappingDelete
--   deriving (Generic, Show)

data DeleteModel
  = AppInstallsDelete
  | BlackListOrgDelete
  | BookingDelete
  | BookingLocationDelete
  | BookingCancellationReasonDelete
  | CallbackRequestDelete
  | CallStatusDelete
  | CancellationReasonDelete
  | DriverOfferDelete
  | EstimateDelete
  | EstimateBreakupDelete
  | ExophoneDelete
  | FareBreakupDelete
  | GeometryDelete
  | IssueDelete
  | PlaceNameCacheDelete
  | MerchantDelete
  | MerchantMessageDelete
  | MerchantPaymentMethodDelete
  | MerchantServiceConfigDelete
  | MerchantServiceUsageConfigDelete
  | MerchantConfigDelete
  | OnSearchEventDelete
  | PaymentOrderDelete
  | PaymentTransactionDelete
  | PersonDelete
  | PersonDefaultEmergencyNumberDelete
  | PersonFlowStatusDelete
  | QuoteDelete
  | RegistrationTokenDelete
  | RentalSlabDelete
  | RideDelete
  | SavedReqLocationDelete
  | SearchRequestDelete
  | SearchReqLocationDelete
  | SosDelete
  | SpecialZoneQuoteDelete
  | TripTermsDelete
  | WebengageDelete
  | FeedbackFormDelete
  | HotSpotConfigDelete
  deriving (Generic, Show)

getTagDelete :: DeleteModel -> Text
getTagDelete AppInstallsDelete = "AppInstallsOptions"
getTagDelete BlackListOrgDelete = "BlackListOrgOptions"
getTagDelete BookingDelete = "BookingOptions"
getTagDelete BookingLocationDelete = "BookingLocationOptions"
getTagDelete BookingCancellationReasonDelete = "BookingCancellationReasonOptions"
getTagDelete CallbackRequestDelete = "CallbackRequestOptions"
getTagDelete CallStatusDelete = "CallStatusOptions"
getTagDelete CancellationReasonDelete = "CancellationReasonOptions"
getTagDelete DriverOfferDelete = "DriverOfferOptions"
getTagDelete EstimateDelete = "EstimateOptions"
getTagDelete EstimateBreakupDelete = "EstimateBreakupOptions"
getTagDelete ExophoneDelete = "ExophoneOptions"
getTagDelete FareBreakupDelete = "FareBreakupOptions"
getTagDelete GeometryDelete = "GeometryOptions"
getTagDelete IssueDelete = "IssueOptions"
getTagDelete PlaceNameCacheDelete = "PlaceNameCacheOptions"
getTagDelete MerchantDelete = "MerchantOptions"
getTagDelete MerchantMessageDelete = "MerchantMessageOptions"
getTagDelete MerchantPaymentMethodDelete = "MerchantPaymentMethodOptions"
getTagDelete MerchantServiceConfigDelete = "MerchantServiceConfigOptions"
getTagDelete MerchantServiceUsageConfigDelete = "MerchantServiceUsageConfigOptions"
getTagDelete MerchantConfigDelete = "MerchantConfigOptions"
getTagDelete OnSearchEventDelete = "OnSearchEventOptions"
getTagDelete PaymentOrderDelete = "PaymentOrderOptions"
getTagDelete PaymentTransactionDelete = "PaymentTransactionOptions"
getTagDelete PersonDelete = "PersonOptions"
getTagDelete PersonDefaultEmergencyNumberDelete = "PersonDefaultEmergencyNumberOptions"
getTagDelete PersonFlowStatusDelete = "PersonFlowStatusOptions"
getTagDelete QuoteDelete = "QuoteOptions"
getTagDelete RegistrationTokenDelete = "RegistrationTokenOptions"
getTagDelete RentalSlabDelete = "RentalSlabOptions"
getTagDelete RideDelete = "RideOptions"
getTagDelete SavedReqLocationDelete = "SavedReqLocationOptions"
getTagDelete SearchRequestDelete = "SearchRequestOptions"
getTagDelete SearchReqLocationDelete = "SearchReqLocationOptions"
getTagDelete SosDelete = "SosOptions"
getTagDelete SpecialZoneQuoteDelete = "SpecialZoneQuoteOptions"
getTagDelete TripTermsDelete = "TripTermsOptions"
getTagDelete WebengageDelete = "WebengageOptions"
getTagDelete FeedbackFormDelete = "FeedbackFormOptions"
getTagDelete HotSpotConfigDelete = "HotSpotConfigOptions"

-- parseTagDelete :: Text -> Parser DeleteModel
-- parseTagDelete "TxnOfferInfoOptions" = return TxnOfferInfoDelete
-- parseTagDelete "TestTableOptions" = return TestTableDelete
-- parseTagDelete "HdfcHashedNumOptions" = return HdfcHashedNumDelete
-- parseTagDelete "MerchantGatewayAccountOptions" = return MerchantGatewayAccountDelete
-- parseTagDelete "BankInfoOptions" = return BankInfoDelete
-- parseTagDelete "MerchantProviderCofDetailsOptions" = return MerchantProviderCofDetailsDelete
-- parseTagDelete "RuleOptions" = return RuleDelete
-- parseTagDelete "MerchantGatewayAccountSubInfoOptions" = return MerchantGatewayAccountSubInfoDelete
-- parseTagDelete "MerchantCustomerCofDetailsOptions" = return MerchantCustomerCofDetailsDelete
-- parseTagDelete "OrderReferenceOptions" = return OrderReferenceDelete
-- parseTagDelete "MandateOptions" = return MandateDelete
-- parseTagDelete "GatewayTxnDataOptions" = return GatewayTxnDataDelete
-- parseTagDelete "MerchantGatewayCardInfoOptions" = return MerchantGatewayCardInfoDelete
-- parseTagDelete "TxnRiskCheckOptions" = return TxnRiskCheckDelete
-- parseTagDelete "PaymentMethodOptions" = return PaymentMethodDelete
-- parseTagDelete "OfferBenefitInfoOptions" = return OfferBenefitInfoDelete
-- parseTagDelete "MerchantIframePreferencesOptions" = return MerchantIframePreferencesDelete
-- parseTagDelete "ResellerAccountOptions" = return ResellerAccountDelete
-- parseTagDelete "UnifiedGatewayResponseOptions" = return UnifiedGatewayResponseDelete
-- parseTagDelete "WalletTopUpTxnOptions" = return WalletTopUpTxnDelete
-- parseTagDelete "IsinRoutesOptions" = return IsinRoutesDelete
-- parseTagDelete "TxnCardInfoOptions" = return TxnCardInfoDelete
-- parseTagDelete "OrderAddressOptions" = return OrderAddressDelete
-- parseTagDelete "LockerAccountOptions" = return LockerAccountDelete
-- parseTagDelete "JuspayBankCodeOptions" = return JuspayBankCodeDelete
-- parseTagDelete "MerchantPriorityLogicOptions" = return MerchantPriorityLogicDelete
-- parseTagDelete "IssuerRoutesOptions" = return IssuerRoutesDelete
-- parseTagDelete "RefundOptions" = return RefundDelete
-- parseTagDelete "SecondFactorOptions" = return SecondFactorDelete
-- parseTagDelete "GatewayOutageOptions" = return GatewayOutageDelete
-- parseTagDelete "TxnDetailOptions" = return TxnDetailDelete
-- parseTagDelete "EmiPlanOptions" = return EmiPlanDelete
-- parseTagDelete "MerchantKeyOptions" = return MerchantKeyDelete
-- parseTagDelete "NetworkCardFingerprintOptions" = return NetworkCardFingerprintDelete
-- parseTagDelete "TokenRequestorOptions" = return TokenRequestorDelete
-- parseTagDelete "TxnOfferDetailOptions" = return TxnOfferDetailDelete
-- parseTagDelete "SecondFactorResponseOptions" = return SecondFactorResponseDelete
-- parseTagDelete "EncryptionKeysOptions" = return EncryptionKeysDelete
-- parseTagDelete "OfferRedemptionOptions" = return OfferRedemptionDelete
-- parseTagDelete "CardBrandRoutesOptions" = return CardBrandRoutesDelete
-- parseTagDelete "CustomerOptions" = return CustomerDelete
-- parseTagDelete "MerchantAccountOptions" = return MerchantAccountDelete
-- parseTagDelete "ExternalMerchantCustomerOptions" = return ExternalMerchantCustomerDelete
-- parseTagDelete "TokenBinInfoOptions" = return TokenBinInfoDelete
-- parseTagDelete "MerchantGatewayPaymentMethodOptions" = return MerchantGatewayPaymentMethodDelete
-- parseTagDelete "PromotionOptions" = return PromotionDelete
-- parseTagDelete "LockerTokenRequestorOptions" = return LockerTokenRequestorDelete
-- parseTagDelete "BankAccountOptions" = return BankAccountDelete
-- parseTagDelete "AgencyOptions" = return AgencyDelete
-- parseTagDelete "ProviderOptions" = return ProviderDelete
-- parseTagDelete "GatewayCardInfoOptions" = return GatewayCardInfoDelete
-- parseTagDelete "PaymentGatewayResponseOptions" = return PaymentGatewayResponseDelete
-- parseTagDelete "MetadataOptions" = return MetadataDelete
-- parseTagDelete "GatewayTxnStatusMapOptions" = return GatewayTxnStatusMapDelete
-- parseTagDelete "JuspayErrorMapOptions" = return JuspayErrorMapDelete
-- parseTagDelete "ChargebackOptions" = return ChargebackDelete
-- parseTagDelete "ProcessTrackerOptions" = return ProcessTrackerDelete
-- parseTagDelete "ConfigurationsOptions" = return ConfigurationsDelete
-- parseTagDelete "WalletAccountOptions" = return WalletAccountDelete
-- parseTagDelete "GatewayStatusMapOptions" = return GatewayStatusMapDelete
-- parseTagDelete "TokenOptions" = return TokenDelete
-- parseTagDelete "MerchantLockerAccountOptions" = return MerchantLockerAccountDelete
-- parseTagDelete "JuspayEventOptions" = return JuspayEventDelete
-- parseTagDelete "TempCardOptions" = return TempCardDelete
-- parseTagDelete "MerchantRiskSettingsOptions" = return MerchantRiskSettingsDelete
-- parseTagDelete "UserOptions" = return UserDelete
-- parseTagDelete "CofDetailsOptions" = return CofDetailsDelete
-- parseTagDelete "JuspayIssuerNameMappingOptions" = return JuspayIssuerNameMappingDelete
-- parseTagDelete "ServiceConfigurationOptions" = return ServiceConfigurationDelete
-- parseTagDelete "OrderMetadataV2Options" = return OrderMetadataV2Delete
-- parseTagDelete "StoredCardOptions" = return StoredCardDelete
-- parseTagDelete "TokenCustomerOptions" = return TokenCustomerDelete
-- parseTagDelete "EnrolledPanOptions" = return EnrolledPanDelete
-- parseTagDelete "OffersOptions" = return OffersDelete
-- parseTagDelete "PaymentLinksOptions" = return PaymentLinksDelete
-- parseTagDelete "RoleOptions" = return RoleDelete
-- parseTagDelete "FeatureOptions" = return FeatureDelete
-- parseTagDelete "GatewayBankEmiSupportOptions" = return GatewayBankEmiSupportDelete
-- parseTagDelete "AuthenticationAccountOptions" = return AuthenticationAccountDelete
-- parseTagDelete "PaymentGatewayResponseV1Options" = return PaymentGatewayResponseV1Delete
-- parseTagDelete "SavedPaymentMethodOptions" = return SavedPaymentMethodDelete
-- parseTagDelete "MerchantProviderDetailsOptions" = return MerchantProviderDetailsDelete
-- parseTagDelete "TxnOfferOptions" = return TxnOfferDelete
-- parseTagDelete "GatewayHealthOptions" = return GatewayHealthDelete
-- parseTagDelete "RiskManagementAccountOptions" = return RiskManagementAccountDelete
-- parseTagDelete "CardInfoOptions" = return CardInfoDelete
-- parseTagDelete "DeviceBindingOptions" = return DeviceBindingDelete
-- parseTagDelete "NotificationOptions" = return NotificationDelete
-- parseTagDelete "OrderBasketOptions" = return OrderBasketDelete
-- parseTagDelete "GatewayPaymentMethodOptions" = return GatewayPaymentMethodDelete
-- parseTagDelete "CustomerAccountOptions" = return CustomerAccountDelete
-- parseTagDelete "EntityMapOptions" = return EntityMapDelete
-- parseTagDelete "FormInputOptions" = return FormInputDelete
-- parseTagDelete "IngressRuleOptions" = return IngressRuleDelete
-- parseTagDelete "InstallmentOptions" = return InstallmentDelete
-- parseTagDelete "InstallmentRefundOptions" = return InstallmentRefundDelete
-- parseTagDelete "PaymentFormOptions" = return PaymentFormDelete
-- parseTagDelete "UserRoleOptions" = return UserRoleDelete
-- parseTagDelete "TxnIntentDetailOptions" = return TxnIntentDetailDelete
-- parseTagDelete "AuthMappingOptions" = return AuthMappingDelete
-- parseTagDelete t = fail $ T.unpack ("Expected a DeleteModel but got '" <> t <> "'")

parseTagDelete :: Text -> Parser DeleteModel
parseTagDelete "AppInstallsOptions" = return AppInstallsDelete
parseTagDelete "BlackListOrgOptions" = return BlackListOrgDelete
parseTagDelete "BookingOptions" = return BookingDelete
parseTagDelete "BookingLocationOptions" = return BookingLocationDelete
parseTagDelete "BookingCancellationReasonOptions" = return BookingCancellationReasonDelete
parseTagDelete "CallbackRequestOptions" = return CallbackRequestDelete
parseTagDelete "CallStatusOptions" = return CallStatusDelete
parseTagDelete "CancellationReasonOptions" = return CancellationReasonDelete
parseTagDelete "DriverOfferOptions" = return DriverOfferDelete
parseTagDelete "EstimateOptions" = return EstimateDelete
parseTagDelete "EstimateBreakupOptions" = return EstimateBreakupDelete
parseTagDelete "ExophoneOptions" = return ExophoneDelete
parseTagDelete "FareBreakupOptions" = return FareBreakupDelete
parseTagDelete "GeometryOptions" = return GeometryDelete
parseTagDelete "IssueOptions" = return IssueDelete
parseTagDelete "PlaceNameCacheOptions" = return PlaceNameCacheDelete
parseTagDelete "MerchantOptions" = return MerchantDelete
parseTagDelete "MerchantMessageOptions" = return MerchantMessageDelete
parseTagDelete "MerchantPaymentMethodOptions" = return MerchantPaymentMethodDelete
parseTagDelete "MerchantServiceConfigOptions" = return MerchantServiceConfigDelete
parseTagDelete "MerchantServiceUsageConfigOptions" = return MerchantServiceUsageConfigDelete
parseTagDelete "MerchantConfigOptions" = return MerchantConfigDelete
parseTagDelete "OnSearchEventOptions" = return OnSearchEventDelete
parseTagDelete "PaymentOrderOptions" = return PaymentOrderDelete
parseTagDelete "PaymentTransactionOptions" = return PaymentTransactionDelete
parseTagDelete "PersonOptions" = return PersonDelete
parseTagDelete "PersonDefaultEmergencyNumberOptions" = return PersonDefaultEmergencyNumberDelete
parseTagDelete "PersonFlowStatusOptions" = return PersonFlowStatusDelete
parseTagDelete "QuoteOptions" = return QuoteDelete
parseTagDelete "RegistrationTokenOptions" = return RegistrationTokenDelete
parseTagDelete "RentalSlabOptions" = return RentalSlabDelete
parseTagDelete "RideOptions" = return RideDelete
parseTagDelete "SavedReqLocationOptions" = return SavedReqLocationDelete
parseTagDelete "SearchRequestOptions" = return SearchRequestDelete
parseTagDelete "SearchReqLocationOptions" = return SearchReqLocationDelete
parseTagDelete "SosOptions" = return SosDelete
parseTagDelete "SpecialZoneQuoteOptions" = return SpecialZoneQuoteDelete
parseTagDelete "TripTermsOptions" = return TripTermsDelete
parseTagDelete "WebengageOptions" = return WebengageDelete
parseTagDelete "FeedbackFormOptions" = return FeedbackFormDelete
parseTagDelete "HotSpotConfigOptions" = return HotSpotConfigDelete
parseTagDelete t = fail $ T.unpack ("Expected a DeleteModel but got '" <> t <> "'")

-- data DBDeleteObject
--   = TxnOfferInfoDeleteOptions DeleteModel (Where MySQL TxnOfferInfoT)
--   | HdfcHashedNumDeleteOptions DeleteModel (Where MySQL HdfcHashedNumT)
--   | MerchantGatewayAccountDeleteOptions DeleteModel (Where MySQL MerchantGatewayAccountT)
--   | MerchantGatewayAccountSubInfoDeleteOptions DeleteModel (Where MySQL MerchantGatewayAccountSubInfoT)
--   | OrderReferenceDeleteOptions DeleteModel (Where MySQL OrderReferenceT)
--   | MandateDeleteOptions DeleteModel (Where MySQL MandateT)
--   | GatewayTxnDataDeleteOptions DeleteModel (Where MySQL GatewayTxnDataT)
--   | MerchantGatewayCardInfoDeleteOptions DeleteModel (Where MySQL MerchantGatewayCardInfoT)
--   | TxnRiskCheckDeleteOptions DeleteModel (Where MySQL TxnRiskCheckT)
--   | PaymentMethodDeleteOptions DeleteModel (Where MySQL PaymentMethodT)
--   | OfferBenefitInfoDeleteOptions DeleteModel (Where MySQL OfferBenefitInfoT)
--   | MerchantIframePreferencesDeleteOptions DeleteModel (Where MySQL MerchantIframePreferencesT)
--   | ResellerAccountDeleteOptions DeleteModel (Where MySQL ResellerAccountT)
--   | UnifiedGatewayResponseDeleteOptions DeleteModel (Where MySQL UnifiedGatewayResponseT)
--   | WalletTopUpTxnDeleteOptions DeleteModel (Where MySQL WalletTopUpTxnT)
--   | IsinRoutesDeleteOptions DeleteModel (Where MySQL IsinRoutesT)
--   | TxnCardInfoDeleteOptions DeleteModel (Where MySQL TxnCardInfoT)
--   | OrderAddressDeleteOptions DeleteModel (Where MySQL OrderAddressT)
--   | LockerAccountDeleteOptions DeleteModel (Where MySQL LockerAccountT)
--   | JuspayBankCodeDeleteOptions DeleteModel (Where MySQL JuspayBankCodeT)
--   | IssuerRoutesDeleteOptions DeleteModel (Where MySQL IssuerRoutesT)
--   | RefundDeleteOptions DeleteModel (Where MySQL RefundT)
--   | SecondFactorDeleteOptions DeleteModel (Where MySQL SecondFactorT)
--   | GatewayOutageDeleteOptions DeleteModel (Where MySQL GatewayOutageT)
--   | TxnDetailDeleteOptions DeleteModel (Where MySQL TxnDetailT)
--   | EmiPlanDeleteOptions DeleteModel (Where MySQL EmiPlanT)
--   | MerchantKeyDeleteOptions DeleteModel (Where MySQL MerchantKeyT)
--   | NetworkCardFingerprintDeleteOptions DeleteModel (Where MySQL NetworkCardFingerprintT)
--   | TokenRequestorDeleteOptions DeleteModel (Where MySQL TokenRequestorT)
--   | TxnOfferDetailDeleteOptions DeleteModel (Where MySQL TxnOfferDetailT)
--   | SecondFactorResponseDeleteOptions DeleteModel (Where MySQL SecondFactorResponseT)
--   | CardBrandRoutesDeleteOptions DeleteModel (Where MySQL CardBrandRoutesT)
--   | CustomerDeleteOptions DeleteModel (Where MySQL CustomerT)
--   | MerchantAccountDeleteOptions DeleteModel (Where MySQL MerchantAccountT)
--   | TokenBinInfoDeleteOptions DeleteModel (Where MySQL TokenBinInfoT)
--   | MerchantGatewayPaymentMethodDeleteOptions DeleteModel (Where MySQL MerchantGatewayPaymentMethodT)
--   | PromotionDeleteOptions DeleteModel (Where MySQL PromotionT)
--   | LockerTokenRequestorDeleteOptions DeleteModel (Where MySQL LockerTokenRequestorT)
--   | BankAccountDeleteOptions DeleteModel (Where MySQL BankAccountT)
--   | ProviderDeleteOptions DeleteModel (Where MySQL ProviderT)
--   | GatewayCardInfoDeleteOptions DeleteModel (Where MySQL GatewayCardInfoT)
--   | PaymentGatewayResponseDeleteOptions DeleteModel (Where MySQL PaymentGatewayResponseT)
--   | MetadataDeleteOptions DeleteModel (Where MySQL MetadataT)
--   | ChargebackDeleteOptions DeleteModel (Where MySQL ChargebackT)
--   | WalletAccountDeleteOptions DeleteModel (Where MySQL WalletAccountT)
--   | GatewayStatusMapDeleteOptions DeleteModel (Where MySQL GatewayStatusMapT)
--   | TokenDeleteOptions DeleteModel (Where MySQL TokenT)
--   | MerchantLockerAccountDeleteOptions DeleteModel (Where MySQL MerchantLockerAccountT)
--   | JuspayEventDeleteOptions DeleteModel (Where MySQL JuspayEventT)
--   | TempCardDeleteOptions DeleteModel (Where MySQL TempCardT)
--   | MerchantRiskSettingsDeleteOptions DeleteModel (Where MySQL MerchantRiskSettingsT)
--   | UserDeleteOptions DeleteModel (Where MySQL UserT)
--   | CofDetailsDeleteOptions DeleteModel (Where MySQL CofDetailsT)
--   | OrderMetadataV2DeleteOptions DeleteModel (Where MySQL OrderMetadataV2T)
--   | StoredCardDeleteOptions DeleteModel (Where MySQL StoredCardT)
--   | TokenCustomerDeleteOptions DeleteModel (Where MySQL TokenCustomerT)
--   | EnrolledPanDeleteOptions DeleteModel (Where MySQL EnrolledPanT)
--   | RoleDeleteOptions DeleteModel (Where MySQL RoleT)
--   | FeatureDeleteOptions DeleteModel (Where MySQL FeatureT)
--   | GatewayBankEmiSupportDeleteOptions DeleteModel (Where MySQL GatewayBankEmiSupportT)
--   | AuthenticationAccountDeleteOptions DeleteModel (Where MySQL AuthenticationAccountT)
--   | PaymentGatewayResponseV1DeleteOptions DeleteModel (Where MySQL PaymentGatewayResponseV1T)
--   | MerchantProviderDetailsDeleteOptions DeleteModel (Where MySQL MerchantProviderDetailsT)
--   | TxnOfferDeleteOptions DeleteModel (Where MySQL TxnOfferT)
--   | GatewayHealthDeleteOptions DeleteModel (Where MySQL GatewayHealthT)
--   | RiskManagementAccountDeleteOptions DeleteModel (Where MySQL RiskManagementAccountT)
--   | CardInfoDeleteOptions DeleteModel (Where MySQL CardInfoT)
--   | DeviceBindingDeleteOptions DeleteModel (Where MySQL DeviceBindingT)
--   | NotificationDeleteOptions DeleteModel (Where MySQL NotificationT)
--   | OrderBasketDeleteOptions DeleteModel (Where MySQL OrderBasketT)
--   | GatewayPaymentMethodDeleteOptions DeleteModel (Where MySQL GatewayPaymentMethodT)
--   | PaymentLinksDeleteOptions DeleteModel (Where MySQL PaymentLinksT)
--   | CustomerAccountDeleteOptions DeleteModel (Where MySQL CustomerAccountT)
--   | EntityMapDeleteOptions DeleteModel (Where MySQL EntityMapT)
--   | FormInputDeleteOptions DeleteModel (Where MySQL FormInputT)
--   | IngressRuleDeleteOptions DeleteModel (Where MySQL IngressRuleT)
--   | InstallmentDeleteOptions DeleteModel (Where MySQL InstallmentT)
--   | InstallmentRefundDeleteOptions DeleteModel (Where MySQL InstallmentRefundT)
--   | PaymentFormDeleteOptions DeleteModel (Where MySQL PaymentFormT)
--   | UserRoleDeleteOptions DeleteModel (Where MySQL UserRoleT)
--   | TxnIntentDetailDeleteOptions DeleteModel (Where MySQL TxnIntentDetailT)
--   | AuthMappingDeleteOptions DeleteModel (Where MySQL AuthMappingT)
--   | RuleDeleteOptions DeleteModel (Where Postgres RuleT)
--   | OfferRedemptionDeleteOptions DeleteModel (Where Postgres OfferRedemptionT)
--   | AgencyDeleteOptions DeleteModel (Where Postgres AgencyT)
--   | ExternalMerchantCustomerDeleteOptions DeleteModel (Where Postgres ExternalMerchantCustomerT)
--   | ProcessTrackerDeleteOptions DeleteModel (Where Postgres ProcessTrackerT)
--   | OffersDeleteOptions DeleteModel (Where Postgres OffersT)
--   | SavedPaymentMethodDeleteOptions DeleteModel (Where Postgres SavedPaymentMethodT)

data DBDeleteObject
  = AppInstallsDeleteOptions DeleteModel (Where Postgres AppInstalls.AppInstallsT)
  | BlackListOrgDeleteOptions DeleteModel (Where Postgres BlackListOrg.BlackListOrgT)
  | BookingDeleteOptions DeleteModel (Where Postgres Booking.BookingT)
  | BookingLocationDeleteOptions DeleteModel (Where Postgres BookingLocation.BookingLocationT)
  | BookingCancellationReasonDeleteOptions DeleteModel (Where Postgres BookingCancellationReason.BookingCancellationReasonT)
  | CallbackRequestDeleteOptions DeleteModel (Where Postgres CallbackRequest.CallbackRequestT)
  | CallStatusDeleteOptions DeleteModel (Where Postgres CallStatus.CallStatusT)
  | CancellationReasonDeleteOptions DeleteModel (Where Postgres CancellationReason.CancellationReasonT)
  | DriverOfferDeleteOptions DeleteModel (Where Postgres DriverOffer.DriverOfferT)
  | EstimateDeleteOptions DeleteModel (Where Postgres Estimate.EstimateT)
  | EstimateBreakupDeleteOptions DeleteModel (Where Postgres EstimateBreakup.EstimateBreakupT)
  | ExophoneDeleteOptions DeleteModel (Where Postgres Exophone.ExophoneT)
  | FareBreakupDeleteOptions DeleteModel (Where Postgres FareBreakup.FareBreakupT)
  | GeometryDeleteOptions DeleteModel (Where Postgres Geometry.GeometryT)
  | IssueDeleteOptions DeleteModel (Where Postgres Issue.IssueT)
  | PlaceNameCacheDeleteOptions DeleteModel (Where Postgres PlaceNameCache.PlaceNameCacheT)
  | MerchantDeleteOptions DeleteModel (Where Postgres Merchant.MerchantT)
  | MerchantMessageDeleteOptions DeleteModel (Where Postgres MerchantMessage.MerchantMessageT)
  | MerchantPaymentMethodDeleteOptions DeleteModel (Where Postgres MerchantPaymentMethod.MerchantPaymentMethodT)
  | MerchantServiceConfigDeleteOptions DeleteModel (Where Postgres MerchantServiceConfig.MerchantServiceConfigT)
  | MerchantServiceUsageConfigDeleteOptions DeleteModel (Where Postgres MerchantServiceUsageConfig.MerchantServiceUsageConfigT)
  | MerchantConfigDeleteOptions DeleteModel (Where Postgres MerchantConfig.MerchantConfigT)
  | OnSearchEventDeleteOptions DeleteModel (Where Postgres OnSearchEvent.OnSearchEventT)
  | PaymentOrderDeleteOptions DeleteModel (Where Postgres PaymentOrder.PaymentOrderT)
  | PaymentTransactionDeleteOptions DeleteModel (Where Postgres PaymentTransaction.PaymentTransactionT)
  | PersonDeleteOptions DeleteModel (Where Postgres Person.PersonT)
  | PersonDefaultEmergencyNumberDeleteOptions DeleteModel (Where Postgres PersonDefaultEmergencyNumber.PersonDefaultEmergencyNumberT)
  | PersonFlowStatusDeleteOptions DeleteModel (Where Postgres PersonFlowStatus.PersonFlowStatusT)
  | QuoteDeleteOptions DeleteModel (Where Postgres Quote.QuoteT)
  | RegistrationTokenDeleteOptions DeleteModel (Where Postgres RegistrationToken.RegistrationTokenT)
  | RentalSlabDeleteOptions DeleteModel (Where Postgres RentalSlab.RentalSlabT)
  | RideDeleteOptions DeleteModel (Where Postgres Ride.RideT)
  | SavedReqLocationDeleteOptions DeleteModel (Where Postgres SavedReqLocation.SavedReqLocationT)
  | SearchRequestDeleteOptions DeleteModel (Where Postgres SearchRequest.SearchRequestT)
  | SearchReqLocationDeleteOptions DeleteModel (Where Postgres SearchReqLocation.SearchReqLocationT)
  | SosDeleteOptions DeleteModel (Where Postgres Sos.SosT)
  | SpecialZoneQuoteDeleteOptions DeleteModel (Where Postgres SpecialZoneQuote.SpecialZoneQuoteT)
  | TripTermsDeleteOptions DeleteModel (Where Postgres TripTerms.TripTermsT)
  | WebengageDeleteOptions DeleteModel (Where Postgres Webengage.WebengageT)
  | FeedbackFormDeleteOptions DeleteModel (Where Postgres FeedbackForm.FeedbackFormT)
  | HotSpotConfigDeleteOptions DeleteModel (Where Postgres HotSpotConfig.HotSpotConfigT)

instance ToJSON DBDeleteObject where
  toJSON = error "ToJSON not implemented for DBDeleteObject - Use getDbDeleteCommandJson instead" -- Using getDbDeleteCommandJson instead of toJSON

-- instance FromJSON DBDeleteObject where
--   parseJSON = A.withObject "DBDeleteObject" $ \o -> do
--     contents <- o .: "contents"
--     deleteModel <- parseTagDelete =<< (o .: "tag")
--     case deleteModel of
--       TxnOfferInfoDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ TxnOfferInfoDeleteOptions deleteModel whereClause
--       HdfcHashedNumDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ HdfcHashedNumDeleteOptions deleteModel whereClause
--       MerchantGatewayAccountDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ MerchantGatewayAccountDeleteOptions deleteModel whereClause
--       RuleDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ RuleDeleteOptions deleteModel whereClause
--       MerchantGatewayAccountSubInfoDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ MerchantGatewayAccountSubInfoDeleteOptions deleteModel whereClause
--       OrderReferenceDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ OrderReferenceDeleteOptions deleteModel whereClause
--       MandateDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ MandateDeleteOptions deleteModel whereClause
--       GatewayTxnDataDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ GatewayTxnDataDeleteOptions deleteModel whereClause
--       MerchantGatewayCardInfoDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ MerchantGatewayCardInfoDeleteOptions deleteModel whereClause
--       TxnRiskCheckDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ TxnRiskCheckDeleteOptions deleteModel whereClause
--       PaymentMethodDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ PaymentMethodDeleteOptions deleteModel whereClause
--       OfferBenefitInfoDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ OfferBenefitInfoDeleteOptions deleteModel whereClause
--       MerchantIframePreferencesDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ MerchantIframePreferencesDeleteOptions deleteModel whereClause
--       ResellerAccountDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ ResellerAccountDeleteOptions deleteModel whereClause
--       UnifiedGatewayResponseDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ UnifiedGatewayResponseDeleteOptions deleteModel whereClause
--       WalletTopUpTxnDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ WalletTopUpTxnDeleteOptions deleteModel whereClause
--       IsinRoutesDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ IsinRoutesDeleteOptions deleteModel whereClause
--       TxnCardInfoDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ TxnCardInfoDeleteOptions deleteModel whereClause
--       OrderAddressDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ OrderAddressDeleteOptions deleteModel whereClause
--       LockerAccountDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ LockerAccountDeleteOptions deleteModel whereClause
--       JuspayBankCodeDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ JuspayBankCodeDeleteOptions deleteModel whereClause
--       IssuerRoutesDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ IssuerRoutesDeleteOptions deleteModel whereClause
--       RefundDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ RefundDeleteOptions deleteModel whereClause
--       SecondFactorDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ SecondFactorDeleteOptions deleteModel whereClause
--       GatewayOutageDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ GatewayOutageDeleteOptions deleteModel whereClause
--       TxnDetailDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ TxnDetailDeleteOptions deleteModel whereClause
--       EmiPlanDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ EmiPlanDeleteOptions deleteModel whereClause
--       MerchantKeyDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ MerchantKeyDeleteOptions deleteModel whereClause
--       NetworkCardFingerprintDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ NetworkCardFingerprintDeleteOptions deleteModel whereClause
--       TokenRequestorDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ TokenRequestorDeleteOptions deleteModel whereClause
--       TxnOfferDetailDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ TxnOfferDetailDeleteOptions deleteModel whereClause
--       SecondFactorResponseDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ SecondFactorResponseDeleteOptions deleteModel whereClause
--       OfferRedemptionDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ OfferRedemptionDeleteOptions deleteModel whereClause
--       CardBrandRoutesDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ CardBrandRoutesDeleteOptions deleteModel whereClause
--       CustomerDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ CustomerDeleteOptions deleteModel whereClause
--       MerchantAccountDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ MerchantAccountDeleteOptions deleteModel whereClause
--       ExternalMerchantCustomerDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ ExternalMerchantCustomerDeleteOptions deleteModel whereClause
--       TokenBinInfoDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ TokenBinInfoDeleteOptions deleteModel whereClause
--       MerchantGatewayPaymentMethodDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ MerchantGatewayPaymentMethodDeleteOptions deleteModel whereClause
--       PromotionDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ PromotionDeleteOptions deleteModel whereClause
--       LockerTokenRequestorDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ LockerTokenRequestorDeleteOptions deleteModel whereClause
--       BankAccountDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ BankAccountDeleteOptions deleteModel whereClause
--       AgencyDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ AgencyDeleteOptions deleteModel whereClause
--       ProviderDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ ProviderDeleteOptions deleteModel whereClause
--       GatewayCardInfoDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ GatewayCardInfoDeleteOptions deleteModel whereClause
--       PaymentGatewayResponseDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ PaymentGatewayResponseDeleteOptions deleteModel whereClause
--       MetadataDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ MetadataDeleteOptions deleteModel whereClause
--       ChargebackDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ ChargebackDeleteOptions deleteModel whereClause
--       ProcessTrackerDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ ProcessTrackerDeleteOptions deleteModel whereClause
--       WalletAccountDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ WalletAccountDeleteOptions deleteModel whereClause
--       GatewayStatusMapDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ GatewayStatusMapDeleteOptions deleteModel whereClause
--       TokenDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ TokenDeleteOptions deleteModel whereClause
--       MerchantLockerAccountDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ MerchantLockerAccountDeleteOptions deleteModel whereClause
--       JuspayEventDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ JuspayEventDeleteOptions deleteModel whereClause
--       TempCardDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ TempCardDeleteOptions deleteModel whereClause
--       MerchantRiskSettingsDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ MerchantRiskSettingsDeleteOptions deleteModel whereClause
--       UserDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ UserDeleteOptions deleteModel whereClause
--       CofDetailsDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ CofDetailsDeleteOptions deleteModel whereClause
--       OrderMetadataV2Delete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ OrderMetadataV2DeleteOptions deleteModel whereClause
--       StoredCardDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ StoredCardDeleteOptions deleteModel whereClause
--       TokenCustomerDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ TokenCustomerDeleteOptions deleteModel whereClause
--       EnrolledPanDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ EnrolledPanDeleteOptions deleteModel whereClause
--       OffersDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ OffersDeleteOptions deleteModel whereClause
--       RoleDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ RoleDeleteOptions deleteModel whereClause
--       FeatureDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ FeatureDeleteOptions deleteModel whereClause
--       GatewayBankEmiSupportDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ GatewayBankEmiSupportDeleteOptions deleteModel whereClause
--       AuthenticationAccountDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ AuthenticationAccountDeleteOptions deleteModel whereClause
--       PaymentGatewayResponseV1Delete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ PaymentGatewayResponseV1DeleteOptions deleteModel whereClause
--       SavedPaymentMethodDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ SavedPaymentMethodDeleteOptions deleteModel whereClause
--       MerchantProviderDetailsDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ MerchantProviderDetailsDeleteOptions deleteModel whereClause
--       TxnOfferDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ TxnOfferDeleteOptions deleteModel whereClause
--       GatewayHealthDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ GatewayHealthDeleteOptions deleteModel whereClause
--       RiskManagementAccountDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ RiskManagementAccountDeleteOptions deleteModel whereClause
--       CardInfoDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ CardInfoDeleteOptions deleteModel whereClause
--       DeviceBindingDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ DeviceBindingDeleteOptions deleteModel whereClause
--       NotificationDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ NotificationDeleteOptions deleteModel whereClause
--       OrderBasketDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ OrderBasketDeleteOptions deleteModel whereClause
--       GatewayPaymentMethodDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ GatewayPaymentMethodDeleteOptions deleteModel whereClause
--       PaymentLinksDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ PaymentLinksDeleteOptions deleteModel whereClause
--       CustomerAccountDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ CustomerAccountDeleteOptions deleteModel whereClause
--       EntityMapDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ EntityMapDeleteOptions deleteModel whereClause
--       FormInputDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ FormInputDeleteOptions deleteModel whereClause
--       IngressRuleDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ IngressRuleDeleteOptions deleteModel whereClause
--       InstallmentDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ InstallmentDeleteOptions deleteModel whereClause
--       InstallmentRefundDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ InstallmentRefundDeleteOptions deleteModel whereClause
--       PaymentFormDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ PaymentFormDeleteOptions deleteModel whereClause
--       UserRoleDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ UserRoleDeleteOptions deleteModel whereClause
--       TxnIntentDetailDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ TxnIntentDetailDeleteOptions deleteModel whereClause
--       AuthMappingDelete -> do
--         whereClause <- parseDeleteCommandValues contents
--         return $ AuthMappingDeleteOptions deleteModel whereClause

instance FromJSON DBDeleteObject where
  parseJSON = A.withObject "DBDeleteObject" $ \o -> do
    contents <- o .: "contents"
    deleteModel <- parseTagDelete =<< (o .: "tag")
    case deleteModel of
      AppInstallsDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ AppInstallsDeleteOptions deleteModel whereClause
      BlackListOrgDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ BlackListOrgDeleteOptions deleteModel whereClause
      BookingDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ BookingDeleteOptions deleteModel whereClause
      BookingLocationDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ BookingLocationDeleteOptions deleteModel whereClause
      BookingCancellationReasonDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ BookingCancellationReasonDeleteOptions deleteModel whereClause
      CallbackRequestDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ CallbackRequestDeleteOptions deleteModel whereClause
      CallStatusDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ CallStatusDeleteOptions deleteModel whereClause
      CancellationReasonDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ CancellationReasonDeleteOptions deleteModel whereClause
      DriverOfferDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ DriverOfferDeleteOptions deleteModel whereClause
      EstimateDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ EstimateDeleteOptions deleteModel whereClause
      EstimateBreakupDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ EstimateBreakupDeleteOptions deleteModel whereClause
      ExophoneDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ ExophoneDeleteOptions deleteModel whereClause
      FareBreakupDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ FareBreakupDeleteOptions deleteModel whereClause
      GeometryDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ GeometryDeleteOptions deleteModel whereClause
      IssueDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ IssueDeleteOptions deleteModel whereClause
      PlaceNameCacheDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ PlaceNameCacheDeleteOptions deleteModel whereClause
      MerchantDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MerchantDeleteOptions deleteModel whereClause
      MerchantMessageDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MerchantMessageDeleteOptions deleteModel whereClause
      MerchantPaymentMethodDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MerchantPaymentMethodDeleteOptions deleteModel whereClause
      MerchantServiceConfigDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MerchantServiceConfigDeleteOptions deleteModel whereClause
      MerchantServiceUsageConfigDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MerchantServiceUsageConfigDeleteOptions deleteModel whereClause
      MerchantConfigDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MerchantConfigDeleteOptions deleteModel whereClause
      OnSearchEventDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ OnSearchEventDeleteOptions deleteModel whereClause
      PaymentOrderDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ PaymentOrderDeleteOptions deleteModel whereClause
      PaymentTransactionDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ PaymentTransactionDeleteOptions deleteModel whereClause
      PersonDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ PersonDeleteOptions deleteModel whereClause
      PersonDefaultEmergencyNumberDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ PersonDefaultEmergencyNumberDeleteOptions deleteModel whereClause
      PersonFlowStatusDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ PersonFlowStatusDeleteOptions deleteModel whereClause
      QuoteDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ QuoteDeleteOptions deleteModel whereClause
      RegistrationTokenDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ RegistrationTokenDeleteOptions deleteModel whereClause
      RentalSlabDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ RentalSlabDeleteOptions deleteModel whereClause
      RideDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ RideDeleteOptions deleteModel whereClause
      SavedReqLocationDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ SavedReqLocationDeleteOptions deleteModel whereClause
      SearchRequestDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ SearchRequestDeleteOptions deleteModel whereClause
      SearchReqLocationDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ SearchReqLocationDeleteOptions deleteModel whereClause
      SosDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ SosDeleteOptions deleteModel whereClause
      SpecialZoneQuoteDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ SpecialZoneQuoteDeleteOptions deleteModel whereClause
      TripTermsDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ TripTermsDeleteOptions deleteModel whereClause
      WebengageDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ WebengageDeleteOptions deleteModel whereClause
      FeedbackFormDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ FeedbackFormDeleteOptions deleteModel whereClause
      HotSpotConfigDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ HotSpotConfigDeleteOptions deleteModel whereClause
