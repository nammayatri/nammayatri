{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Types.DBSync.Delete where

import Data.Aeson as A
import Data.Aeson.Types (Parser)
import qualified Data.Text as T
-- import Database.Beam.MySQL (MySQL)
import Database.Beam.Postgres (Postgres)
-- import Euler.DB.Storage.Types
--   ( AgencyT,
--     AuthMappingT,
--     -- TODO: Export below tables from euler-db
--     -- , BankInfoT
--     -- , ClientConfigurationT
--     -- , ConfigurationsT
--     -- , EncryptionKeysT
--     -- , GatewayTxnStatusMapT
--     -- , JuspayErrorMapT
--     -- , JuspayIssuerNameMappingT
--     -- , MerchantPriorityLogicT
--     -- , MerchantCustomerCofDetailsT
--     -- , UserAclT
--     AuthenticationAccountT,
--     BankAccountT,
--     CardBrandRoutesT,
--     CardInfoT,
--     ChargebackT,
--     CofDetailsT,
--     CustomerAccountT,
--     CustomerT,
--     DeviceBindingT,
--     EmiPlanT,
--     EnrolledPanT,
--     EntityMapT,
--     ExternalMerchantCustomerT,
--     FeatureT,
--     FormInputT,
--     GatewayBankEmiSupportT,
--     GatewayCardInfoT,
--     GatewayHealthT,
--     GatewayOutageT,
--     GatewayPaymentMethodT,
--     GatewayStatusMapT,
--     GatewayTxnDataT,
--     HdfcHashedNumT,
--     IngressRuleT,
--     InstallmentRefundT,
--     InstallmentT,
--     IsinRoutesT,
--     IssuerRoutesT,
--     JuspayBankCodeT,
--     JuspayEventT,
--     LockerAccountT,
--     LockerTokenRequestorT,
--     MandateT,
--     MerchantAccountT,
--     MerchantGatewayAccountSubInfoT,
--     MerchantGatewayAccountT,
--     MerchantGatewayCardInfoT,
--     MerchantGatewayPaymentMethodT,
--     MerchantIframePreferencesT,
--     MerchantKeyT,
--     MerchantLockerAccountT,
--     MerchantProviderDetailsT,
--     MerchantRiskSettingsT,
--     MetadataT,
--     NetworkCardFingerprintT,
--     NotificationT,
--     OfferBenefitInfoT,
--     OfferRedemptionT,
--     OffersT,
--     OrderAddressT,
--     OrderBasketT,
--     OrderMetadataV2T,
--     OrderReferenceT,
--     PaymentFormT,
--     PaymentGatewayResponseT,
--     PaymentGatewayResponseV1T,
--     PaymentLinksT,
--     PaymentMethodT,
--     ProcessTrackerT,
--     PromotionT,
--     ProviderT,
--     RefundT,
--     ResellerAccountT,
--     RiskManagementAccountT,
--     RoleT,
--     RuleT,
--     SavedPaymentMethodT,
--     SecondFactorResponseT,
--     SecondFactorT,
--     StoredCardT,
--     TempCardT,
--     TokenBinInfoT,
--     TokenCustomerT,
--     TokenRequestorT,
--     TokenT,
--     TxnCardInfoT,
--     TxnDetailT,
--     TxnIntentDetailT,
--     TxnOfferDetailT,
--     TxnOfferInfoT,
--     TxnOfferT,
--     TxnRiskCheckT,
--     UnifiedGatewayResponseT,
--     UserRoleT,
--     UserT,
--     WalletAccountT,
--     WalletTopUpTxnT,
--   )
import EulerHS.Prelude
import Sequelize
import qualified "dynamic-offer-driver-app" Storage.Beam.Booking as Booking
import qualified "dynamic-offer-driver-app" Storage.Beam.Booking.BookingLocation as BookingLocation
import qualified "dynamic-offer-driver-app" Storage.Beam.BookingCancellationReason as BookingCancellationReason
import qualified "dynamic-offer-driver-app" Storage.Beam.BusinessEvent as BusinessEvent
import qualified "dynamic-offer-driver-app" Storage.Beam.CallStatus as CallStatus
import qualified "dynamic-offer-driver-app" Storage.Beam.CancellationReason as CancellationReason
import qualified "dynamic-offer-driver-app" Storage.Beam.Driver.DriverFlowStatus as DriverFlowStatus
import qualified "dynamic-offer-driver-app" Storage.Beam.DriverFee as DriverFee
import qualified "dynamic-offer-driver-app" Storage.Beam.DriverInformation as DriverInformation
import qualified "dynamic-offer-driver-app" Storage.Beam.DriverLocation as DriverLocation
import qualified "dynamic-offer-driver-app" Storage.Beam.DriverOnboarding.AadhaarOtpReq as AadhaarOtpReq
import qualified "dynamic-offer-driver-app" Storage.Beam.DriverOnboarding.AadhaarOtpVerify as AadhaarOtpVerify
import qualified "dynamic-offer-driver-app" Storage.Beam.DriverOnboarding.AadhaarVerification as AadhaarVerification
import qualified "dynamic-offer-driver-app" Storage.Beam.DriverOnboarding.DriverLicense as DriverLicense
import qualified "dynamic-offer-driver-app" Storage.Beam.DriverOnboarding.DriverRCAssociation as DriverRCAssociation
import qualified "dynamic-offer-driver-app" Storage.Beam.DriverOnboarding.IdfyVerification as IdfyVerification
import qualified "dynamic-offer-driver-app" Storage.Beam.DriverOnboarding.Image as Image
import qualified "dynamic-offer-driver-app" Storage.Beam.DriverOnboarding.OperatingCity as OperatingCity
import qualified "dynamic-offer-driver-app" Storage.Beam.DriverOnboarding.VehicleRegistrationCertificate as VehicleRegistrationCertificate
import qualified "dynamic-offer-driver-app" Storage.Beam.DriverQuote as DriverQuote
import qualified "dynamic-offer-driver-app" Storage.Beam.DriverReferral as DriverReferral
import qualified "dynamic-offer-driver-app" Storage.Beam.DriverStats as DriverStats
import qualified "dynamic-offer-driver-app" Storage.Beam.Estimate as Estimate
import qualified "dynamic-offer-driver-app" Storage.Beam.Exophone as Exophone
import qualified "dynamic-offer-driver-app" Storage.Beam.FareParameters as FareParameters
import qualified "dynamic-offer-driver-app" Storage.Beam.FareParameters.FareParametersProgressiveDetails as FareParametersProgressiveDetails
import qualified "dynamic-offer-driver-app" Storage.Beam.FareParameters.FareParametersSlabDetails as FareParametersSlabDetails
import qualified "dynamic-offer-driver-app" Storage.Beam.FarePolicy as FarePolicy
import qualified "dynamic-offer-driver-app" Storage.Beam.FarePolicy.DriverExtraFeeBounds as DriverExtraFeeBounds
import qualified "dynamic-offer-driver-app" Storage.Beam.FarePolicy.FarePolicyProgressiveDetails as FarePolicyProgressiveDetails
import qualified "dynamic-offer-driver-app" Storage.Beam.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraKmRateSection as FarePolicyProgressiveDetailsPerExtraKmRateSection
import qualified "dynamic-offer-driver-app" Storage.Beam.FarePolicy.FarePolicySlabDetails.FarePolicySlabDetailsSlab as FarePolicySlabDetailsSlab
import qualified "dynamic-offer-driver-app" Storage.Beam.FarePolicy.RestrictedExtraFare as RestrictedExtraFare
import qualified "dynamic-offer-driver-app" Storage.Beam.FareProduct as FareProduct
import qualified "dynamic-offer-driver-app" Storage.Beam.Feedback.Feedback as Feedback
import qualified "dynamic-offer-driver-app" Storage.Beam.Feedback.FeedbackBadge as FeedbackBadge
import qualified "dynamic-offer-driver-app" Storage.Beam.Feedback.FeedbackForm as FeedbackForm
import qualified "dynamic-offer-driver-app" Storage.Beam.Geometry as Geometry
import qualified "dynamic-offer-driver-app" Storage.Beam.Issue.Comment as Comment
import qualified "dynamic-offer-driver-app" Storage.Beam.Issue.IssueCategory as IssueCategory
import qualified "dynamic-offer-driver-app" Storage.Beam.Issue.IssueOption as IssueOption
import qualified "dynamic-offer-driver-app" Storage.Beam.Issue.IssueReport as IssueReport
import qualified "dynamic-offer-driver-app" Storage.Beam.Issue.IssueTranslation as IssueTranslation
import qualified "dynamic-offer-driver-app" Storage.Beam.LeaderBoardConfig as LeaderBoardConfig
import qualified "dynamic-offer-driver-app" Storage.Beam.Maps.PlaceNameCache as PlaceNameCache
import qualified "dynamic-offer-driver-app" Storage.Beam.MediaFile as MediaFile
import qualified "dynamic-offer-driver-app" Storage.Beam.Merchant as Merchant
import qualified "dynamic-offer-driver-app" Storage.Beam.Merchant.DriverIntelligentPoolConfig as DriverIntelligentPoolConfig
import qualified "dynamic-offer-driver-app" Storage.Beam.Merchant.DriverPoolConfig as DriverPoolConfig
import qualified "dynamic-offer-driver-app" Storage.Beam.Merchant.LeaderBoardConfig as MerchantLeaderBoardConfig
import qualified "dynamic-offer-driver-app" Storage.Beam.Merchant.MerchantMessage as MerchantMessage
import qualified "dynamic-offer-driver-app" Storage.Beam.Merchant.MerchantPaymentMethod as MerchantPaymentMethod
import qualified "dynamic-offer-driver-app" Storage.Beam.Merchant.MerchantServiceConfig as MerchantServiceConfig
import qualified "dynamic-offer-driver-app" Storage.Beam.Merchant.MerchantServiceUsageConfig as MerchantServiceUsageConfig
import qualified "dynamic-offer-driver-app" Storage.Beam.Merchant.OnboardingDocumentConfig as MerchantOnboardingDocumentConfig
import qualified "dynamic-offer-driver-app" Storage.Beam.Merchant.TransporterConfig as TransporterConfig
import qualified "dynamic-offer-driver-app" Storage.Beam.Message.Message as Message
import qualified "dynamic-offer-driver-app" Storage.Beam.Message.MessageReport as MessageReport
import qualified "dynamic-offer-driver-app" Storage.Beam.Message.MessageTranslation as MessageTranslation
import qualified "dynamic-offer-driver-app" Storage.Beam.OnboardingDocumentConfig as OnboardingDocumentConfig
import qualified "dynamic-offer-driver-app" Storage.Beam.Person as Person
import qualified "dynamic-offer-driver-app" Storage.Beam.QuoteSpecialZone as QuoteSpecialZone
import qualified "dynamic-offer-driver-app" Storage.Beam.Rating as Rating
import qualified "dynamic-offer-driver-app" Storage.Beam.RegistrationToken as RegistrationToken
import qualified "dynamic-offer-driver-app" Storage.Beam.Ride.Table as Ride
import qualified "dynamic-offer-driver-app" Storage.Beam.RideDetails as RideDetails
import qualified "dynamic-offer-driver-app" Storage.Beam.RiderDetails as RiderDetails
import qualified "dynamic-offer-driver-app" Storage.Beam.SearchRequest as SearchRequest
import qualified "dynamic-offer-driver-app" Storage.Beam.SearchRequest.SearchReqLocation as SearchReqLocation
import qualified "dynamic-offer-driver-app" Storage.Beam.SearchRequestForDriver as SearchRequestForDriver
import qualified "dynamic-offer-driver-app" Storage.Beam.SearchRequestSpecialZone as SearchRequestSpecialZone
import qualified "dynamic-offer-driver-app" Storage.Beam.SearchTry as SearchTry
import qualified "dynamic-offer-driver-app" Storage.Beam.Vehicle as Vehicle
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
  = RegistrationTokenDelete
  | BookingDelete
  | BookingLocationDelete
  | BookingCancellationReasonDelete
  | BusinessEventDelete
  | CallStatusDelete
  | CancellationReasonDelete
  | DriverFlowStatusDelete
  | DriverFeeDelete
  | DriverInformationDelete
  | DriverLocationDelete
  | AadhaarOtpReqDelete
  | AadhaarOtpVerifyDelete
  | AadhaarVerificationDelete
  | DriverLicenseDelete
  | DriverRCAssociationDelete
  | IdfyVerificationDelete
  | ImageDelete
  | OperatingCityDelete
  | VehicleRegistrationCertificateDelete
  | DriverQuoteDelete
  | DriverReferralDelete
  | DriverStatsDelete
  | EstimateDelete
  | ExophoneDelete
  | FareParametersDelete
  | FareParametersProgressiveDetailsDelete
  | FareParametersSlabDetailsDelete
  | FarePolicyDelete
  | DriverExtraFeeBoundsDelete
  | FarePolicyProgressiveDetailsDelete
  | FarePolicyProgressiveDetailsPerExtraKmRateSectionDelete
  | FarePolicySlabDetailsSlabDelete
  | RestrictedExtraFareDelete
  | FareProductDelete
  | GeometryDelete
  | CommentDelete
  | IssueCategoryDelete
  | IssueOptionDelete
  | IssueReportDelete
  | IssueTranslationDelete
  | LeaderBoardConfigDelete
  | PlaceNameCacheDelete
  | MediaFileDelete
  | MerchantDelete
  | DriverIntelligentPoolConfigDelete
  | DriverPoolConfigDelete
  | MerchantLeaderBoardConfigDelete
  | MerchantMessageDelete
  | MerchantPaymentMethodDelete
  | MerchantServiceConfigDelete
  | MerchantServiceUsageConfigDelete
  | MerchantOnboardingDocumentConfigDelete
  | TransporterConfigDelete
  | MessageDelete
  | MessageReportDelete
  | MessageTranslationDelete
  | OnboardingDocumentConfigDelete
  | PersonDelete
  | QuoteSpecialZoneDelete
  | RatingDelete
  | RideDelete
  | RideDetailsDelete
  | RiderDetailsDelete
  | SearchRequestDelete
  | SearchReqLocationDelete
  | SearchRequestForDriverDelete
  | SearchRequestSpecialZoneDelete
  | SearchTryDelete
  | VehicleDelete
  | FeedbackFormDelete
  | FeedbackDelete
  | FeedbackBadgeDelete
  deriving (Generic, Show)

-- getTagDelete :: DeleteModel -> Text
-- getTagDelete TxnOfferInfoDelete = "TxnOfferInfoOptions"
-- getTagDelete TestTableDelete = " TestTableOptions"
-- getTagDelete JuspayEventDelete = "JuspayEventOptions"
-- getTagDelete HdfcHashedNumDelete = "HdfcHashedNumOptions"
-- getTagDelete MerchantGatewayAccountDelete = "MerchantGatewayAccountOptions"
-- getTagDelete BankInfoDelete = "BankInfoOptions"
-- getTagDelete RuleDelete = "RuleOptions"
-- getTagDelete MerchantProviderCofDetailsDelete = "MerchantProviderCofDetailsOptions"
-- getTagDelete MerchantCustomerCofDetailsDelete = "MerchantCustomerCofDetailsOptions"
-- getTagDelete MerchantGatewayAccountSubInfoDelete = "MerchantGatewayAccountSubInfoOptions"
-- getTagDelete OrderReferenceDelete = "OrderReferenceOptions"
-- getTagDelete MandateDelete = "MandateOptions"
-- getTagDelete GatewayTxnDataDelete = "GatewayTxnDataOptions"
-- getTagDelete MerchantGatewayCardInfoDelete = "MerchantGatewayCardInfoOptions"
-- getTagDelete TxnRiskCheckDelete = "TxnRiskCheckOptions"
-- getTagDelete PaymentMethodDelete = "PaymentMethodOptions"
-- getTagDelete OfferBenefitInfoDelete = "OfferBenefitInfoOptions"
-- getTagDelete MerchantIframePreferencesDelete = "MerchantIframePreferencesOptions"
-- getTagDelete ResellerAccountDelete = "ResellerAccountOptions"
-- getTagDelete UnifiedGatewayResponseDelete = "UnifiedGatewayResponseOptions"
-- getTagDelete WalletTopUpTxnDelete = "WalletTopUpTxnOptions"
-- getTagDelete IsinRoutesDelete = "IsinRoutesOptions"
-- getTagDelete TxnCardInfoDelete = "TxnCardInfoOptions"
-- getTagDelete OrderAddressDelete = "OrderAddressOptions"
-- getTagDelete LockerAccountDelete = "LockerAccountOptions"
-- getTagDelete JuspayBankCodeDelete = "JuspayBankCodeOptions"
-- getTagDelete IssuerRoutesDelete = "IssuerRoutesOptions"
-- getTagDelete MerchantPriorityLogicDelete = "MerchantPriorityLogicOptions"
-- getTagDelete RefundDelete = "RefundOptions"
-- getTagDelete SecondFactorDelete = "SecondFactorOptions"
-- getTagDelete GatewayOutageDelete = "GatewayOutageOptions"
-- getTagDelete JuspayErrorMapDelete = "JuspayErrorMapOptions"
-- getTagDelete TxnDetailDelete = "TxnDetailOptions"
-- getTagDelete EmiPlanDelete = "EmiPlanOptions"
-- getTagDelete MerchantKeyDelete = "MerchantKeyOptions"
-- getTagDelete NetworkCardFingerprintDelete = "NetworkCardFingerprintOptions"
-- getTagDelete TokenRequestorDelete = "TokenRequestorOptions"
-- getTagDelete TxnOfferDetailDelete = "TxnOfferDetailOptions"
-- getTagDelete SecondFactorResponseDelete = "SecondFactorResponseOptions"
-- getTagDelete EncryptionKeysDelete = "EncryptionKeysOptions"
-- getTagDelete OfferRedemptionDelete = "OfferRedemptionOptions"
-- getTagDelete CardBrandRoutesDelete = "CardBrandRoutesOptions"
-- getTagDelete CustomerDelete = "CustomerOptions"
-- getTagDelete MerchantAccountDelete = "MerchantAccountOptions"
-- getTagDelete ExternalMerchantCustomerDelete = "ExternalMerchantCustomerOptions"
-- getTagDelete TokenBinInfoDelete = "TokenBinInfoOptions"
-- getTagDelete MerchantGatewayPaymentMethodDelete = "MerchantGatewayPaymentMethodOptions"
-- getTagDelete PromotionDelete = "PromotionOptions"
-- getTagDelete LockerTokenRequestorDelete = "LockerTokenRequestorOptions"
-- getTagDelete BankAccountDelete = "BankAccountOptions"
-- getTagDelete AgencyDelete = "AgencyOptions"
-- getTagDelete ProviderDelete = "ProviderOptions"
-- getTagDelete GatewayCardInfoDelete = "GatewayCardInfoOptions"
-- getTagDelete PaymentGatewayResponseDelete = "PaymentGatewayResponseOptions"
-- getTagDelete MetadataDelete = "MetadataOptions"
-- getTagDelete GatewayTxnStatusMapDelete = "GatewayTxnStatusMapOptions"
-- getTagDelete ChargebackDelete = "ChargebackOptions"
-- getTagDelete WalletAccountDelete = "WalletAccountOptions"
-- getTagDelete GatewayStatusMapDelete = "GatewayStatusMapOptions"
-- getTagDelete TokenDelete = "TokenOptions"
-- getTagDelete MerchantLockerAccountDelete = "MerchantLockerAccountOptions"
-- getTagDelete TempCardDelete = "TempCardOptions"
-- getTagDelete MerchantRiskSettingsDelete = "MerchantRiskSettingsOptions"
-- getTagDelete UserDelete = "UserOptions"
-- getTagDelete CofDetailsDelete = "CofDetailsOptions"
-- getTagDelete JuspayIssuerNameMappingDelete = "JuspayIssuerNameMappingOptions"
-- getTagDelete ServiceConfigurationDelete = "ServiceConfigurationOptions"
-- getTagDelete OrderMetadataV2Delete = "OrderMetadataV2Options"
-- getTagDelete StoredCardDelete = "StoredCardOptions"
-- getTagDelete TokenCustomerDelete = "TokenCustomerOptions"
-- getTagDelete EnrolledPanDelete = "EnrolledPanOptions"
-- getTagDelete OffersDelete = "OffersOptions"
-- getTagDelete PaymentLinksDelete = "PaymentLinksOptions"
-- getTagDelete RoleDelete = "RoleOptions"
-- getTagDelete FeatureDelete = "FeatureOptions"
-- getTagDelete GatewayBankEmiSupportDelete = "GatewayBankEmiSupportOptions"
-- getTagDelete AuthenticationAccountDelete = "AuthenticationAccountOptions"
-- getTagDelete PaymentGatewayResponseV1Delete = "PaymentGatewayResponseV1Options"
-- getTagDelete SavedPaymentMethodDelete = "SavedPaymentMethodOptions"
-- getTagDelete MerchantProviderDetailsDelete = "MerchantProviderDetailsOptions"
-- getTagDelete TxnOfferDelete = "TxnOfferOptions"
-- getTagDelete GatewayHealthDelete = "GatewayHealthOptions"
-- getTagDelete RiskManagementAccountDelete = "RiskManagementAccountOptions"
-- getTagDelete CardInfoDelete = "CardInfoOptions"
-- getTagDelete DeviceBindingDelete = "DeviceBindingOptions"
-- getTagDelete NotificationDelete = "NotificationOptions"
-- getTagDelete OrderBasketDelete = "OrderBasketOptions"
-- getTagDelete GatewayPaymentMethodDelete = "GatewayPaymentMethodOptions"
-- getTagDelete ProcessTrackerDelete = "ProcessTrackerOptions"
-- getTagDelete ConfigurationsDelete = "ConfigurationsOptions"
-- getTagDelete CustomerAccountDelete = "CustomerAccountOptions"
-- getTagDelete EntityMapDelete = "EntityMapOptions"
-- getTagDelete FormInputDelete = "FormInputOptions"
-- getTagDelete IngressRuleDelete = "IngressRuleOptions"
-- getTagDelete InstallmentDelete = "InstallmentOptions"
-- getTagDelete InstallmentRefundDelete = "InstallmentRefundOptions"
-- getTagDelete PaymentFormDelete = "PaymentFormOptions"
-- getTagDelete UserRoleDelete = "UserRoleOptions"
-- getTagDelete TxnIntentDetailDelete = "TxnIntentDetailOptions"
-- getTagDelete AuthMappingDelete = "AuthMappingOptions"

getTagDelete :: DeleteModel -> Text
getTagDelete RegistrationTokenDelete = "RegistrationTokenOptions"
getTagDelete BookingDelete = "BookingOptions"
getTagDelete BookingLocationDelete = "BookingLocationOptions"
getTagDelete BookingCancellationReasonDelete = "BookingCancellationReasonOptions"
getTagDelete BusinessEventDelete = "BusinessEventOptions"
getTagDelete CallStatusDelete = "CallStatusOptions"
getTagDelete CancellationReasonDelete = "CancellationReasonOptions"
getTagDelete DriverFlowStatusDelete = "DriverFlowStatusOptions"
getTagDelete DriverFeeDelete = "DriverFeeOptions"
getTagDelete DriverInformationDelete = "DriverInformationOptions"
getTagDelete DriverLocationDelete = "DriverLocationOptions"
getTagDelete AadhaarOtpReqDelete = "AadhaarOtpReqOptions"
getTagDelete AadhaarOtpVerifyDelete = "AadhaarOtpVerifyOptions"
getTagDelete AadhaarVerificationDelete = "AadhaarVerificationOptions"
getTagDelete DriverLicenseDelete = "DriverLicenseOptions"
getTagDelete DriverRCAssociationDelete = "DriverRCAssociationOptions"
getTagDelete IdfyVerificationDelete = "IdfyVerificationOptions"
getTagDelete ImageDelete = "ImageOptions"
getTagDelete OperatingCityDelete = "OperatingCityOptions"
getTagDelete VehicleRegistrationCertificateDelete = "VehicleRegistrationCertificateOptions"
getTagDelete DriverQuoteDelete = "DriverQuoteOptions"
getTagDelete DriverReferralDelete = "DriverReferralOptions"
getTagDelete DriverStatsDelete = "DriverStatsOptions"
getTagDelete EstimateDelete = "EstimateOptions"
getTagDelete ExophoneDelete = "ExophoneOptions"
getTagDelete FareParametersDelete = "FareParametersOptions"
getTagDelete FareParametersProgressiveDetailsDelete = "FareParametersProgressiveDetailsOptions"
getTagDelete FareParametersSlabDetailsDelete = "FareParametersSlabDetailsOptions"
getTagDelete FarePolicyDelete = "FarePolicyOptions"
getTagDelete DriverExtraFeeBoundsDelete = "DriverExtraFeeBoundsOptions"
getTagDelete FarePolicyProgressiveDetailsDelete = "FarePolicyProgressiveDetailsOptions"
getTagDelete FarePolicyProgressiveDetailsPerExtraKmRateSectionDelete = "FarePolicyProgressiveDetailsPerExtraKmRateSectionOptions"
getTagDelete FarePolicySlabDetailsSlabDelete = "FarePolicySlabDetailsSlabOptions"
getTagDelete RestrictedExtraFareDelete = "RestrictedExtraFareOptions"
getTagDelete FareProductDelete = "FareProductOptions"
getTagDelete GeometryDelete = "GeometryOptions"
getTagDelete CommentDelete = "CommentOptions"
getTagDelete IssueCategoryDelete = "IssueCategoryOptions"
getTagDelete IssueOptionDelete = "IssueOptionOptions"
getTagDelete IssueReportDelete = "IssueReportOptions"
getTagDelete IssueTranslationDelete = "IssueTranslationOptions"
getTagDelete LeaderBoardConfigDelete = "LeaderBoardConfigOptions"
getTagDelete PlaceNameCacheDelete = "PlaceNameCacheOptions"
getTagDelete MediaFileDelete = "MediaFileOptions"
getTagDelete MerchantDelete = "MerchantOptions"
getTagDelete DriverIntelligentPoolConfigDelete = "DriverIntelligentPoolConfigOptions"
getTagDelete DriverPoolConfigDelete = "DriverPoolConfigOptions"
getTagDelete MerchantLeaderBoardConfigDelete = "MerchantLeaderBoardConfigOptions"
getTagDelete MerchantMessageDelete = "MerchantMessageOptions"
getTagDelete MerchantPaymentMethodDelete = "MerchantPaymentMethodOptions"
getTagDelete MerchantServiceConfigDelete = "MerchantServiceConfigOptions"
getTagDelete MerchantServiceUsageConfigDelete = "MerchantServiceUsageConfigOptions"
getTagDelete MerchantOnboardingDocumentConfigDelete = "MerchantOnboardingDocumentConfigOptions"
getTagDelete TransporterConfigDelete = "TransporterConfigOptions"
getTagDelete MessageDelete = "MessageOptions"
getTagDelete MessageReportDelete = "MessageReportOptions"
getTagDelete MessageTranslationDelete = "MessageTranslationOptions"
getTagDelete OnboardingDocumentConfigDelete = "OnboardingDocumentConfigOptions"
getTagDelete PersonDelete = "PersonOptions"
getTagDelete QuoteSpecialZoneDelete = "QuoteSpecialZoneOptions"
getTagDelete RatingDelete = "RatingOptions"
getTagDelete RideDelete = "RideOptions"
getTagDelete RideDetailsDelete = "RideDetailsOptions"
getTagDelete RiderDetailsDelete = "RiderDetailsOptions"
getTagDelete SearchRequestDelete = "SearchRequestOptions"
getTagDelete SearchReqLocationDelete = "SearchReqLocationOptions"
getTagDelete SearchRequestForDriverDelete = "SearchRequestForDriverOptions"
getTagDelete SearchRequestSpecialZoneDelete = "SearchRequestSpecialZoneOptions"
getTagDelete SearchTryDelete = "SearchTryOptions"
getTagDelete VehicleDelete = "VehicleOptions"
getTagDelete FeedbackFormDelete = "FeedbackFormOptions"
getTagDelete FeedbackDelete = "FeedbackOptions"
getTagDelete FeedbackBadgeDelete = "FeedbackBadgeOptions"

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
parseTagDelete "RegistrationTokenOptions" = return RegistrationTokenDelete
parseTagDelete "BookingOptions" = return BookingDelete
parseTagDelete "BookingLocationOptions" = return BookingLocationDelete
parseTagDelete "BookingCancellationReasonOptions" = return BookingCancellationReasonDelete
parseTagDelete "BusinessEventOptions" = return BusinessEventDelete
parseTagDelete "CallStatusOptions" = return CallStatusDelete
parseTagDelete "CancellationReasonOptions" = return CancellationReasonDelete
parseTagDelete "DriverFlowStatusOptions" = return DriverFlowStatusDelete
parseTagDelete "DriverFeeOptions" = return DriverFeeDelete
parseTagDelete "DriverInformationOptions" = return DriverInformationDelete
parseTagDelete "DriverLocationOptions" = return DriverLocationDelete
parseTagDelete "AadhaarOtpReqOptions" = return AadhaarOtpReqDelete
parseTagDelete "AadhaarOtpVerifyOptions" = return AadhaarOtpVerifyDelete
parseTagDelete "AadhaarVerificationOptions" = return AadhaarVerificationDelete
parseTagDelete "DriverLicenseOptions" = return DriverLicenseDelete
parseTagDelete "DriverRCAssociationOptions" = return DriverRCAssociationDelete
parseTagDelete "IdfyVerificationOptions" = return IdfyVerificationDelete
parseTagDelete "ImageOptions" = return ImageDelete
parseTagDelete "OperatingCityOptions" = return OperatingCityDelete
parseTagDelete "VehicleRegistrationCertificateOptions" = return VehicleRegistrationCertificateDelete
parseTagDelete "DriverQuoteOptions" = return DriverQuoteDelete
parseTagDelete "DriverReferralOptions" = return DriverReferralDelete
parseTagDelete "DriverStatsOptions" = return DriverStatsDelete
parseTagDelete "EstimateOptions" = return EstimateDelete
parseTagDelete "ExophoneOptions" = return ExophoneDelete
parseTagDelete "FareParametersOptions" = return FareParametersDelete
parseTagDelete "FareParametersProgressiveDetailsOptions" = return FareParametersProgressiveDetailsDelete
parseTagDelete "FareParametersSlabDetailsOptions" = return FareParametersSlabDetailsDelete
parseTagDelete "FarePolicyOptions" = return FarePolicyDelete
parseTagDelete "DriverExtraFeeBoundsOptions" = return DriverExtraFeeBoundsDelete
parseTagDelete "FarePolicyProgressiveDetailsOptions" = return FarePolicyProgressiveDetailsDelete
parseTagDelete "FarePolicyProgressiveDetailsPerExtraKmRateSectionOptions" = return FarePolicyProgressiveDetailsPerExtraKmRateSectionDelete
parseTagDelete "FarePolicySlabDetailsSlabOptions" = return FarePolicySlabDetailsSlabDelete
parseTagDelete "RestrictedExtraFareOptions" = return RestrictedExtraFareDelete
parseTagDelete "FareProductOptions" = return FareProductDelete
parseTagDelete "GeometryOptions" = return GeometryDelete
parseTagDelete "CommentOptions" = return CommentDelete
parseTagDelete "IssueCategoryOptions" = return IssueCategoryDelete
parseTagDelete "IssueOptionOptions" = return IssueOptionDelete
parseTagDelete "IssueReportOptions" = return IssueReportDelete
parseTagDelete "IssueTranslationOptions" = return IssueTranslationDelete
parseTagDelete "LeaderBoardConfigOptions" = return LeaderBoardConfigDelete
parseTagDelete "PlaceNameCacheOptions" = return PlaceNameCacheDelete
parseTagDelete "MediaFileOptions" = return MediaFileDelete
parseTagDelete "MerchantOptions" = return MerchantDelete
parseTagDelete "DriverIntelligentPoolConfigOptions" = return DriverIntelligentPoolConfigDelete
parseTagDelete "DriverPoolConfigOptions" = return DriverPoolConfigDelete
parseTagDelete "MerchantLeaderBoardConfigOptions" = return MerchantLeaderBoardConfigDelete
parseTagDelete "MerchantMessageOptions" = return MerchantMessageDelete
parseTagDelete "MerchantPaymentMethodOptions" = return MerchantPaymentMethodDelete
parseTagDelete "MerchantServiceConfigOptions" = return MerchantServiceConfigDelete
parseTagDelete "MerchantServiceUsageConfigOptions" = return MerchantServiceUsageConfigDelete
parseTagDelete "MerchantOnboardingDocumentConfigOptions" = return MerchantOnboardingDocumentConfigDelete
parseTagDelete "TransporterConfigOptions" = return TransporterConfigDelete
parseTagDelete "MessageOptions" = return MessageDelete
parseTagDelete "MessageReportOptions" = return MessageReportDelete
parseTagDelete "MessageTranslationOptions" = return MessageTranslationDelete
parseTagDelete "OnboardingDocumentConfigOptions" = return OnboardingDocumentConfigDelete
parseTagDelete "PersonOptions" = return PersonDelete
parseTagDelete "QuoteSpecialZoneOptions" = return QuoteSpecialZoneDelete
parseTagDelete "RatingOptions" = return RatingDelete
parseTagDelete "RideOptions" = return RideDelete
parseTagDelete "RideDetailsOptions" = return RideDetailsDelete
parseTagDelete "RiderDetailsOptions" = return RiderDetailsDelete
parseTagDelete "SearchRequestOptions" = return SearchRequestDelete
parseTagDelete "SearchReqLocationOptions" = return SearchReqLocationDelete
parseTagDelete "SearchRequestForDriverOptions" = return SearchRequestForDriverDelete
parseTagDelete "SearchRequestSpecialZoneOptions" = return SearchRequestSpecialZoneDelete
parseTagDelete "SearchTryOptions" = return SearchTryDelete
parseTagDelete "VehicleOptions" = return VehicleDelete
parseTagDelete "FeedbackFormOptions" = return FeedbackFormDelete
parseTagDelete "FeedbackOptions" = return FeedbackDelete
parseTagDelete "FeedbackBadgeOptions" = return FeedbackBadgeDelete
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
  = RegistrationTokenDeleteOptions DeleteModel (Where Postgres RegistrationToken.RegistrationTokenT)
  | BookingDeleteOptions DeleteModel (Where Postgres Booking.BookingT)
  | BookingLocationDeleteOptions DeleteModel (Where Postgres BookingLocation.BookingLocationT)
  | BookingCancellationReasonDeleteOptions DeleteModel (Where Postgres BookingCancellationReason.BookingCancellationReasonT)
  | BusinessEventDeleteOptions DeleteModel (Where Postgres BusinessEvent.BusinessEventT)
  | CallStatusDeleteOptions DeleteModel (Where Postgres CallStatus.CallStatusT)
  | CancellationReasonDeleteOptions DeleteModel (Where Postgres CancellationReason.CancellationReasonT)
  | DriverFlowStatusDeleteOptions DeleteModel (Where Postgres DriverFlowStatus.DriverFlowStatusT)
  | DriverFeeDeleteOptions DeleteModel (Where Postgres DriverFee.DriverFeeT)
  | DriverInformationDeleteOptions DeleteModel (Where Postgres DriverInformation.DriverInformationT)
  | DriverLocationDeleteOptions DeleteModel (Where Postgres DriverLocation.DriverLocationT)
  | AadhaarOtpReqDeleteOptions DeleteModel (Where Postgres AadhaarOtpReq.AadhaarOtpReqT)
  | AadhaarOtpVerifyDeleteOptions DeleteModel (Where Postgres AadhaarOtpVerify.AadhaarOtpVerifyT)
  | AadhaarVerificationDeleteOptions DeleteModel (Where Postgres AadhaarVerification.AadhaarVerificationT)
  | DriverLicenseDeleteOptions DeleteModel (Where Postgres DriverLicense.DriverLicenseT)
  | DriverRCAssociationDeleteOptions DeleteModel (Where Postgres DriverRCAssociation.DriverRCAssociationT)
  | IdfyVerificationDeleteOptions DeleteModel (Where Postgres IdfyVerification.IdfyVerificationT)
  | ImageDeleteOptions DeleteModel (Where Postgres Image.ImageT)
  | OperatingCityDeleteOptions DeleteModel (Where Postgres OperatingCity.OperatingCityT)
  | VehicleRegistrationCertificateDeleteOptions DeleteModel (Where Postgres VehicleRegistrationCertificate.VehicleRegistrationCertificateT)
  | DriverQuoteDeleteOptions DeleteModel (Where Postgres DriverQuote.DriverQuoteT)
  | DriverReferralDeleteOptions DeleteModel (Where Postgres DriverReferral.DriverReferralT)
  | DriverStatsDeleteOptions DeleteModel (Where Postgres DriverStats.DriverStatsT)
  | EstimateDeleteOptions DeleteModel (Where Postgres Estimate.EstimateT)
  | ExophoneDeleteOptions DeleteModel (Where Postgres Exophone.ExophoneT)
  | FareParametersDeleteOptions DeleteModel (Where Postgres FareParameters.FareParametersT)
  | FareParametersProgressiveDetailsDeleteOptions DeleteModel (Where Postgres FareParametersProgressiveDetails.FareParametersProgressiveDetailsT)
  | FareParametersSlabDetailsDeleteOptions DeleteModel (Where Postgres FareParametersSlabDetails.FareParametersSlabDetailsT)
  | FarePolicyDeleteOptions DeleteModel (Where Postgres FarePolicy.FarePolicyT)
  | DriverExtraFeeBoundsDeleteOptions DeleteModel (Where Postgres DriverExtraFeeBounds.DriverExtraFeeBoundsT)
  | FarePolicyProgressiveDetailsDeleteOptions DeleteModel (Where Postgres FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsT)
  | FarePolicyProgressiveDetailsPerExtraKmRateSectionDeleteOptions DeleteModel (Where Postgres FarePolicyProgressiveDetailsPerExtraKmRateSection.FarePolicyProgressiveDetailsPerExtraKmRateSectionT)
  | FarePolicySlabDetailsSlabDeleteOptions DeleteModel (Where Postgres FarePolicySlabDetailsSlab.FarePolicySlabsDetailsSlabT)
  | RestrictedExtraFareDeleteOptions DeleteModel (Where Postgres RestrictedExtraFare.RestrictedExtraFareT)
  | FareProductDeleteOptions DeleteModel (Where Postgres FareProduct.FareProductT)
  | GeometryDeleteOptions DeleteModel (Where Postgres Geometry.GeometryT)
  | CommentDeleteOptions DeleteModel (Where Postgres Comment.CommentT)
  | IssueCategoryDeleteOptions DeleteModel (Where Postgres IssueCategory.IssueCategoryT)
  | IssueOptionDeleteOptions DeleteModel (Where Postgres IssueOption.IssueOptionT)
  | IssueReportDeleteOptions DeleteModel (Where Postgres IssueReport.IssueReportT)
  | IssueTranslationDeleteOptions DeleteModel (Where Postgres IssueTranslation.IssueTranslationT)
  | LeaderBoardConfigDeleteOptions DeleteModel (Where Postgres LeaderBoardConfig.LeaderBoardConfigsT)
  | PlaceNameCacheDeleteOptions DeleteModel (Where Postgres PlaceNameCache.PlaceNameCacheT)
  | MediaFileDeleteOptions DeleteModel (Where Postgres MediaFile.MediaFileT)
  | MerchantDeleteOptions DeleteModel (Where Postgres Merchant.MerchantT)
  | DriverIntelligentPoolConfigDeleteOptions DeleteModel (Where Postgres DriverIntelligentPoolConfig.DriverIntelligentPoolConfigT)
  | DriverPoolConfigDeleteOptions DeleteModel (Where Postgres DriverPoolConfig.DriverPoolConfigT)
  | MerchantLeaderBoardConfigDeleteOptions DeleteModel (Where Postgres MerchantLeaderBoardConfig.LeaderBoardConfigsT)
  | MerchantMessageDeleteOptions DeleteModel (Where Postgres MerchantMessage.MerchantMessageT)
  | MerchantPaymentMethodDeleteOptions DeleteModel (Where Postgres MerchantPaymentMethod.MerchantPaymentMethodT)
  | MerchantServiceConfigDeleteOptions DeleteModel (Where Postgres MerchantServiceConfig.MerchantServiceConfigT)
  | MerchantServiceUsageConfigDeleteOptions DeleteModel (Where Postgres MerchantServiceUsageConfig.MerchantServiceUsageConfigT)
  | MerchantOnboardingDocumentConfigDeleteOptions DeleteModel (Where Postgres MerchantOnboardingDocumentConfig.OnboardingDocumentConfigT)
  | TransporterConfigDeleteOptions DeleteModel (Where Postgres TransporterConfig.TransporterConfigT)
  | MessageDeleteOptions DeleteModel (Where Postgres Message.MessageT)
  | MessageReportDeleteOptions DeleteModel (Where Postgres MessageReport.MessageReportT)
  | MessageTranslationDeleteOptions DeleteModel (Where Postgres MessageTranslation.MessageTranslationT)
  | OnboardingDocumentConfigDeleteOptions DeleteModel (Where Postgres OnboardingDocumentConfig.OnboardingDocumentConfigT)
  | PersonDeleteOptions DeleteModel (Where Postgres Person.PersonT)
  | QuoteSpecialZoneDeleteOptions DeleteModel (Where Postgres QuoteSpecialZone.QuoteSpecialZoneT)
  | RatingDeleteOptions DeleteModel (Where Postgres Rating.RatingT)
  | RideDeleteOptions DeleteModel (Where Postgres Ride.RideT)
  | RideDetailsDeleteOptions DeleteModel (Where Postgres RideDetails.RideDetailsT)
  | RiderDetailsDeleteOptions DeleteModel (Where Postgres RiderDetails.RiderDetailsT)
  | SearchRequestDeleteOptions DeleteModel (Where Postgres SearchRequest.SearchRequestT)
  | SearchReqLocationDeleteOptions DeleteModel (Where Postgres SearchReqLocation.SearchReqLocationT)
  | SearchRequestForDriverDeleteOptions DeleteModel (Where Postgres SearchRequestForDriver.SearchRequestForDriverT)
  | SearchRequestSpecialZoneDeleteOptions DeleteModel (Where Postgres SearchRequestSpecialZone.SearchRequestSpecialZoneT)
  | SearchTryDeleteOptions DeleteModel (Where Postgres SearchTry.SearchTryT)
  | VehicleDeleteOptions DeleteModel (Where Postgres Vehicle.VehicleT)
  | FeedbackFormDeleteOptions DeleteModel (Where Postgres FeedbackForm.FeedbackFormT)
  | FeedbackDeleteOptions DeleteModel (Where Postgres Feedback.FeedbackT)
  | FeedbackBadgeDeleteOptions DeleteModel (Where Postgres FeedbackBadge.FeedbackBadgeT)

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
      RegistrationTokenDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ RegistrationTokenDeleteOptions deleteModel whereClause
      BookingDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ BookingDeleteOptions deleteModel whereClause
      BookingLocationDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ BookingLocationDeleteOptions deleteModel whereClause
      BookingCancellationReasonDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ BookingCancellationReasonDeleteOptions deleteModel whereClause
      BusinessEventDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ BusinessEventDeleteOptions deleteModel whereClause
      CallStatusDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ CallStatusDeleteOptions deleteModel whereClause
      CancellationReasonDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ CancellationReasonDeleteOptions deleteModel whereClause
      DriverFlowStatusDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ DriverFlowStatusDeleteOptions deleteModel whereClause
      DriverFeeDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ DriverFeeDeleteOptions deleteModel whereClause
      DriverInformationDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ DriverInformationDeleteOptions deleteModel whereClause
      DriverLocationDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ DriverLocationDeleteOptions deleteModel whereClause
      AadhaarOtpReqDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ AadhaarOtpReqDeleteOptions deleteModel whereClause
      AadhaarOtpVerifyDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ AadhaarOtpVerifyDeleteOptions deleteModel whereClause
      AadhaarVerificationDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ AadhaarVerificationDeleteOptions deleteModel whereClause
      DriverLicenseDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ DriverLicenseDeleteOptions deleteModel whereClause
      DriverRCAssociationDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ DriverRCAssociationDeleteOptions deleteModel whereClause
      IdfyVerificationDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ IdfyVerificationDeleteOptions deleteModel whereClause
      ImageDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ ImageDeleteOptions deleteModel whereClause
      OperatingCityDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ OperatingCityDeleteOptions deleteModel whereClause
      VehicleRegistrationCertificateDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ VehicleRegistrationCertificateDeleteOptions deleteModel whereClause
      DriverQuoteDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ DriverQuoteDeleteOptions deleteModel whereClause
      DriverReferralDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ DriverReferralDeleteOptions deleteModel whereClause
      DriverStatsDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ DriverStatsDeleteOptions deleteModel whereClause
      EstimateDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ EstimateDeleteOptions deleteModel whereClause
      ExophoneDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ ExophoneDeleteOptions deleteModel whereClause
      FareParametersDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ FareParametersDeleteOptions deleteModel whereClause
      FareParametersProgressiveDetailsDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ FareParametersProgressiveDetailsDeleteOptions deleteModel whereClause
      FareParametersSlabDetailsDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ FareParametersSlabDetailsDeleteOptions deleteModel whereClause
      FarePolicyDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ FarePolicyDeleteOptions deleteModel whereClause
      DriverExtraFeeBoundsDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ DriverExtraFeeBoundsDeleteOptions deleteModel whereClause
      FarePolicyProgressiveDetailsDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ FarePolicyProgressiveDetailsDeleteOptions deleteModel whereClause
      FarePolicyProgressiveDetailsPerExtraKmRateSectionDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ FarePolicyProgressiveDetailsPerExtraKmRateSectionDeleteOptions deleteModel whereClause
      FarePolicySlabDetailsSlabDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ FarePolicySlabDetailsSlabDeleteOptions deleteModel whereClause
      RestrictedExtraFareDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ RestrictedExtraFareDeleteOptions deleteModel whereClause
      FareProductDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ FareProductDeleteOptions deleteModel whereClause
      GeometryDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ GeometryDeleteOptions deleteModel whereClause
      CommentDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ CommentDeleteOptions deleteModel whereClause
      IssueCategoryDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ IssueCategoryDeleteOptions deleteModel whereClause
      IssueOptionDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ IssueOptionDeleteOptions deleteModel whereClause
      IssueReportDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ IssueReportDeleteOptions deleteModel whereClause
      IssueTranslationDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ IssueTranslationDeleteOptions deleteModel whereClause
      LeaderBoardConfigDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ LeaderBoardConfigDeleteOptions deleteModel whereClause
      PlaceNameCacheDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ PlaceNameCacheDeleteOptions deleteModel whereClause
      MediaFileDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MediaFileDeleteOptions deleteModel whereClause
      MerchantDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MerchantDeleteOptions deleteModel whereClause
      DriverIntelligentPoolConfigDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ DriverIntelligentPoolConfigDeleteOptions deleteModel whereClause
      DriverPoolConfigDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ DriverPoolConfigDeleteOptions deleteModel whereClause
      MerchantLeaderBoardConfigDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MerchantLeaderBoardConfigDeleteOptions deleteModel whereClause
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
      MerchantOnboardingDocumentConfigDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MerchantOnboardingDocumentConfigDeleteOptions deleteModel whereClause
      TransporterConfigDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ TransporterConfigDeleteOptions deleteModel whereClause
      MessageDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MessageDeleteOptions deleteModel whereClause
      MessageReportDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MessageReportDeleteOptions deleteModel whereClause
      MessageTranslationDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MessageTranslationDeleteOptions deleteModel whereClause
      OnboardingDocumentConfigDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ OnboardingDocumentConfigDeleteOptions deleteModel whereClause
      PersonDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ PersonDeleteOptions deleteModel whereClause
      QuoteSpecialZoneDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ QuoteSpecialZoneDeleteOptions deleteModel whereClause
      RatingDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ RatingDeleteOptions deleteModel whereClause
      RideDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ RideDeleteOptions deleteModel whereClause
      RideDetailsDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ RideDetailsDeleteOptions deleteModel whereClause
      RiderDetailsDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ RiderDetailsDeleteOptions deleteModel whereClause
      SearchRequestDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ SearchRequestDeleteOptions deleteModel whereClause
      SearchReqLocationDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ SearchReqLocationDeleteOptions deleteModel whereClause
      SearchRequestForDriverDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ SearchRequestForDriverDeleteOptions deleteModel whereClause
      SearchRequestSpecialZoneDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ SearchRequestSpecialZoneDeleteOptions deleteModel whereClause
      SearchTryDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ SearchTryDeleteOptions deleteModel whereClause
      VehicleDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ VehicleDeleteOptions deleteModel whereClause
      FeedbackFormDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ FeedbackFormDeleteOptions deleteModel whereClause
      FeedbackDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ FeedbackDeleteOptions deleteModel whereClause
      FeedbackBadgeDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ FeedbackBadgeDeleteOptions deleteModel whereClause
