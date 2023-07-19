module Types.DBSync.Update where

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
  = BookingUpdate
  | BookingLocationUpdate
  | BookingCancellationReasonUpdate
  | BusinessEventUpdate
  | CallStatusUpdate
  | CancellationReasonUpdate
  | DriverFlowStatusUpdate
  | DriverFeeUpdate
  | DriverInformationUpdate
  | DriverLocationUpdate
  | AadhaarOtpReqUpdate
  | AadhaarOtpVerifyUpdate
  | AadhaarVerificationUpdate
  | DriverLicenseUpdate
  | DriverRCAssociationUpdate
  | IdfyVerificationUpdate
  | ImageUpdate
  | OperatingCityUpdate
  | VehicleRegistrationCertificateUpdate
  | DriverQuoteUpdate
  | DriverReferralUpdate
  | DriverStatsUpdate
  | EstimateUpdate
  | ExophoneUpdate
  | FareParametersUpdate
  | FareParametersProgressiveDetailsUpdate
  | FareParametersSlabDetailsUpdate
  | FarePolicyUpdate
  | DriverExtraFeeBoundsUpdate
  | FarePolicyProgressiveDetailsUpdate
  | FarePolicyProgressiveDetailsPerExtraKmRateSectionUpdate
  | FarePolicySlabDetailsSlabUpdate
  | RestrictedExtraFareUpdate
  | FareProductUpdate
  | GeometryUpdate
  | CommentUpdate
  | IssueCategoryUpdate
  | IssueOptionUpdate
  | IssueReportUpdate
  | IssueTranslationUpdate
  | LeaderBoardConfigUpdate
  | PlaceNameCacheUpdate
  | MediaFileUpdate
  | MerchantUpdate
  | DriverIntelligentPoolConfigUpdate
  | DriverPoolConfigUpdate
  | MerchantLeaderBoardConfigUpdate
  | MerchantMessageUpdate
  | MerchantPaymentMethodUpdate
  | MerchantServiceConfigUpdate
  | MerchantServiceUsageConfigUpdate
  | MerchantOnboardingDocumentConfigUpdate
  | TransporterConfigUpdate
  | MessageUpdate
  | MessageReportUpdate
  | MessageTranslationUpdate
  | OnboardingDocumentConfigUpdate
  | PersonUpdate
  | QuoteSpecialZoneUpdate
  | RatingUpdate
  | RegistrationTokenUpdate
  | RideUpdate
  | RideDetailsUpdate
  | RiderDetailsUpdate
  | SearchRequestUpdate
  | SearchReqLocationUpdate
  | SearchRequestForDriverUpdate
  | SearchRequestSpecialZoneUpdate
  | SearchTryUpdate
  | VehicleUpdate
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
getTagUpdate BookingUpdate = "BookingOptions"
getTagUpdate BookingLocationUpdate = "BookingLocationOptions"
getTagUpdate BookingCancellationReasonUpdate = "BookingCancellationReasonOptions"
getTagUpdate BusinessEventUpdate = "BusinessEventOptions"
getTagUpdate CallStatusUpdate = "CallStatusOptions"
getTagUpdate CancellationReasonUpdate = "CancellationReasonOptions"
getTagUpdate DriverFlowStatusUpdate = "DriverFlowStatusOptions"
getTagUpdate DriverFeeUpdate = "DriverFeeOptions"
getTagUpdate DriverInformationUpdate = "DriverInformationOptions"
getTagUpdate DriverLocationUpdate = "DriverLocationOptions"
getTagUpdate AadhaarOtpReqUpdate = "AadhaarOtpReqOptions"
getTagUpdate AadhaarOtpVerifyUpdate = "AadhaarOtpVerifyOptions"
getTagUpdate AadhaarVerificationUpdate = "AadhaarVerificationOptions"
getTagUpdate DriverLicenseUpdate = "DriverLicenseOptions"
getTagUpdate DriverRCAssociationUpdate = "DriverRCAssociationOptions"
getTagUpdate IdfyVerificationUpdate = "IdfyVerificationOptions"
getTagUpdate ImageUpdate = "ImageOptions"
getTagUpdate OperatingCityUpdate = "OperatingCityOptions"
getTagUpdate VehicleRegistrationCertificateUpdate = "VehicleRegistrationCertificateOptions"
getTagUpdate DriverQuoteUpdate = "DriverQuoteOptions"
getTagUpdate DriverReferralUpdate = "DriverReferralOptions"
getTagUpdate DriverStatsUpdate = "DriverStatsOptions"
getTagUpdate EstimateUpdate = "EstimateOptions"
getTagUpdate ExophoneUpdate = "ExophoneOptions"
getTagUpdate FareParametersUpdate = "FareParametersOptions"
getTagUpdate FareParametersProgressiveDetailsUpdate = "FareParametersProgressiveDetailsOptions"
getTagUpdate FareParametersSlabDetailsUpdate = "FareParametersSlabDetailsOptions"
getTagUpdate FarePolicyUpdate = "FarePolicyOptions"
getTagUpdate DriverExtraFeeBoundsUpdate = "DriverExtraFeeBoundsOptions"
getTagUpdate FarePolicyProgressiveDetailsUpdate = "FarePolicyProgressiveDetailsOptions"
getTagUpdate FarePolicyProgressiveDetailsPerExtraKmRateSectionUpdate = "FarePolicyProgressiveDetailsPerExtraKmRateSectionOptions"
getTagUpdate FarePolicySlabDetailsSlabUpdate = "FarePolicySlabDetailsSlabOptions"
getTagUpdate RestrictedExtraFareUpdate = "RestrictedExtraFareOptions"
getTagUpdate FareProductUpdate = "FareProductOptions"
getTagUpdate GeometryUpdate = "GeometryOptions"
getTagUpdate CommentUpdate = "CommentOptions"
getTagUpdate IssueCategoryUpdate = "IssueCategoryOptions"
getTagUpdate IssueOptionUpdate = "IssueOptionOptions"
getTagUpdate IssueReportUpdate = "IssueReportOptions"
getTagUpdate IssueTranslationUpdate = "IssueTranslationOptions"
getTagUpdate LeaderBoardConfigUpdate = "LeaderBoardConfigOptions"
getTagUpdate PlaceNameCacheUpdate = "PlaceNameCacheOptions"
getTagUpdate MediaFileUpdate = "MediaFileOptions"
getTagUpdate MerchantUpdate = "MerchantOptions"
getTagUpdate DriverIntelligentPoolConfigUpdate = "DriverIntelligentPoolConfigOptions"
getTagUpdate DriverPoolConfigUpdate = "DriverPoolConfigOptions"
getTagUpdate MerchantLeaderBoardConfigUpdate = "MerchantLeaderBoardConfigOptions"
getTagUpdate MerchantMessageUpdate = "MerchantMessageOptions"
getTagUpdate MerchantPaymentMethodUpdate = "MerchantPaymentMethodOptions"
getTagUpdate MerchantServiceConfigUpdate = "MerchantServiceConfigOptions"
getTagUpdate MerchantServiceUsageConfigUpdate = "MerchantServiceUsageConfigOptions"
getTagUpdate MerchantOnboardingDocumentConfigUpdate = "MerchantOnboardingDocumentConfigOptions"
getTagUpdate TransporterConfigUpdate = "TransporterConfigOptions"
getTagUpdate MessageUpdate = "MessageOptions"
getTagUpdate MessageReportUpdate = "MessageReportOptions"
getTagUpdate MessageTranslationUpdate = "MessageTranslationOptions"
getTagUpdate OnboardingDocumentConfigUpdate = "OnboardingDocumentConfigOptions"
getTagUpdate PersonUpdate = "PersonOptions"
getTagUpdate QuoteSpecialZoneUpdate = "QuoteSpecialZoneOptions"
getTagUpdate RatingUpdate = "RatingOptions"
getTagUpdate RegistrationTokenUpdate = "RegistrationTokenOptions"
getTagUpdate RideUpdate = "RideOptions"
getTagUpdate RideDetailsUpdate = "RideDetailsOptions"
getTagUpdate RiderDetailsUpdate = "RiderDetailsOptions"
getTagUpdate SearchRequestUpdate = "SearchRequestOptions"
getTagUpdate SearchReqLocationUpdate = "SearchReqLocationOptions"
getTagUpdate SearchRequestForDriverUpdate = "SearchRequestForDriverOptions"
getTagUpdate SearchRequestSpecialZoneUpdate = "SearchRequestSpecialZoneOptions"
getTagUpdate SearchTryUpdate = "SearchTryOptions"
getTagUpdate VehicleUpdate = "VehicleOptions"

parseTagUpdate :: Text -> Parser UpdateModel
parseTagUpdate "BookingOptions" = return BookingUpdate
parseTagUpdate "BookingLocationOptions" = return BookingLocationUpdate
parseTagUpdate "BookingCancellationReasonOptions" = return BookingCancellationReasonUpdate
parseTagUpdate "BusinessEventOptions" = return BusinessEventUpdate
parseTagUpdate "CallStatusOptions" = return CallStatusUpdate
parseTagUpdate "CancellationReasonOptions" = return CancellationReasonUpdate
parseTagUpdate "DriverFlowStatusOptions" = return DriverFlowStatusUpdate
parseTagUpdate "DriverFeeOptions" = return DriverFeeUpdate
parseTagUpdate "DriverInformationOptions" = return DriverInformationUpdate
parseTagUpdate "DriverLocationOptions" = return DriverLocationUpdate
parseTagUpdate "AadhaarOtpReqOptions" = return AadhaarOtpReqUpdate
parseTagUpdate "AadhaarOtpVerifyOptions" = return AadhaarOtpVerifyUpdate
parseTagUpdate "AadhaarVerificationOptions" = return AadhaarVerificationUpdate
parseTagUpdate "DriverLicenseOptions" = return DriverLicenseUpdate
parseTagUpdate "DriverRCAssociationOptions" = return DriverRCAssociationUpdate
parseTagUpdate "IdfyVerificationOptions" = return IdfyVerificationUpdate
parseTagUpdate "ImageOptions" = return ImageUpdate
parseTagUpdate "OperatingCityOptions" = return OperatingCityUpdate
parseTagUpdate "VehicleRegistrationCertificateOptions" = return VehicleRegistrationCertificateUpdate
parseTagUpdate "DriverQuoteOptions" = return DriverQuoteUpdate
parseTagUpdate "DriverReferralOptions" = return DriverReferralUpdate
parseTagUpdate "DriverStatsOptions" = return DriverStatsUpdate
parseTagUpdate "EstimateOptions" = return EstimateUpdate
parseTagUpdate "ExophoneOptions" = return ExophoneUpdate
parseTagUpdate "FareParametersOptions" = return FareParametersUpdate
parseTagUpdate "FareParametersProgressiveDetailsOptions" = return FareParametersProgressiveDetailsUpdate
parseTagUpdate "FareParametersSlabDetailsOptions" = return FareParametersSlabDetailsUpdate
parseTagUpdate "FarePolicyOptions" = return FarePolicyUpdate
parseTagUpdate "DriverExtraFeeBoundsOptions" = return DriverExtraFeeBoundsUpdate
parseTagUpdate "FarePolicyProgressiveDetailsOptions" = return FarePolicyProgressiveDetailsUpdate
parseTagUpdate "FarePolicyProgressiveDetailsPerExtraKmRateSectionOptions" = return FarePolicyProgressiveDetailsPerExtraKmRateSectionUpdate
parseTagUpdate "FarePolicySlabDetailsSlabOptions" = return FarePolicySlabDetailsSlabUpdate
parseTagUpdate "RestrictedExtraFareOptions" = return RestrictedExtraFareUpdate
parseTagUpdate "FareProductOptions" = return FareProductUpdate
parseTagUpdate "GeometryOptions" = return GeometryUpdate
parseTagUpdate "CommentOptions" = return CommentUpdate
parseTagUpdate "IssueCategoryOptions" = return IssueCategoryUpdate
parseTagUpdate "IssueOptionOptions" = return IssueOptionUpdate
parseTagUpdate "IssueReportOptions" = return IssueReportUpdate
parseTagUpdate "IssueTranslationOptions" = return IssueTranslationUpdate
parseTagUpdate "LeaderBoardConfigOptions" = return LeaderBoardConfigUpdate
parseTagUpdate "PlaceNameCacheOptions" = return PlaceNameCacheUpdate
parseTagUpdate "MediaFileOptions" = return MediaFileUpdate
parseTagUpdate "MerchantOptions" = return MerchantUpdate
parseTagUpdate "DriverIntelligentPoolConfigOptions" = return DriverIntelligentPoolConfigUpdate
parseTagUpdate "DriverPoolConfigOptions" = return DriverPoolConfigUpdate
parseTagUpdate "MerchantLeaderBoardConfigOptions" = return MerchantLeaderBoardConfigUpdate
parseTagUpdate "MerchantMessageOptions" = return MerchantMessageUpdate
parseTagUpdate "MerchantPaymentMethodOptions" = return MerchantPaymentMethodUpdate
parseTagUpdate "MerchantServiceConfigOptions" = return MerchantServiceConfigUpdate
parseTagUpdate "MerchantServiceUsageConfigOptions" = return MerchantServiceUsageConfigUpdate
parseTagUpdate "MerchantOnboardingDocumentConfigOptions" = return MerchantOnboardingDocumentConfigUpdate
parseTagUpdate "TransporterConfigOptions" = return TransporterConfigUpdate
parseTagUpdate "MessageOptions" = return MessageUpdate
parseTagUpdate "MessageReportOptions" = return MessageReportUpdate
parseTagUpdate "MessageTranslationOptions" = return MessageTranslationUpdate
parseTagUpdate "OnboardingDocumentConfigOptions" = return OnboardingDocumentConfigUpdate
parseTagUpdate "PersonOptions" = return PersonUpdate
parseTagUpdate "QuoteSpecialZoneOptions" = return QuoteSpecialZoneUpdate
parseTagUpdate "RatingOptions" = return RatingUpdate
parseTagUpdate "RegistrationTokenOptions" = return RegistrationTokenUpdate
parseTagUpdate "RideOptions" = return RideUpdate
parseTagUpdate "RideDetailsOptions" = return RideDetailsUpdate
parseTagUpdate "RiderDetailsOptions" = return RiderDetailsUpdate
parseTagUpdate "SearchRequestOptions" = return SearchRequestUpdate
parseTagUpdate "SearchReqLocationOptions" = return SearchReqLocationUpdate
parseTagUpdate "SearchRequestForDriverOptions" = return SearchRequestForDriverUpdate
parseTagUpdate "SearchRequestSpecialZoneOptions" = return SearchRequestSpecialZoneUpdate
parseTagUpdate "SearchTryOptions" = return SearchTryUpdate
parseTagUpdate "VehicleOptions" = return VehicleUpdate
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
  = BookingOptions UpdateModel [Set Postgres Booking.BookingT] (Where Postgres Booking.BookingT)
  | BookingLocationOptions UpdateModel [Set Postgres BookingLocation.BookingLocationT] (Where Postgres BookingLocation.BookingLocationT)
  | BookingCancellationReasonOptions UpdateModel [Set Postgres BookingCancellationReason.BookingCancellationReasonT] (Where Postgres BookingCancellationReason.BookingCancellationReasonT)
  | BusinessEventOptions UpdateModel [Set Postgres BusinessEvent.BusinessEventT] (Where Postgres BusinessEvent.BusinessEventT)
  | CallStatusOptions UpdateModel [Set Postgres CallStatus.CallStatusT] (Where Postgres CallStatus.CallStatusT)
  | CancellationReasonOptions UpdateModel [Set Postgres CancellationReason.CancellationReasonT] (Where Postgres CancellationReason.CancellationReasonT)
  | DriverFlowStatusOptions UpdateModel [Set Postgres DriverFlowStatus.DriverFlowStatusT] (Where Postgres DriverFlowStatus.DriverFlowStatusT)
  | DriverFeeOptions UpdateModel [Set Postgres DriverFee.DriverFeeT] (Where Postgres DriverFee.DriverFeeT)
  | DriverInformationOptions UpdateModel [Set Postgres DriverInformation.DriverInformationT] (Where Postgres DriverInformation.DriverInformationT)
  | DriverLocationOptions UpdateModel [Set Postgres DriverLocation.DriverLocationT] (Where Postgres DriverLocation.DriverLocationT)
  | AadhaarOtpReqOptions UpdateModel [Set Postgres AadhaarOtpReq.AadhaarOtpReqT] (Where Postgres AadhaarOtpReq.AadhaarOtpReqT)
  | AadhaarOtpVerifyOptions UpdateModel [Set Postgres AadhaarOtpVerify.AadhaarOtpVerifyT] (Where Postgres AadhaarOtpVerify.AadhaarOtpVerifyT)
  | AadhaarVerificationOptions UpdateModel [Set Postgres AadhaarVerification.AadhaarVerificationT] (Where Postgres AadhaarVerification.AadhaarVerificationT)
  | DriverLicenseOptions UpdateModel [Set Postgres DriverLicense.DriverLicenseT] (Where Postgres DriverLicense.DriverLicenseT)
  | DriverRCAssociationOptions UpdateModel [Set Postgres DriverRCAssociation.DriverRCAssociationT] (Where Postgres DriverRCAssociation.DriverRCAssociationT)
  | IdfyVerificationOptions UpdateModel [Set Postgres IdfyVerification.IdfyVerificationT] (Where Postgres IdfyVerification.IdfyVerificationT)
  | ImageOptions UpdateModel [Set Postgres Image.ImageT] (Where Postgres Image.ImageT)
  | OperatingCityOptions UpdateModel [Set Postgres OperatingCity.OperatingCityT] (Where Postgres OperatingCity.OperatingCityT)
  | VehicleRegistrationCertificateOptions UpdateModel [Set Postgres VehicleRegistrationCertificate.VehicleRegistrationCertificateT] (Where Postgres VehicleRegistrationCertificate.VehicleRegistrationCertificateT)
  | DriverQuoteOptions UpdateModel [Set Postgres DriverQuote.DriverQuoteT] (Where Postgres DriverQuote.DriverQuoteT)
  | DriverReferralOptions UpdateModel [Set Postgres DriverReferral.DriverReferralT] (Where Postgres DriverReferral.DriverReferralT)
  | DriverStatsOptions UpdateModel [Set Postgres DriverStats.DriverStatsT] (Where Postgres DriverStats.DriverStatsT)
  | EstimateOptions UpdateModel [Set Postgres Estimate.EstimateT] (Where Postgres Estimate.EstimateT)
  | ExophoneOptions UpdateModel [Set Postgres Exophone.ExophoneT] (Where Postgres Exophone.ExophoneT)
  | FareParametersOptions UpdateModel [Set Postgres FareParameters.FareParametersT] (Where Postgres FareParameters.FareParametersT)
  | FareParametersProgressiveDetailsOptions UpdateModel [Set Postgres FareParametersProgressiveDetails.FareParametersProgressiveDetailsT] (Where Postgres FareParametersProgressiveDetails.FareParametersProgressiveDetailsT)
  | FareParametersSlabDetailsOptions UpdateModel [Set Postgres FareParametersSlabDetails.FareParametersSlabDetailsT] (Where Postgres FareParametersSlabDetails.FareParametersSlabDetailsT)
  | FarePolicyOptions UpdateModel [Set Postgres FarePolicy.FarePolicyT] (Where Postgres FarePolicy.FarePolicyT)
  | DriverExtraFeeBoundsOptions UpdateModel [Set Postgres DriverExtraFeeBounds.DriverExtraFeeBoundsT] (Where Postgres DriverExtraFeeBounds.DriverExtraFeeBoundsT)
  | FarePolicyProgressiveDetailsOptions UpdateModel [Set Postgres FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsT] (Where Postgres FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsT)
  | FarePolicyProgressiveDetailsPerExtraKmRateSectionOptions UpdateModel [Set Postgres FarePolicyProgressiveDetailsPerExtraKmRateSection.FarePolicyProgressiveDetailsPerExtraKmRateSectionT] (Where Postgres FarePolicyProgressiveDetailsPerExtraKmRateSection.FarePolicyProgressiveDetailsPerExtraKmRateSectionT)
  | FarePolicySlabDetailsSlabOptions UpdateModel [Set Postgres FarePolicySlabDetailsSlab.FarePolicySlabsDetailsSlabT] (Where Postgres FarePolicySlabDetailsSlab.FarePolicySlabsDetailsSlabT)
  | RestrictedExtraFareOptions UpdateModel [Set Postgres RestrictedExtraFare.RestrictedExtraFareT] (Where Postgres RestrictedExtraFare.RestrictedExtraFareT)
  | FareProductOptions UpdateModel [Set Postgres FareProduct.FareProductT] (Where Postgres FareProduct.FareProductT)
  | GeometryOptions UpdateModel [Set Postgres Geometry.GeometryT] (Where Postgres Geometry.GeometryT)
  | CommentOptions UpdateModel [Set Postgres Comment.CommentT] (Where Postgres Comment.CommentT)
  | IssueCategoryOptions UpdateModel [Set Postgres IssueCategory.IssueCategoryT] (Where Postgres IssueCategory.IssueCategoryT)
  | IssueOptionOptions UpdateModel [Set Postgres IssueOption.IssueOptionT] (Where Postgres IssueOption.IssueOptionT)
  | IssueReportOptions UpdateModel [Set Postgres IssueReport.IssueReportT] (Where Postgres IssueReport.IssueReportT)
  | IssueTranslationOptions UpdateModel [Set Postgres IssueTranslation.IssueTranslationT] (Where Postgres IssueTranslation.IssueTranslationT)
  | LeaderBoardConfigOptions UpdateModel [Set Postgres LeaderBoardConfig.LeaderBoardConfigsT] (Where Postgres LeaderBoardConfig.LeaderBoardConfigsT)
  | PlaceNameCacheOptions UpdateModel [Set Postgres PlaceNameCache.PlaceNameCacheT] (Where Postgres PlaceNameCache.PlaceNameCacheT)
  | MediaFileOptions UpdateModel [Set Postgres MediaFile.MediaFileT] (Where Postgres MediaFile.MediaFileT)
  | MerchantOptions UpdateModel [Set Postgres Merchant.MerchantT] (Where Postgres Merchant.MerchantT)
  | DriverIntelligentPoolConfigOptions UpdateModel [Set Postgres DriverIntelligentPoolConfig.DriverIntelligentPoolConfigT] (Where Postgres DriverIntelligentPoolConfig.DriverIntelligentPoolConfigT)
  | DriverPoolConfigOptions UpdateModel [Set Postgres DriverPoolConfig.DriverPoolConfigT] (Where Postgres DriverPoolConfig.DriverPoolConfigT)
  | MerchantLeaderBoardConfigOptions UpdateModel [Set Postgres MerchantLeaderBoardConfig.LeaderBoardConfigsT] (Where Postgres MerchantLeaderBoardConfig.LeaderBoardConfigsT)
  | MerchantMessageOptions UpdateModel [Set Postgres MerchantMessage.MerchantMessageT] (Where Postgres MerchantMessage.MerchantMessageT)
  | MerchantPaymentMethodOptions UpdateModel [Set Postgres MerchantPaymentMethod.MerchantPaymentMethodT] (Where Postgres MerchantPaymentMethod.MerchantPaymentMethodT)
  | MerchantServiceConfigOptions UpdateModel [Set Postgres MerchantServiceConfig.MerchantServiceConfigT] (Where Postgres MerchantServiceConfig.MerchantServiceConfigT)
  | MerchantServiceUsageConfigOptions UpdateModel [Set Postgres MerchantServiceUsageConfig.MerchantServiceUsageConfigT] (Where Postgres MerchantServiceUsageConfig.MerchantServiceUsageConfigT)
  | MerchantOnboardingDocumentConfigOptions UpdateModel [Set Postgres MerchantOnboardingDocumentConfig.OnboardingDocumentConfigT] (Where Postgres MerchantOnboardingDocumentConfig.OnboardingDocumentConfigT)
  | TransporterConfigOptions UpdateModel [Set Postgres TransporterConfig.TransporterConfigT] (Where Postgres TransporterConfig.TransporterConfigT)
  | MessageOptions UpdateModel [Set Postgres Message.MessageT] (Where Postgres Message.MessageT)
  | MessageReportOptions UpdateModel [Set Postgres MessageReport.MessageReportT] (Where Postgres MessageReport.MessageReportT)
  | MessageTranslationOptions UpdateModel [Set Postgres MessageTranslation.MessageTranslationT] (Where Postgres MessageTranslation.MessageTranslationT)
  | OnboardingDocumentConfigOptions UpdateModel [Set Postgres OnboardingDocumentConfig.OnboardingDocumentConfigT] (Where Postgres OnboardingDocumentConfig.OnboardingDocumentConfigT)
  | PersonOptions UpdateModel [Set Postgres Person.PersonT] (Where Postgres Person.PersonT)
  | QuoteSpecialZoneOptions UpdateModel [Set Postgres QuoteSpecialZone.QuoteSpecialZoneT] (Where Postgres QuoteSpecialZone.QuoteSpecialZoneT)
  | RatingOptions UpdateModel [Set Postgres Rating.RatingT] (Where Postgres Rating.RatingT)
  | RegistrationTokenOptions UpdateModel [Set Postgres RegistrationToken.RegistrationTokenT] (Where Postgres RegistrationToken.RegistrationTokenT)
  | RideOptions UpdateModel [Set Postgres Ride.RideT] (Where Postgres Ride.RideT)
  | RideDetailsOptions UpdateModel [Set Postgres RideDetails.RideDetailsT] (Where Postgres RideDetails.RideDetailsT)
  | RiderDetailsOptions UpdateModel [Set Postgres RiderDetails.RiderDetailsT] (Where Postgres RiderDetails.RiderDetailsT)
  | SearchRequestOptions UpdateModel [Set Postgres SearchRequest.SearchRequestT] (Where Postgres SearchRequest.SearchRequestT)
  | SearchReqLocationOptions UpdateModel [Set Postgres SearchReqLocation.SearchReqLocationT] (Where Postgres SearchReqLocation.SearchReqLocationT)
  | SearchRequestForDriverOptions UpdateModel [Set Postgres SearchRequestForDriver.SearchRequestForDriverT] (Where Postgres SearchRequestForDriver.SearchRequestForDriverT)
  | SearchRequestSpecialZoneOptions UpdateModel [Set Postgres SearchRequestSpecialZone.SearchRequestSpecialZoneT] (Where Postgres SearchRequestSpecialZone.SearchRequestSpecialZoneT)
  | SearchTryOptions UpdateModel [Set Postgres SearchTry.SearchTryT] (Where Postgres SearchTry.SearchTryT)
  | VehicleOptions UpdateModel [Set Postgres Vehicle.VehicleT] (Where Postgres Vehicle.VehicleT)

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
      BookingUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ BookingOptions updateModel updVals whereClause
      BookingLocationUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ BookingLocationOptions updateModel updVals whereClause
      BookingCancellationReasonUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ BookingCancellationReasonOptions updateModel updVals whereClause
      BusinessEventUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ BusinessEventOptions updateModel updVals whereClause
      CallStatusUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ CallStatusOptions updateModel updVals whereClause
      CancellationReasonUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ CancellationReasonOptions updateModel updVals whereClause
      DriverFlowStatusUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ DriverFlowStatusOptions updateModel updVals whereClause
      DriverFeeUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ DriverFeeOptions updateModel updVals whereClause
      DriverInformationUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ DriverInformationOptions updateModel updVals whereClause
      DriverLocationUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ DriverLocationOptions updateModel updVals whereClause
      AadhaarOtpReqUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ AadhaarOtpReqOptions updateModel updVals whereClause
      AadhaarOtpVerifyUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ AadhaarOtpVerifyOptions updateModel updVals whereClause
      AadhaarVerificationUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ AadhaarVerificationOptions updateModel updVals whereClause
      DriverLicenseUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ DriverLicenseOptions updateModel updVals whereClause
      DriverRCAssociationUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ DriverRCAssociationOptions updateModel updVals whereClause
      IdfyVerificationUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ IdfyVerificationOptions updateModel updVals whereClause
      ImageUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ ImageOptions updateModel updVals whereClause
      OperatingCityUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ OperatingCityOptions updateModel updVals whereClause
      VehicleRegistrationCertificateUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ VehicleRegistrationCertificateOptions updateModel updVals whereClause
      DriverQuoteUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ DriverQuoteOptions updateModel updVals whereClause
      DriverReferralUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ DriverReferralOptions updateModel updVals whereClause
      DriverStatsUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ DriverStatsOptions updateModel updVals whereClause
      EstimateUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ EstimateOptions updateModel updVals whereClause
      ExophoneUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ ExophoneOptions updateModel updVals whereClause
      FareParametersUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ FareParametersOptions updateModel updVals whereClause
      FareParametersProgressiveDetailsUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ FareParametersProgressiveDetailsOptions updateModel updVals whereClause
      FareParametersSlabDetailsUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ FareParametersSlabDetailsOptions updateModel updVals whereClause
      FarePolicyUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ FarePolicyOptions updateModel updVals whereClause
      DriverExtraFeeBoundsUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ DriverExtraFeeBoundsOptions updateModel updVals whereClause
      FarePolicyProgressiveDetailsUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ FarePolicyProgressiveDetailsOptions updateModel updVals whereClause
      FarePolicyProgressiveDetailsPerExtraKmRateSectionUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ FarePolicyProgressiveDetailsPerExtraKmRateSectionOptions updateModel updVals whereClause
      FarePolicySlabDetailsSlabUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ FarePolicySlabDetailsSlabOptions updateModel updVals whereClause
      RestrictedExtraFareUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ RestrictedExtraFareOptions updateModel updVals whereClause
      FareProductUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ FareProductOptions updateModel updVals whereClause
      GeometryUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ GeometryOptions updateModel updVals whereClause
      CommentUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ CommentOptions updateModel updVals whereClause
      IssueCategoryUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ IssueCategoryOptions updateModel updVals whereClause
      IssueOptionUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ IssueOptionOptions updateModel updVals whereClause
      IssueReportUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ IssueReportOptions updateModel updVals whereClause
      IssueTranslationUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ IssueTranslationOptions updateModel updVals whereClause
      LeaderBoardConfigUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ LeaderBoardConfigOptions updateModel updVals whereClause
      PlaceNameCacheUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ PlaceNameCacheOptions updateModel updVals whereClause
      MediaFileUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MediaFileOptions updateModel updVals whereClause
      MerchantUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MerchantOptions updateModel updVals whereClause
      DriverIntelligentPoolConfigUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ DriverIntelligentPoolConfigOptions updateModel updVals whereClause
      DriverPoolConfigUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ DriverPoolConfigOptions updateModel updVals whereClause
      MerchantLeaderBoardConfigUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MerchantLeaderBoardConfigOptions updateModel updVals whereClause
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
      MerchantOnboardingDocumentConfigUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MerchantOnboardingDocumentConfigOptions updateModel updVals whereClause
      TransporterConfigUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ TransporterConfigOptions updateModel updVals whereClause
      MessageUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MessageOptions updateModel updVals whereClause
      MessageReportUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MessageReportOptions updateModel updVals whereClause
      MessageTranslationUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ MessageTranslationOptions updateModel updVals whereClause
      OnboardingDocumentConfigUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ OnboardingDocumentConfigOptions updateModel updVals whereClause
      PersonUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ PersonOptions updateModel updVals whereClause
      QuoteSpecialZoneUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ QuoteSpecialZoneOptions updateModel updVals whereClause
      RatingUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ RatingOptions updateModel updVals whereClause
      RegistrationTokenUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ RegistrationTokenOptions updateModel updVals whereClause
      RideUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ RideOptions updateModel updVals whereClause
      RideDetailsUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ RideDetailsOptions updateModel updVals whereClause
      RiderDetailsUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ RiderDetailsOptions updateModel updVals whereClause
      SearchRequestUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ SearchRequestOptions updateModel updVals whereClause
      SearchReqLocationUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ SearchReqLocationOptions updateModel updVals whereClause
      SearchRequestForDriverUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ SearchRequestForDriverOptions updateModel updVals whereClause
      SearchRequestSpecialZoneUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ SearchRequestSpecialZoneOptions updateModel updVals whereClause
      SearchTryUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ SearchTryOptions updateModel updVals whereClause
      VehicleUpdate -> do
        (updVals, whereClause) <- parseUpdateCommandValues contents
        return $ VehicleOptions updateModel updVals whereClause
