{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Types.DBSync.Create where

-- import qualified "dynamic-offer-driver-app" Storage.Beam.Common as Common

import EulerHS.Prelude
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

-- import Euler.DB.Storage.Types
--   ( TxnOfferInfo
--   , HdfcHashedNum
--   , MerchantGatewayAccount
--   , Rule
--   , MerchantGatewayAccountSubInfo
--   , OrderReference
--   , Mandate
--   , GatewayTxnData
--   , MerchantGatewayCardInfo
--   , TxnRiskCheck
--   , PaymentMethod
--   , OfferBenefitInfo
--   , MerchantIframePreferences
--   , ResellerAccount
--   , UnifiedGatewayResponse
--   , WalletTopUpTxn
--   , IsinRoutes
--   , TxnCardInfo
--   , OrderAddress
--   , LockerAccount
--   , JuspayBankCode
--   , IssuerRoutes
--   , Refund
--   , SecondFactor
--   , GatewayOutage
--   , TxnDetail
--   , EmiPlan
--   , MerchantKey
--   , NetworkCardFingerprint
--   , TokenRequestor
--   , TxnOfferDetail
--   , SecondFactorResponse
--   , OfferRedemption
--   , CardBrandRoutes
--   , Customer
--   , MerchantAccount
--   , ExternalMerchantCustomer
--   , TokenBinInfo
--   , MerchantGatewayPaymentMethod
--   , Promotion
--   , LockerTokenRequestor
--   , BankAccount
--   , Agency
--   , Provider
--   , GatewayCardInfo
--   , PaymentGatewayResponse
--   , Metadata
--   , Chargeback
--   , WalletAccount
--   , GatewayStatusMap
--   , Token
--   , MerchantLockerAccount
--   , TempCard
--   , MerchantRiskSettings
--   , User
--   , CofDetails
--   , OrderMetadataV2
--   , StoredCard
--   , TokenCustomer
--   , EnrolledPan
--   , Offers
--   , Role
--   , Feature
--   , GatewayBankEmiSupport
--   , AuthenticationAccount
--   , PaymentGatewayResponseV1
--   , SavedPaymentMethod
--   , MerchantProviderDetails
--   , TxnOffer
--   , GatewayHealth
--   , RiskManagementAccount
--   , CardInfo
--   , DeviceBinding
--   , Notification
--   , OrderBasket
--   , GatewayPaymentMethod
--   , JuspayEvent
--   , ProcessTracker
--   , PaymentLinks
--   , CustomerAccount
--   , EntityMap
--   , FormInput
--   , IngressRule
--   , Installment
--   , InstallmentRefund
--   , PaymentForm
--   , UserRole
--   , TxnIntentDetail
--   , AuthMapping
--   )

-- -- Create Object
-- data DBCreateObject
--   = TxnOfferInfoObject TxnOfferInfo
--   | JuspayEventObject JuspayEvent
--   | HdfcHashedNumObject HdfcHashedNum
--   | MerchantGatewayAccountObject MerchantGatewayAccount
--   | RuleObject Rule
--   | MerchantGatewayAccountSubInfoObject MerchantGatewayAccountSubInfo
--   | OrderReferenceObject OrderReference
--   | MandateObject Mandate
--   | GatewayTxnDataObject GatewayTxnData
--   | MerchantGatewayCardInfoObject MerchantGatewayCardInfo
--   | TxnRiskCheckObject TxnRiskCheck
--   | PaymentMethodObject PaymentMethod
--   | OfferBenefitInfoObject OfferBenefitInfo
--   | MerchantIframePreferencesObject MerchantIframePreferences
--   | ResellerAccountObject ResellerAccount
--   | UnifiedGatewayResponseObject UnifiedGatewayResponse
--   | WalletTopUpTxnObject WalletTopUpTxn
--   | IsinRoutesObject IsinRoutes
--   | TxnCardInfoObject TxnCardInfo
--   | OrderAddressObject OrderAddress
--   | LockerAccountObject LockerAccount
--   | JuspayBankCodeObject JuspayBankCode
--   | IssuerRoutesObject IssuerRoutes
--   | RefundObject Refund
--   | SecondFactorObject SecondFactor
--   | GatewayOutageObject GatewayOutage
--   | TxnDetailObject TxnDetail
--   | EmiPlanObject EmiPlan
--   | MerchantKeyObject MerchantKey
--   | NetworkCardFingerprintObject NetworkCardFingerprint
--   | TokenRequestorObject TokenRequestor
--   | TxnOfferDetailObject TxnOfferDetail
--   | SecondFactorResponseObject SecondFactorResponse
--   | OfferRedemptionObject OfferRedemption
--   | CardBrandRoutesObject CardBrandRoutes
--   | CustomerObject Customer
--   | MerchantAccountObject MerchantAccount
--   | ExternalMerchantCustomerObject ExternalMerchantCustomer
--   | TokenBinInfoObject TokenBinInfo
--   | MerchantGatewayPaymentMethodObject MerchantGatewayPaymentMethod
--   | PromotionObject Promotion
--   | LockerTokenRequestorObject LockerTokenRequestor
--   | BankAccountObject BankAccount
--   | AgencyObject Agency
--   | ProviderObject Provider
--   | GatewayCardInfoObject GatewayCardInfo
--   | PaymentGatewayResponseObject PaymentGatewayResponse
--   | MetadataObject Metadata
--   | ChargebackObject Chargeback
--   | WalletAccountObject WalletAccount
--   | GatewayStatusMapObject GatewayStatusMap
--   | TokenObject Token
--   | MerchantLockerAccountObject MerchantLockerAccount
--   | TempCardObject TempCard
--   | MerchantRiskSettingsObject MerchantRiskSettings
--   | UserObject User
--   | CofDetailsObject CofDetails
--   | OrderMetadataV2Object OrderMetadataV2
--   | StoredCardObject StoredCard
--   | TokenCustomerObject TokenCustomer
--   | EnrolledPanObject EnrolledPan
--   | OffersObject Offers
--   | RoleObject Role
--   | FeatureObject Feature
--   | GatewayBankEmiSupportObject GatewayBankEmiSupport
--   | AuthenticationAccountObject AuthenticationAccount
--   | PaymentGatewayResponseV1Object PaymentGatewayResponseV1
--   | SavedPaymentMethodObject SavedPaymentMethod
--   | MerchantProviderDetailsObject MerchantProviderDetails
--   | TxnOfferObject TxnOffer
--   | GatewayHealthObject GatewayHealth
--   | RiskManagementAccountObject RiskManagementAccount
--   | CardInfoObject CardInfo
--   | DeviceBindingObject DeviceBinding
--   | NotificationObject Notification
--   | OrderBasketObject OrderBasket
--   | GatewayPaymentMethodObject GatewayPaymentMethod
--   | ProcessTrackerObject ProcessTracker
--   | PaymentLinksObject PaymentLinks
--   | CustomerAccountObject CustomerAccount
--   | EntityMapObject EntityMap
--   | FormInputObject FormInput
--   | IngressRuleObject IngressRule
--   | InstallmentObject Installment
--   | InstallmentRefundObject InstallmentRefund
--   | PaymentFormObject PaymentForm
--   | UserRoleObject UserRole
--   | TxnIntentDetailObject TxnIntentDetail
--   | AuthMappingObject AuthMapping
--   deriving (Generic, FromJSON, ToJSON, Show)

data DBCreateObject
  = BookingObject Booking.Booking
  | BookingLocationObject BookingLocation.BookingLocation
  | BookingCancellationReasonObject BookingCancellationReason.BookingCancellationReason
  | BusinessEventObject BusinessEvent.BusinessEvent
  | CallStatusObject CallStatus.CallStatus
  | CancellationReasonObject CancellationReason.CancellationReason
  | DriverFlowStatusObject DriverFlowStatus.DriverFlowStatus
  | DriverFeeObject DriverFee.DriverFee
  | DriverInformationObject DriverInformation.DriverInformation
  | DriverLocationObject DriverLocation.DriverLocation
  | AadhaarOtpReqObject AadhaarOtpReq.AadhaarOtpReq
  | AadhaarOtpVerifyObject AadhaarOtpVerify.AadhaarOtpVerify
  | AadhaarVerificationObject AadhaarVerification.AadhaarVerification
  | DriverLicenseObject DriverLicense.DriverLicense
  | DriverRCAssociationObject DriverRCAssociation.DriverRCAssociation
  | IdfyVerificationObject IdfyVerification.IdfyVerification
  | ImageObject Image.Image
  | OperatingCityObject OperatingCity.OperatingCity
  | VehicleRegistrationCertificateObject VehicleRegistrationCertificate.VehicleRegistrationCertificate
  | DriverQuoteObject DriverQuote.DriverQuote
  | DriverReferralObject DriverReferral.DriverReferral
  | DriverStatsObject DriverStats.DriverStats
  | EstimateObject Estimate.Estimate
  | ExophoneObject Exophone.Exophone
  | FareParametersObject FareParameters.FareParameters
  | FareParametersProgressiveDetailsObject FareParametersProgressiveDetails.FareParametersProgressiveDetails
  | FareParametersSlabDetailsObject FareParametersSlabDetails.FareParametersSlabDetails
  | FarePolicyObject FarePolicy.FarePolicy
  | DriverExtraFeeBoundsObject DriverExtraFeeBounds.DriverExtraFeeBounds
  | FarePolicyProgressiveDetailsObject FarePolicyProgressiveDetails.FarePolicyProgressiveDetails
  | FarePolicyProgressiveDetailsPerExtraKmRateSectionObject FarePolicyProgressiveDetailsPerExtraKmRateSection.FarePolicyProgressiveDetailsPerExtraKmRateSection
  | FarePolicySlabDetailsSlabObject FarePolicySlabDetailsSlab.FarePolicySlabsDetailsSlab
  | RestrictedExtraFareObject RestrictedExtraFare.RestrictedExtraFare
  | FareProductObject FareProduct.FareProduct
  | GeometryObject Geometry.Geometry
  | CommentObject Comment.Comment
  | IssueCategoryObject IssueCategory.IssueCategory
  | IssueOptionObject IssueOption.IssueOption
  | IssueReportObject IssueReport.IssueReport
  | IssueTranslationObject IssueTranslation.IssueTranslation
  | LeaderBoardConfigObject LeaderBoardConfig.LeaderBoardConfigs
  | PlaceNameCacheObject PlaceNameCache.PlaceNameCache
  | MediaFileObject MediaFile.MediaFile
  | MerchantObject Merchant.Merchant
  | DriverIntelligentPoolConfigObject DriverIntelligentPoolConfig.DriverIntelligentPoolConfig
  | DriverPoolConfigObject DriverPoolConfig.DriverPoolConfig
  | MerchantLeaderBoardConfigObject MerchantLeaderBoardConfig.LeaderBoardConfigs
  | MerchantMessageObject MerchantMessage.MerchantMessage
  | MerchantPaymentMethodObject MerchantPaymentMethod.MerchantPaymentMethod
  | MerchantServiceConfigObject MerchantServiceConfig.MerchantServiceConfig
  | MerchantServiceUsageConfigObject MerchantServiceUsageConfig.MerchantServiceUsageConfig
  | MerchantOnboardingDocumentConfigObject MerchantOnboardingDocumentConfig.OnboardingDocumentConfig
  | TransporterConfigObject TransporterConfig.TransporterConfig
  | MessageObject Message.Message
  | MessageReportObject MessageReport.MessageReport
  | MessageTranslationObject MessageTranslation.MessageTranslation
  | OnboardingDocumentConfigObject OnboardingDocumentConfig.OnboardingDocumentConfig
  | PersonObject Person.Person
  | QuoteSpecialZoneObject QuoteSpecialZone.QuoteSpecialZone
  | RatingObject Rating.Rating
  | RideObject Ride.Ride
  | RideDetailsObject RideDetails.RideDetails
  | RiderDetailsObject RiderDetails.RiderDetails
  | SearchRequestObject SearchRequest.SearchRequest
  | SearchReqLocationObject SearchReqLocation.SearchReqLocation
  | SearchRequestForDriverObject SearchRequestForDriver.SearchRequestForDriver
  | SearchRequestSpecialZoneObject SearchRequestSpecialZone.SearchRequestSpecialZone
  | SearchTryObject SearchTry.SearchTry
  | VehicleObject Vehicle.Vehicle
  | RegistrationTokenObject RegistrationToken.RegistrationToken
  | FeedbackFormObject FeedbackForm.FeedbackForm
  | FeedbackObject Feedback.Feedback
  | FeedbackBadgeObject FeedbackBadge.FeedbackBadge
  deriving (Generic, FromJSON, ToJSON, Show)

-- -- Convert database storage types into DBObject types
-- -- which are part of a command and can be deserialized.

-- modelName :: DBCreateObject -> Text
-- modelName (TxnOfferInfoObject _                 ) = "TxnOfferInfo"
-- modelName (JuspayEventObject _                  ) = "JuspayEvent"
-- modelName (HdfcHashedNumObject _                ) = "HdfcHashedNum"
-- modelName (MerchantGatewayAccountObject _       ) = "MerchantGatewayAccount"
-- modelName (RuleObject _                         ) = "Rule"
-- modelName (MerchantGatewayAccountSubInfoObject _) = "MerchantGatewayAccountSubInfo"
-- modelName (OrderReferenceObject _               ) = "OrderReference"
-- modelName (MandateObject _                      ) = "Mandate"
-- modelName (GatewayTxnDataObject _               ) = "GatewayTxnData"
-- modelName (MerchantGatewayCardInfoObject _      ) = "MerchantGatewayCardInfo"
-- modelName (TxnRiskCheckObject _                 ) = "TxnRiskCheck"
-- modelName (PaymentMethodObject _                ) = "PaymentMethod"
-- modelName (OfferBenefitInfoObject _             ) = "OfferBenefitInfo"
-- modelName (MerchantIframePreferencesObject _    ) = "MerchantIframePreferences"
-- modelName (ResellerAccountObject _              ) = "ResellerAccount"
-- modelName (UnifiedGatewayResponseObject _       ) = "UnifiedGatewayResponse"
-- modelName (WalletTopUpTxnObject _               ) = "WalletTopUpTxn"
-- modelName (IsinRoutesObject _                   ) = "IsinRoutes"
-- modelName (TxnCardInfoObject _                  ) = "TxnCardInfo"
-- modelName (OrderAddressObject _                 ) = "OrderAddress"
-- modelName (LockerAccountObject _                ) = "LockerAccount"
-- modelName (JuspayBankCodeObject _               ) = "JuspayBankCode"
-- modelName (IssuerRoutesObject _                 ) = "IssuerRoutes"
-- modelName (RefundObject _                       ) = "Refund"
-- modelName (SecondFactorObject _                 ) = "SecondFactor"
-- modelName (GatewayOutageObject _                ) = "GatewayOutage"
-- modelName (TxnDetailObject _                    ) = "TxnDetail"
-- modelName (EmiPlanObject _                      ) = "EmiPlan"
-- modelName (MerchantKeyObject _                  ) = "MerchantKey"
-- modelName (NetworkCardFingerprintObject _       ) = "NetworkCardFingerprint"
-- modelName (TokenRequestorObject _               ) = "TokenRequestor"
-- modelName (TxnOfferDetailObject _               ) = "TxnOfferDetail"
-- modelName (SecondFactorResponseObject _         ) = "SecondFactorResponse"
-- modelName (OfferRedemptionObject _              ) = "OfferRedemption"
-- modelName (CardBrandRoutesObject _              ) = "CardBrandRoutes"
-- modelName (CustomerObject _                     ) = "Customer"
-- modelName (MerchantAccountObject _              ) = "MerchantAccount"
-- modelName (ExternalMerchantCustomerObject _     ) = "ExternalMerchantCustomer"
-- modelName (TokenBinInfoObject _                 ) = "TokenBinInfo"
-- modelName (MerchantGatewayPaymentMethodObject _ ) = "MerchantGatewayPaymentMethod"
-- modelName (PromotionObject _                    ) = "Promotion"
-- modelName (LockerTokenRequestorObject _         ) = "LockerTokenRequestor"
-- modelName (BankAccountObject _                  ) = "BankAccount"
-- modelName (AgencyObject _                       ) = "Agency"
-- modelName (ProviderObject _                     ) = "Provider"
-- modelName (GatewayCardInfoObject _              ) = "GatewayCardInfo"
-- modelName (PaymentGatewayResponseObject _       ) = "PaymentGatewayResponse"
-- modelName (MetadataObject _                     ) = "Metadata"
-- modelName (ChargebackObject _                   ) = "Chargeback"
-- modelName (WalletAccountObject _                ) = "WalletAccount"
-- modelName (GatewayStatusMapObject _             ) = "GatewayStatusMap"
-- modelName (TokenObject _                        ) = "Token"
-- modelName (MerchantLockerAccountObject _        ) = "MerchantLockerAccount"
-- modelName (TempCardObject _                     ) = "TempCard"
-- modelName (MerchantRiskSettingsObject _         ) = "MerchantRiskSettings"
-- modelName (UserObject _                         ) = "User"
-- modelName (CofDetailsObject _                   ) = "CofDetails"
-- modelName (OrderMetadataV2Object _              ) = "OrderMetadataV2"
-- modelName (StoredCardObject _                   ) = "StoredCard"
-- modelName (TokenCustomerObject _                ) = "TokenCustomer"
-- modelName (EnrolledPanObject _                  ) = "EnrolledPan"
-- modelName (OffersObject _                       ) = "Offers"
-- modelName (RoleObject _                         ) = "Role"
-- modelName (FeatureObject _                      ) = "Feature"
-- modelName (GatewayBankEmiSupportObject _        ) = "GatewayBankEmiSupport"
-- modelName (AuthenticationAccountObject _        ) = "AuthenticationAccount"
-- modelName (PaymentGatewayResponseV1Object _     ) = "PaymentGatewayResponseV1"
-- modelName (SavedPaymentMethodObject _           ) = "SavedPaymentMethod"
-- modelName (MerchantProviderDetailsObject _      ) = "MerchantProviderDetails"
-- modelName (TxnOfferObject _                     ) = "TxnOffer"
-- modelName (GatewayHealthObject _                ) = "GatewayHealth"
-- modelName (RiskManagementAccountObject _        ) = "RiskManagementAccount"
-- modelName (CardInfoObject _                     ) = "CardInfo"
-- modelName (DeviceBindingObject _                ) = "DeviceBinding"
-- modelName (NotificationObject _                 ) = "Notification"
-- modelName (OrderBasketObject _                  ) = "OrderBasket"
-- modelName (GatewayPaymentMethodObject _         ) = "GatewayPaymentMethod"
-- modelName (ProcessTrackerObject _               ) = "ProcessTracker"
-- modelName (PaymentLinksObject _                 ) = "PaymentLinks"
-- modelName (CustomerAccountObject _              ) = "CustomerAccount"
-- modelName (EntityMapObject _                    ) = "EntityMap"
-- modelName (FormInputObject _                    ) = "FormInput"
-- modelName (IngressRuleObject _                  ) = "IngressRule"
-- modelName (InstallmentObject _                  ) = "Installment"
-- modelName (InstallmentRefundObject _            ) = "InstallmentRefund"
-- modelName (PaymentFormObject _                  ) = "PaymentForm"
-- modelName (UserRoleObject _                     ) = "UserRole"
-- modelName (TxnIntentDetailObject _              ) = "TxnIntentDetail"
-- modelName (AuthMappingObject _                  ) = "AuthMapping"

modelName :: DBCreateObject -> Text
modelName (BookingObject _) = "Booking"
modelName (BookingLocationObject _) = "BookingLocation"
modelName (BookingCancellationReasonObject _) = "BookingCancellationReason"
modelName (BusinessEventObject _) = "BusinessEvent"
modelName (CallStatusObject _) = "CallStatus"
modelName (CancellationReasonObject _) = "CancellationReason"
modelName (DriverFlowStatusObject _) = "DriverFlowStatus"
modelName (DriverFeeObject _) = "DriverFee"
modelName (DriverInformationObject _) = "DriverInformation"
modelName (DriverLocationObject _) = "DriverLocation"
modelName (AadhaarOtpReqObject _) = "AadhaarOtpReq"
modelName (AadhaarOtpVerifyObject _) = "AadhaarOtpVerify"
modelName (AadhaarVerificationObject _) = "AadhaarVerification"
modelName (DriverLicenseObject _) = "DriverLicense"
modelName (DriverRCAssociationObject _) = "DriverRCAssociation"
modelName (IdfyVerificationObject _) = "IdfyVerification"
modelName (ImageObject _) = "Image"
modelName (OperatingCityObject _) = "OperatingCity"
modelName (VehicleRegistrationCertificateObject _) = "VehicleRegistrationCertificate"
modelName (DriverQuoteObject _) = "DriverQuote"
modelName (DriverReferralObject _) = "DriverReferral"
modelName (DriverStatsObject _) = "DriverStats"
modelName (EstimateObject _) = "Estimate"
modelName (ExophoneObject _) = "Exophone"
modelName (FareParametersObject _) = "FareParameters"
modelName (FareParametersProgressiveDetailsObject _) = "FareParametersProgressiveDetails"
modelName (FareParametersSlabDetailsObject _) = "FareParametersSlabDetails"
modelName (FarePolicyObject _) = "FarePolicy"
modelName (DriverExtraFeeBoundsObject _) = "DriverExtraFeeBounds"
modelName (FarePolicyProgressiveDetailsObject _) = "FarePolicyProgressiveDetails"
modelName (FarePolicyProgressiveDetailsPerExtraKmRateSectionObject _) = "FarePolicyProgressiveDetailsPerExtraKmRateSection"
modelName (FarePolicySlabDetailsSlabObject _) = "FarePolicySlabDetailsSlab"
modelName (RestrictedExtraFareObject _) = "RestrictedExtraFare"
modelName (FareProductObject _) = "FareProduct"
modelName (GeometryObject _) = "Geometry"
modelName (CommentObject _) = "Comment"
modelName (IssueCategoryObject _) = "IssueCategory"
modelName (IssueOptionObject _) = "IssueOption"
modelName (IssueReportObject _) = "IssueReport"
modelName (IssueTranslationObject _) = "IssueTranslation"
modelName (LeaderBoardConfigObject _) = "LeaderBoardConfig"
modelName (PlaceNameCacheObject _) = "PlaceNameCache"
modelName (MediaFileObject _) = "MediaFile"
modelName (MerchantObject _) = "Merchant"
modelName (DriverIntelligentPoolConfigObject _) = "DriverIntelligentPoolConfig"
modelName (DriverPoolConfigObject _) = "DriverPoolConfig"
modelName (MerchantLeaderBoardConfigObject _) = "MerchantLeaderBoardConfig"
modelName (MerchantMessageObject _) = "MerchantMessage"
modelName (MerchantPaymentMethodObject _) = "MerchantPaymentMethod"
modelName (MerchantServiceConfigObject _) = "MerchantServiceConfig"
modelName (MerchantServiceUsageConfigObject _) = "MerchantServiceUsageConfig"
modelName (MerchantOnboardingDocumentConfigObject _) = "MerchantOnboardingDocumentConfig"
modelName (TransporterConfigObject _) = "TransporterConfig"
modelName (MessageObject _) = "Message"
modelName (MessageReportObject _) = "MessageReport"
modelName (MessageTranslationObject _) = "MessageTranslation"
modelName (OnboardingDocumentConfigObject _) = "OnboardingDocumentConfig"
modelName (PersonObject _) = "Person"
modelName (QuoteSpecialZoneObject _) = "QuoteSpecialZone"
modelName (RatingObject _) = "Rating"
modelName (RideObject _) = "Ride"
modelName (RideDetailsObject _) = "RideDetails"
modelName (RiderDetailsObject _) = "RiderDetails"
modelName (SearchRequestObject _) = "SearchRequest"
modelName (SearchReqLocationObject _) = "SearchReqLocation"
modelName (SearchRequestForDriverObject _) = "SearchRequestForDriver"
modelName (SearchRequestSpecialZoneObject _) = "SearchRequestSpecialZone"
modelName (SearchTryObject _) = "SearchTry"
modelName (VehicleObject _) = "Vehicle"
modelName (RegistrationTokenObject _) = "RegistrationToken"
modelName (FeedbackFormObject _) = "FeedBackForm"
modelName (FeedbackObject _) = "FeedBack"
modelName (FeedbackBadgeObject _) = "FeedBackBadge"
