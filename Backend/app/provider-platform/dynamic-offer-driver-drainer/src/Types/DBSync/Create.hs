{-# LANGUAGE DeriveAnyClass #-}

module Types.DBSync.Create where

import EulerHS.Prelude
import qualified "dynamic-offer-driver-app" Storage.Beam.BapMetadata as BapMetadata
import qualified "dynamic-offer-driver-app" Storage.Beam.BecknRequest as BecknRequest
import qualified "dynamic-offer-driver-app" Storage.Beam.Booking as Booking
import qualified "dynamic-offer-driver-app" Storage.Beam.BookingCancellationReason as BookingCancellationReason
import qualified "dynamic-offer-driver-app" Storage.Beam.BusinessEvent as BusinessEvent
import qualified "dynamic-offer-driver-app" Storage.Beam.CallStatus as CallStatus
import qualified "dynamic-offer-driver-app" Storage.Beam.CancellationReason as CancellationReason
import qualified "dynamic-offer-driver-app" Storage.Beam.Driver.DriverFlowStatus as DriverFlowStatus
import qualified "dynamic-offer-driver-app" Storage.Beam.Driver.GoHomeFeature.DriverGoHomeRequest as DriverGoHomeRequest
import qualified "dynamic-offer-driver-app" Storage.Beam.Driver.GoHomeFeature.DriverHomeLocation as DriverHomeLocation
import qualified "dynamic-offer-driver-app" Storage.Beam.DriverBlockReason as DriverBlockReason
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
import qualified "dynamic-offer-driver-app" Storage.Beam.GoHomeConfig as GoHomeConfig
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
import qualified "dynamic-offer-driver-app" Storage.Beam.MetaData as MetaData
import qualified "dynamic-offer-driver-app" Storage.Beam.OnboardingDocumentConfig as OnboardingDocumentConfig
import qualified "dynamic-offer-driver-app" Storage.Beam.Person as Person
import qualified "dynamic-offer-driver-app" Storage.Beam.QuoteSpecialZone as QuoteSpecialZone
import qualified "dynamic-offer-driver-app" Storage.Beam.Rating as Rating
import qualified "dynamic-offer-driver-app" Storage.Beam.RegistrationToken as RegistrationToken
import qualified "dynamic-offer-driver-app" Storage.Beam.RegistryMapFallback as RegistryMapFallback
import qualified "dynamic-offer-driver-app" Storage.Beam.Ride.Table as Ride
import qualified "dynamic-offer-driver-app" Storage.Beam.RideDetails as RideDetails
import qualified "dynamic-offer-driver-app" Storage.Beam.RiderDetails as RiderDetails
import qualified "dynamic-offer-driver-app" Storage.Beam.SearchRequest as SearchRequest
import qualified "dynamic-offer-driver-app" Storage.Beam.SearchRequestForDriver as SearchRequestForDriver
import qualified "dynamic-offer-driver-app" Storage.Beam.SearchRequestSpecialZone as SearchRequestSpecialZone
import qualified "dynamic-offer-driver-app" Storage.Beam.SearchTry as SearchTry
import qualified "dynamic-offer-driver-app" Storage.Beam.Vehicle as Vehicle
import qualified "dynamic-offer-driver-app" Storage.Beam.Volunteer as Volunteer

data DBCreateObject
  = BapMetadataObject BapMetadata.BapMetadata
  | BookingObject Booking.Booking
  | BookingCancellationReasonObject BookingCancellationReason.BookingCancellationReason
  | BusinessEventObject BusinessEvent.BusinessEvent
  | CallStatusObject CallStatus.CallStatus
  | CancellationReasonObject CancellationReason.CancellationReason
  | DriverFlowStatusObject DriverFlowStatus.DriverFlowStatus
  | DriverBlockReasonObject DriverBlockReason.DriverBlockReason
  | DriverFeeObject DriverFee.DriverFee
  | DriverInformationObject DriverInformation.DriverInformation
  | DriverLocationObject DriverLocation.DriverLocation
  | AadhaarOtpReqObject AadhaarOtpReq.AadhaarOtpReq
  | AadhaarOtpVerifyObject AadhaarOtpVerify.AadhaarOtpVerify
  | AadhaarVerificationObject AadhaarVerification.AadhaarVerification
  | DriverLicenseObject DriverLicense.DriverLicense
  | DriverRcAssociationObject DriverRCAssociation.DriverRCAssociation
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
  | MetaDataObject MetaData.MetaData
  | OnboardingDocumentConfigObject OnboardingDocumentConfig.OnboardingDocumentConfig
  | PersonObject Person.Person
  | QuoteSpecialZoneObject QuoteSpecialZone.QuoteSpecialZone
  | RatingObject Rating.Rating
  | RideObject Ride.Ride
  | RideDetailsObject RideDetails.RideDetails
  | RiderDetailsObject RiderDetails.RiderDetails
  | SearchRequestObject SearchRequest.SearchRequest
  | SearchRequestForDriverObject SearchRequestForDriver.SearchRequestForDriver
  | SearchRequestSpecialZoneObject SearchRequestSpecialZone.SearchRequestSpecialZone
  | SearchTryObject SearchTry.SearchTry
  | VehicleObject Vehicle.Vehicle
  | VolunteerObject Volunteer.Volunteer
  | RegistrationTokenObject RegistrationToken.RegistrationToken
  | FeedbackFormObject FeedbackForm.FeedbackForm
  | FeedbackObject Feedback.Feedback
  | FeedbackBadgeObject FeedbackBadge.FeedbackBadge
  | BecknRequestObject BecknRequest.BecknRequest
  | RegistryMapFallbackObject RegistryMapFallback.RegistryMapFallback
  | DriverGoHomeRequestObject DriverGoHomeRequest.DriverGoHomeRequest
  | DriverHomeLocationObject DriverHomeLocation.DriverHomeLocation
  | GoHomeConfigObject GoHomeConfig.GoHomeConfig
  deriving (Generic, FromJSON, ToJSON, Show)

-- -- Convert database storage types into DBObject types
-- -- which are part of a command and can be deserialized.

modelName :: DBCreateObject -> Text
modelName (BapMetadataObject _) = "BapMetadata"
modelName (BookingObject _) = "Booking"
modelName (BookingCancellationReasonObject _) = "BookingCancellationReason"
modelName (BusinessEventObject _) = "BusinessEvent"
modelName (CallStatusObject _) = "CallStatus"
modelName (CancellationReasonObject _) = "CancellationReason"
modelName (DriverFlowStatusObject _) = "DriverFlowStatus"
modelName (DriverBlockReasonObject _) = "DriverBlockReason"
modelName (DriverFeeObject _) = "DriverFee"
modelName (DriverInformationObject _) = "DriverInformation"
modelName (DriverLocationObject _) = "DriverLocation"
modelName (AadhaarOtpReqObject _) = "AadhaarOtpReq"
modelName (AadhaarOtpVerifyObject _) = "AadhaarOtpVerify"
modelName (AadhaarVerificationObject _) = "AadhaarVerification"
modelName (DriverLicenseObject _) = "DriverLicense"
modelName (DriverRcAssociationObject _) = "DriverRcAssociation"
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
modelName (MetaDataObject _) = "MetaData"
modelName (OnboardingDocumentConfigObject _) = "OnboardingDocumentConfig"
modelName (PersonObject _) = "Person"
modelName (QuoteSpecialZoneObject _) = "QuoteSpecialZone"
modelName (RatingObject _) = "Rating"
modelName (RideObject _) = "Ride"
modelName (RideDetailsObject _) = "RideDetails"
modelName (RiderDetailsObject _) = "RiderDetails"
modelName (SearchRequestObject _) = "SearchRequest"
modelName (SearchRequestForDriverObject _) = "SearchRequestForDriver"
modelName (SearchRequestSpecialZoneObject _) = "SearchRequestSpecialZone"
modelName (SearchTryObject _) = "SearchTry"
modelName (VehicleObject _) = "Vehicle"
modelName (VolunteerObject _) = "Volunteer"
modelName (RegistrationTokenObject _) = "RegistrationToken"
modelName (FeedbackFormObject _) = "FeedBackForm"
modelName (FeedbackObject _) = "FeedBack"
modelName (FeedbackBadgeObject _) = "FeedBackBadge"
modelName (BecknRequestObject _) = "BecknRequest"
modelName (RegistryMapFallbackObject _) = "RegistryMapFallback"
modelName (DriverGoHomeRequestObject _) = "DriverGoHomeRequest"
modelName (DriverHomeLocationObject _) = "DriverHomeLocation"
modelName (GoHomeConfigObject _) = "GoHomeConfig"
