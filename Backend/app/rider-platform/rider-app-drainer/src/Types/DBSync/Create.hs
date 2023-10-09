{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Types.DBSync.Create where

import EulerHS.Prelude
import qualified "rider-app" Storage.Beam.AppInstalls as AppInstalls
import qualified "rider-app" Storage.Beam.BecknRequest as BecknRequest
import qualified "rider-app" Storage.Beam.BlackListOrg as BlackListOrg
import qualified "rider-app" Storage.Beam.Booking as Booking
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
import qualified "rider-app" Storage.Beam.Location as Location
import qualified "rider-app" Storage.Beam.LocationMapping as LocationMapping
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
import qualified "rider-app" Storage.Beam.Rating as Rating
import qualified "rider-app" Storage.Beam.RegistrationToken as RegistrationToken
import qualified "rider-app" Storage.Beam.RentalSlab as RentalSlab
import qualified "rider-app" Storage.Beam.Ride as Ride
import qualified "rider-app" Storage.Beam.SavedReqLocation as SavedReqLocation
import qualified "rider-app" Storage.Beam.SearchRequest as SearchRequest
import qualified "rider-app" Storage.Beam.Sos as Sos
import qualified "rider-app" Storage.Beam.SpecialZoneQuote as SpecialZoneQuote
import qualified "rider-app" Storage.Beam.TripTerms as TripTerms
import qualified "rider-app" Storage.Beam.Webengage as Webengage

data DBCreateObject
  = AppInstallsObject AppInstalls.AppInstalls
  | BlackListOrgObject BlackListOrg.BlackListOrg
  | BookingObject Booking.Booking
  | BookingCancellationReasonObject BookingCancellationReason.BookingCancellationReason
  | CallbackRequestObject CallbackRequest.CallbackRequest
  | CallStatusObject CallStatus.CallStatus
  | CancellationReasonObject CancellationReason.CancellationReason
  | DriverOfferObject DriverOffer.DriverOffer
  | EstimateObject Estimate.Estimate
  | EstimateBreakupObject EstimateBreakup.EstimateBreakup
  | ExophoneObject Exophone.Exophone
  | FareBreakupObject FareBreakup.FareBreakup
  | GeometryObject Geometry.Geometry
  | IssueObject Issue.Issue
  | PlaceNameCacheObject PlaceNameCache.PlaceNameCache
  | MerchantObject Merchant.Merchant
  | MerchantMessageObject MerchantMessage.MerchantMessage
  | MerchantPaymentMethodObject MerchantPaymentMethod.MerchantPaymentMethod
  | MerchantServiceConfigObject MerchantServiceConfig.MerchantServiceConfig
  | MerchantServiceUsageConfigObject MerchantServiceUsageConfig.MerchantServiceUsageConfig
  | MerchantConfigObject MerchantConfig.MerchantConfig
  | OnSearchEventObject OnSearchEvent.OnSearchEvent
  | PaymentOrderObject PaymentOrder.PaymentOrder
  | PaymentTransactionObject PaymentTransaction.PaymentTransaction
  | PersonObject Person.Person
  | PersonDefaultEmergencyNumberObject PersonDefaultEmergencyNumber.PersonDefaultEmergencyNumber
  | PersonFlowStatusObject PersonFlowStatus.PersonFlowStatus
  | QuoteObject Quote.Quote
  | RegistrationTokenObject RegistrationToken.RegistrationToken
  | RentalSlabObject RentalSlab.RentalSlab
  | RatingObject Rating.Rating
  | RideObject Ride.Ride
  | SavedReqLocationObject SavedReqLocation.SavedReqLocation
  | SearchRequestObject SearchRequest.SearchRequest
  | SosObject Sos.Sos
  | SpecialZoneQuoteObject SpecialZoneQuote.SpecialZoneQuote
  | TripTermsObject TripTerms.TripTerms
  | WebengageObject Webengage.Webengage
  | FeedbackFormObject FeedbackForm.FeedbackForm
  | HotSpotConfigObject HotSpotConfig.HotSpotConfig
  | BecknRequestObject BecknRequest.BecknRequest
  | LocationObject Location.Location
  | LocationMappingObject LocationMapping.LocationMapping
  deriving (Generic, FromJSON, ToJSON, Show)

modelName :: DBCreateObject -> Text
modelName (AppInstallsObject _) = "AppInstalls"
modelName (BlackListOrgObject _) = "BlackListOrg"
modelName (BookingObject _) = "Booking"
modelName (BookingCancellationReasonObject _) = "BookingCancellationReason"
modelName (CallbackRequestObject _) = "CallbackRequest"
modelName (CallStatusObject _) = "CallStatus"
modelName (CancellationReasonObject _) = "CancellationReason"
modelName (DriverOfferObject _) = "DriverOffer"
modelName (EstimateObject _) = "Estimate"
modelName (EstimateBreakupObject _) = "EstimateBreakup"
modelName (ExophoneObject _) = "Exophone"
modelName (FareBreakupObject _) = "FareBreakup"
modelName (GeometryObject _) = "Geometry"
modelName (IssueObject _) = "Issue"
modelName (PlaceNameCacheObject _) = "PlaceNameCache"
modelName (MerchantObject _) = "Merchant"
modelName (MerchantMessageObject _) = "MerchantMessage"
modelName (MerchantPaymentMethodObject _) = "MerchantPaymentMethod"
modelName (MerchantServiceConfigObject _) = "MerchantServiceConfig"
modelName (MerchantServiceUsageConfigObject _) = "MerchantServiceUsageConfig"
modelName (MerchantConfigObject _) = "MerchantConfig"
modelName (OnSearchEventObject _) = "OnSearchEvent"
modelName (PaymentOrderObject _) = "PaymentOrder"
modelName (PaymentTransactionObject _) = "PaymentTransaction"
modelName (PersonObject _) = "Person"
modelName (PersonDefaultEmergencyNumberObject _) = "PersonDefaultEmergencyNumber"
modelName (PersonFlowStatusObject _) = "PersonFlowStatus"
modelName (QuoteObject _) = "Quote"
modelName (RegistrationTokenObject _) = "RegistrationToken"
modelName (RentalSlabObject _) = "RentalSlab"
modelName (RatingObject _) = "Rating"
modelName (RideObject _) = "Ride"
modelName (SavedReqLocationObject _) = "SavedReqLocation"
modelName (SearchRequestObject _) = "SearchRequest"
modelName (SosObject _) = "Sos"
modelName (SpecialZoneQuoteObject _) = "SpecialZoneQuote"
modelName (TripTermsObject _) = "TripTerms"
modelName (WebengageObject _) = "Webengage"
modelName (FeedbackFormObject _) = "FeedbackForm"
modelName (HotSpotConfigObject _) = "HotSpotConfig"
modelName (BecknRequestObject _) = "BecknRequest"
modelName (LocationObject _) = "Location"
modelName (LocationMappingObject _) = "LocationMapping"
