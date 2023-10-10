{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.DBSync.DBModel (module Types.DBSync.DBModel, module Reexport) where

import qualified Data.Aeson as A
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Kernel.Beam.Lib.UtilsTH as Reexport (IsDBTable (..))
import Kernel.Prelude
import qualified "dynamic-offer-driver-app" Storage.Beam.BapMetadata as BapMetadata
import qualified "dynamic-offer-driver-app" Storage.Beam.BecknRequest as BecknRequest
import qualified "dynamic-offer-driver-app" Storage.Beam.Booking as Booking
import qualified "dynamic-offer-driver-app" Storage.Beam.Booking.BookingLocation as BookingLocation
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
import qualified "dynamic-offer-driver-app" Storage.Beam.DriverPlan as DriverPlan
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
import qualified "dynamic-offer-driver-app" Storage.Beam.FarePolicy.FarePolicySlabDetails.FarePolicySlabDetailsSlab as FarePolicySlabsDetailsSlab
import qualified "dynamic-offer-driver-app" Storage.Beam.FarePolicy.RestrictedExtraFare as RestrictedExtraFare
import qualified "dynamic-offer-driver-app" Storage.Beam.FareProduct as FareProduct
import qualified "dynamic-offer-driver-app" Storage.Beam.Feedback.Feedback as Feedback
import qualified "dynamic-offer-driver-app" Storage.Beam.Feedback.FeedbackBadge as FeedbackBadge
import qualified "dynamic-offer-driver-app" Storage.Beam.Feedback.FeedbackForm as FeedbackForm
import qualified "dynamic-offer-driver-app" Storage.Beam.Geometry as Geometry
import qualified "dynamic-offer-driver-app" Storage.Beam.GoHomeConfig as GoHomeConfig
import qualified "dynamic-offer-driver-app" Storage.Beam.Invoice as Invoice
import qualified "dynamic-offer-driver-app" Storage.Beam.Issue.Comment as Comment
import qualified "dynamic-offer-driver-app" Storage.Beam.Issue.IssueCategory as IssueCategory
import qualified "dynamic-offer-driver-app" Storage.Beam.Issue.IssueOption as IssueOption
import qualified "dynamic-offer-driver-app" Storage.Beam.Issue.IssueReport as IssueReport
import qualified "dynamic-offer-driver-app" Storage.Beam.Issue.IssueTranslation as IssueTranslation
import qualified "dynamic-offer-driver-app" Storage.Beam.KioskLocation as KioskLocation
import qualified "dynamic-offer-driver-app" Storage.Beam.KioskLocationTranslation as KioskLocationTranslation
import qualified "dynamic-offer-driver-app" Storage.Beam.Location as Location
import qualified "dynamic-offer-driver-app" Storage.Beam.LocationMapping as LocationMapping
import qualified "dynamic-offer-driver-app" Storage.Beam.Mandate as Mandate
import qualified "dynamic-offer-driver-app" Storage.Beam.Maps.PlaceNameCache as PlaceNameCache
import qualified "dynamic-offer-driver-app" Storage.Beam.MediaFile as MediaFile
import qualified "dynamic-offer-driver-app" Storage.Beam.Merchant as Merchant
import qualified "dynamic-offer-driver-app" Storage.Beam.Merchant.DriverIntelligentPoolConfig as DriverIntelligentPoolConfig
import qualified "dynamic-offer-driver-app" Storage.Beam.Merchant.DriverPoolConfig as DriverPoolConfig
import qualified "dynamic-offer-driver-app" Storage.Beam.Merchant.LeaderBoardConfig as LeaderBoardConfigs
import qualified "dynamic-offer-driver-app" Storage.Beam.Merchant.MerchantMessage as MerchantMessage
import qualified "dynamic-offer-driver-app" Storage.Beam.Merchant.MerchantPaymentMethod as MerchantPaymentMethod
import qualified "dynamic-offer-driver-app" Storage.Beam.Merchant.MerchantServiceConfig as MerchantServiceConfig
import qualified "dynamic-offer-driver-app" Storage.Beam.Merchant.MerchantServiceUsageConfig as MerchantServiceUsageConfig
import qualified "dynamic-offer-driver-app" Storage.Beam.Merchant.OnboardingDocumentConfig as OnboardingDocumentConfig
import qualified "dynamic-offer-driver-app" Storage.Beam.Merchant.Overlay as Overlay
import qualified "dynamic-offer-driver-app" Storage.Beam.Merchant.TransporterConfig as TransporterConfig
import qualified "dynamic-offer-driver-app" Storage.Beam.Message.Message as Message
import qualified "dynamic-offer-driver-app" Storage.Beam.Message.MessageReport as MessageReport
import qualified "dynamic-offer-driver-app" Storage.Beam.Message.MessageTranslation as MessageTranslation
import qualified "dynamic-offer-driver-app" Storage.Beam.MetaData as MetaData
import qualified "dynamic-offer-driver-app" Storage.Beam.Notification as Notification
import qualified "dynamic-offer-driver-app" Storage.Beam.Person as Person
import qualified "dynamic-offer-driver-app" Storage.Beam.Plan as Plan
import qualified "dynamic-offer-driver-app" Storage.Beam.PlanTranslation as PlanTranslation
import qualified "dynamic-offer-driver-app" Storage.Beam.QuoteSpecialZone as QuoteSpecialZone
import qualified "dynamic-offer-driver-app" Storage.Beam.Rating as Rating
import qualified "dynamic-offer-driver-app" Storage.Beam.RegistrationToken as RegistrationToken
import qualified "dynamic-offer-driver-app" Storage.Beam.RegistryMapFallback as RegistryMapFallback
import qualified "dynamic-offer-driver-app" Storage.Beam.Ride.Table as Ride
import qualified "dynamic-offer-driver-app" Storage.Beam.RideDetails as RideDetails
import qualified "dynamic-offer-driver-app" Storage.Beam.RiderDetails as RiderDetails
import qualified "dynamic-offer-driver-app" Storage.Beam.SearchRequest as SearchRequest
import qualified "dynamic-offer-driver-app" Storage.Beam.SearchRequest.SearchReqLocation as SearchReqLocation
import qualified "dynamic-offer-driver-app" Storage.Beam.SearchRequestForDriver as SearchRequestForDriver
import qualified "dynamic-offer-driver-app" Storage.Beam.SearchRequestSpecialZone as SearchRequestSpecialZone
import qualified "dynamic-offer-driver-app" Storage.Beam.SearchTry as SearchTry
import qualified "dynamic-offer-driver-app" Storage.Beam.Vehicle as Vehicle
import qualified "dynamic-offer-driver-app" Storage.Beam.Volunteer as Volunteer
import qualified "dynamic-offer-driver-app" Storage.DBModel as DBModel
import Utils.TH

$(genDBModelObjects ''DBModel.DriverApp ''DBModel.DBModel)

availableDBModels :: NE.NonEmpty DBModel.DBModel
availableDBModels = fromMaybe (pure minBound) $ NE.nonEmpty [minBound .. maxBound]

type TableK = (Type -> Type) -> Type

newtype DBModelOptions = DBModelOptions {getDBModelOptions :: DBModel.DBModel}

instance FromJSON DBModelOptions where
  parseJSON = A.withText "DBModelOptions" $ \options -> case dropSuffix "Options" options of
    Nothing -> fail $ T.unpack ("Expected a DBModelOptions but got '" <> options <> "'")
    Just model -> DBModelOptions <$> parseJSON (A.String model)

-- dropSuffix "tableOptions" = Just "table"
dropSuffix :: Text -> Text -> Maybe Text
dropSuffix suffix str = do
  let l = T.length str - T.length suffix
  if T.drop l str == suffix then Just $ T.take l str else Nothing

showDBModel :: forall table. IsDBTable DBModel.DriverApp table => Proxy table -> Text
showDBModel _ = show $ getDBModel (Proxy @DBModel.DriverApp) (Proxy @table)

-- Autogenerated code example for three tables:

-- testSplice :: IO ()
-- testSplice = do
--   putStrLn @String $((TH.stringE . TH.pprint) =<< genDBModelObjects ''DBModel.DriverApp ''DBModel.DBModel)

-- data DBObject (f :: TableK -> Type)
--   = AppInstallsObject (f AppInstalls.AppInstallsT)
--   | BlackListOrgObject (f BlackListOrg.BlackListOrgT)
--   | BookingObject (f Booking.BookingT)
--   deriving (Generic)

-- buildDBObject ::
--   forall (f :: TableK -> Type) (m :: Type -> Type).
--   Functor m =>
--   DBModel ->
--   (forall t. IsDBTable DBModel.DriverApp t => m (f t)) ->
--   m (DBObject f)
-- buildDBObject dbModel tableAction = case dbModel of
--   AppInstalls -> AppInstallsObject <$> (tableAction @AppInstalls.AppInstallsT)
--   BlackListOrg -> BlackListOrgObject <$> (tableAction @BlackListOrg.BlackListOrgT)
--   Booking -> BookingObject <$> (tableAction @Booking.BookingT)

-- withDBObjectContent ::
--   forall (f :: TableK -> Type) (res :: Type).
--   DBObject f ->
--   (forall t. IsDBTable DBModel.DriverApp t => f t -> res) ->
--   res
-- withDBObjectContent dbObject action = do
--   case dbObject of
--     AppInstallsObject obj -> action @AppInstalls.AppInstallsT obj
--     BlackListOrgObject obj -> action @BlackListOrg.BlackListOrgT obj
--     BookingObject obj -> action @Booking.BookingT obj

-- withFilteredDBObjectContent ::
--   forall (f :: TableK -> Type) (res :: Type) (payload :: Type).
--   DBModel ->
--   [(DBObject f, payload)] ->
--   (forall t. IsDBTable DBModel.DriverApp t => [(f t, payload)] -> res) ->
--   res
-- withFilteredDBObjectContent dbModel dbObjectsWithPayload action = do
--   case dbModel of
--     AppInstalls -> action @AppInstalls.AppInstallsT [(obj, payload) | (AppInstallsObject obj, payload) <- dbObjectsWithPayload]
--     BlackListOrg -> action @BlackListOrg.BlackListOrgT [(obj, payload) | (BlackListOrgObject obj, payload) <- dbObjectsWithPayload]
--     Booking -> action @Booking.BookingT [(obj, payload) | (BookingObject obj, payload) <- dbObjectsWithPayload]
