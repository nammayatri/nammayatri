{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.DBSync.DBModel (module Types.DBSync.DBModel, module Reexport) where

import qualified Data.Aeson as A
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Kernel.Beam.Lib.UtilsTH as Reexport (IsDBTable (..))
import Kernel.Prelude
import qualified "rider-app" Storage.Beam.AadhaarVerification.AadhaarOtpReq as AadhaarOtpReq
import qualified "rider-app" Storage.Beam.AadhaarVerification.AadhaarOtpVerify as AadhaarOtpVerify
import qualified "rider-app" Storage.Beam.AadhaarVerification.AadhaarVerification as AadhaarVerification
import qualified "rider-app" Storage.Beam.AppInstalls as AppInstalls
import qualified "rider-app" Storage.Beam.BecknRequest as BecknRequest
import qualified "rider-app" Storage.Beam.BlackListOrg as BlackListOrg
import qualified "rider-app" Storage.Beam.Booking as Booking
import qualified "rider-app" Storage.Beam.Booking.BookingLocation as BookingLocation
import qualified "rider-app" Storage.Beam.BookingCancellationReason as BookingCancellationReason
import qualified "rider-app" Storage.Beam.CallStatus as CallStatus
import qualified "rider-app" Storage.Beam.CallbackRequest as CallbackRequest
import qualified "rider-app" Storage.Beam.CancellationReason as CancellationReason
import qualified "rider-app" Storage.Beam.Disability as Disability
import qualified "rider-app" Storage.Beam.DisabilityTranslation as DisabilityTranslation
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
import qualified "rider-app" Storage.Beam.Maps.DirectionsCache as DirectionsCache
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
import qualified "rider-app" Storage.Beam.Person.PersonDisability as PersonDisability
import qualified "rider-app" Storage.Beam.Person.PersonFlowStatus as PersonFlowStatus
import qualified "rider-app" Storage.Beam.Person.PersonStats as PersonStats
import qualified "rider-app" Storage.Beam.Quote as Quote
import qualified "rider-app" Storage.Beam.Rating as Rating
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
import qualified "rider-app" Storage.DBModel as DBModel
import Utils.TH

$(genDBModelObjects ''DBModel.RiderApp ''DBModel.DBModel)

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

showDBModel :: forall table. IsDBTable DBModel.RiderApp table => Proxy table -> Text
showDBModel _ = show $ getDBModel (Proxy @DBModel.RiderApp) (Proxy @table)

-- Autogenerated code example for three tables:

-- testSplice :: IO ()
-- testSplice = do
--   putStrLn @String $((TH.stringE . TH.pprint) =<< genDBModelObjects ''DBModel.RiderApp ''DBModel.DBModel)

-- data DBObject (f :: TableK -> Type)
--   = AppInstallsObject (f AppInstalls.AppInstallsT)
--   | BlackListOrgObject (f BlackListOrg.BlackListOrgT)
--   | BookingObject (f Booking.BookingT)
--   deriving (Generic)

-- buildDBObject ::
--   forall (f :: TableK -> Type) (m :: Type -> Type).
--   Functor m =>
--   DBModel ->
--   (forall t. IsDBTable DBModel.RiderApp t => m (f t)) ->
--   m (DBObject f)
-- buildDBObject dbModel tableAction = case dbModel of
--   AppInstalls -> AppInstallsObject <$> (tableAction @AppInstalls.AppInstallsT)
--   BlackListOrg -> BlackListOrgObject <$> (tableAction @BlackListOrg.BlackListOrgT)
--   Booking -> BookingObject <$> (tableAction @Booking.BookingT)

-- withDBObjectContent ::
--   forall (f :: TableK -> Type) (res :: Type).
--   DBObject f ->
--   (forall t. IsDBTable DBModel.RiderApp t => f t -> res) ->
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
--   (forall t. IsDBTable DBModel.RiderApp t => [(f t, payload)] -> res) ->
--   res
-- withFilteredDBObjectContent dbModel dbObjectsWithPayload action = do
--   case dbModel of
--     AppInstalls -> action @AppInstalls.AppInstallsT [(obj, payload) | (AppInstallsObject obj, payload) <- dbObjectsWithPayload]
--     BlackListOrg -> action @BlackListOrg.BlackListOrgT [(obj, payload) | (BlackListOrgObject obj, payload) <- dbObjectsWithPayload]
--     Booking -> action @Booking.BookingT [(obj, payload) | (BookingObject obj, payload) <- dbObjectsWithPayload]
