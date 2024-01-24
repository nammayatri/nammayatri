module Types.DBSync.Delete where

import Data.Aeson as A
import Data.Aeson.Types (Parser)
import qualified Data.Text as T
import Database.Beam.Postgres (Postgres)
import EulerHS.Prelude
import qualified IssueManagement.Storage.Beam.Issue.Comment as Comment
import qualified IssueManagement.Storage.Beam.Issue.IssueCategory as IssueCategory
import qualified IssueManagement.Storage.Beam.Issue.IssueOption as IssueOption
import qualified IssueManagement.Storage.Beam.Issue.IssueReport as IssueReport
import qualified IssueManagement.Storage.Beam.Issue.IssueTranslation as IssueTranslation
import qualified IssueManagement.Storage.Beam.MediaFile as MediaFile
import qualified Lib.Payment.Storage.Beam.PaymentOrder as PaymentOrder
import qualified Lib.Payment.Storage.Beam.PaymentTransaction as PaymentTransaction
import Sequelize
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
import "rider-app" Storage.Beam.IssueManagement ()
import qualified "rider-app" Storage.Beam.Location as Location
import qualified "rider-app" Storage.Beam.LocationMapping as LocationMapping
import qualified "rider-app" Storage.Beam.Maps.PlaceNameCache as PlaceNameCache
import qualified "rider-app" Storage.Beam.Merchant as Merchant
import qualified "rider-app" Storage.Beam.Merchant.MerchantMessage as MerchantMessage
import qualified "rider-app" Storage.Beam.Merchant.MerchantPaymentMethod as MerchantPaymentMethod
import qualified "rider-app" Storage.Beam.Merchant.MerchantServiceConfig as MerchantServiceConfig
import qualified "rider-app" Storage.Beam.Merchant.MerchantServiceUsageConfig as MerchantServiceUsageConfig
import qualified "rider-app" Storage.Beam.MerchantConfig as MerchantConfig
import qualified "rider-app" Storage.Beam.NextBillionData as NextBillionData
import qualified "rider-app" Storage.Beam.OnSearchEvent as OnSearchEvent
import qualified "rider-app" Storage.Beam.Payment ()
import qualified "rider-app" Storage.Beam.Person as Person
import qualified "rider-app" Storage.Beam.Person.PersonDefaultEmergencyNumber as PersonDefaultEmergencyNumber
import qualified "rider-app" Storage.Beam.Person.PersonFlowStatus as PersonFlowStatus
import qualified "rider-app" Storage.Beam.Quote as Quote
import qualified "rider-app" Storage.Beam.RegistrationToken as RegistrationToken
import qualified "rider-app" Storage.Beam.RentalDetails as RentalDetails
import qualified "rider-app" Storage.Beam.Ride as Ride
import qualified "rider-app" Storage.Beam.SavedReqLocation as SavedReqLocation
import qualified "rider-app" Storage.Beam.SearchRequest as SearchRequest
import qualified "rider-app" Storage.Beam.Sos as Sos
import qualified "rider-app" Storage.Beam.SpecialZoneQuote as SpecialZoneQuote
import qualified "rider-app" Storage.Beam.TripTerms as TripTerms
import Utils.Parse

data DeleteModel
  = AppInstallsDelete
  | BlackListOrgDelete
  | BookingDelete
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
  | CommentDelete
  | IssueCategoryDelete
  | IssueOptionDelete
  | IssueReportDelete
  | IssueTranslationDelete
  | PlaceNameCacheDelete
  | MerchantDelete
  | MerchantMessageDelete
  | MerchantPaymentMethodDelete
  | MerchantServiceConfigDelete
  | MerchantServiceUsageConfigDelete
  | MerchantConfigDelete
  | MediaFileDelete
  | OnSearchEventDelete
  | PaymentOrderDelete
  | PaymentTransactionDelete
  | PersonDelete
  | PersonDefaultEmergencyNumberDelete
  | PersonFlowStatusDelete
  | QuoteDelete
  | RegistrationTokenDelete
  | RentalDetailsDelete
  | RideDelete
  | SavedReqLocationDelete
  | SearchRequestDelete
  | SosDelete
  | SpecialZoneQuoteDelete
  | TripTermsDelete
  | FeedbackFormDelete
  | HotSpotConfigDelete
  | BecknRequestDelete
  | LocationDelete
  | LocationMappingDelete
  | NextBillionDataDelete
  deriving (Generic, Show)

getTagDelete :: DeleteModel -> Text
getTagDelete AppInstallsDelete = "AppInstallsOptions"
getTagDelete BlackListOrgDelete = "BlackListOrgOptions"
getTagDelete BookingDelete = "BookingOptions"
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
getTagDelete CommentDelete = "CommentOptions"
getTagDelete IssueCategoryDelete = "IssueCategoryOptions"
getTagDelete IssueOptionDelete = "IssueOptionOptions"
getTagDelete IssueReportDelete = "IssueReportOptions"
getTagDelete IssueTranslationDelete = "IssueTranslationOptions"
getTagDelete PlaceNameCacheDelete = "PlaceNameCacheOptions"
getTagDelete MerchantDelete = "MerchantOptions"
getTagDelete MerchantMessageDelete = "MerchantMessageOptions"
getTagDelete MerchantPaymentMethodDelete = "MerchantPaymentMethodOptions"
getTagDelete MerchantServiceConfigDelete = "MerchantServiceConfigOptions"
getTagDelete MerchantServiceUsageConfigDelete = "MerchantServiceUsageConfigOptions"
getTagDelete MerchantConfigDelete = "MerchantConfigOptions"
getTagDelete MediaFileDelete = "MediaFileOptions"
getTagDelete OnSearchEventDelete = "OnSearchEventOptions"
getTagDelete PaymentOrderDelete = "PaymentOrderOptions"
getTagDelete PaymentTransactionDelete = "PaymentTransactionOptions"
getTagDelete PersonDelete = "PersonOptions"
getTagDelete PersonDefaultEmergencyNumberDelete = "PersonDefaultEmergencyNumberOptions"
getTagDelete PersonFlowStatusDelete = "PersonFlowStatusOptions"
getTagDelete QuoteDelete = "QuoteOptions"
getTagDelete RegistrationTokenDelete = "RegistrationTokenOptions"
getTagDelete RentalDetailsDelete = "RentalDetailsOptions"
getTagDelete RideDelete = "RideOptions"
getTagDelete SavedReqLocationDelete = "SavedReqLocationOptions"
getTagDelete SearchRequestDelete = "SearchRequestOptions"
getTagDelete SosDelete = "SosOptions"
getTagDelete SpecialZoneQuoteDelete = "SpecialZoneQuoteOptions"
getTagDelete TripTermsDelete = "TripTermsOptions"
getTagDelete FeedbackFormDelete = "FeedbackFormOptions"
getTagDelete HotSpotConfigDelete = "HotSpotConfigOptions"
getTagDelete BecknRequestDelete = "BecknRequestOptions"
getTagDelete LocationDelete = "LocationOptions"
getTagDelete LocationMappingDelete = "LocationMappingOptions"
getTagDelete NextBillionDataDelete = "NextBillionDataDelete"

parseTagDelete :: Text -> Parser DeleteModel
parseTagDelete "AppInstallsOptions" = return AppInstallsDelete
parseTagDelete "BlackListOrgOptions" = return BlackListOrgDelete
parseTagDelete "BookingOptions" = return BookingDelete
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
parseTagDelete "CommentOptions" = return CommentDelete
parseTagDelete "IssueCategoryOptions" = return IssueCategoryDelete
parseTagDelete "IssueOptionOptions" = return IssueOptionDelete
parseTagDelete "IssueReportOptions" = return IssueReportDelete
parseTagDelete "IssueTranslationOptions" = return IssueTranslationDelete
parseTagDelete "PlaceNameCacheOptions" = return PlaceNameCacheDelete
parseTagDelete "MerchantOptions" = return MerchantDelete
parseTagDelete "MerchantMessageOptions" = return MerchantMessageDelete
parseTagDelete "MerchantPaymentMethodOptions" = return MerchantPaymentMethodDelete
parseTagDelete "MerchantServiceConfigOptions" = return MerchantServiceConfigDelete
parseTagDelete "MerchantServiceUsageConfigOptions" = return MerchantServiceUsageConfigDelete
parseTagDelete "MerchantConfigOptions" = return MerchantConfigDelete
parseTagDelete "MediaFileOptions" = return MediaFileDelete
parseTagDelete "OnSearchEventOptions" = return OnSearchEventDelete
parseTagDelete "PaymentOrderOptions" = return PaymentOrderDelete
parseTagDelete "PaymentTransactionOptions" = return PaymentTransactionDelete
parseTagDelete "PersonOptions" = return PersonDelete
parseTagDelete "PersonDefaultEmergencyNumberOptions" = return PersonDefaultEmergencyNumberDelete
parseTagDelete "PersonFlowStatusOptions" = return PersonFlowStatusDelete
parseTagDelete "QuoteOptions" = return QuoteDelete
parseTagDelete "RegistrationTokenOptions" = return RegistrationTokenDelete
parseTagDelete "RentalDetailsOptions" = return RentalDetailsDelete
parseTagDelete "RideOptions" = return RideDelete
parseTagDelete "SavedReqLocationOptions" = return SavedReqLocationDelete
parseTagDelete "SearchRequestOptions" = return SearchRequestDelete
parseTagDelete "SosOptions" = return SosDelete
parseTagDelete "SpecialZoneQuoteOptions" = return SpecialZoneQuoteDelete
parseTagDelete "TripTermsOptions" = return TripTermsDelete
parseTagDelete "FeedbackFormOptions" = return FeedbackFormDelete
parseTagDelete "HotSpotConfigOptions" = return HotSpotConfigDelete
parseTagDelete "BecknRequestOptions" = return BecknRequestDelete
parseTagDelete "LocationOptions" = return LocationDelete
parseTagDelete "LocationMappingOptions" = return LocationMappingDelete
parseTagDelete "NextBillionDataOptions" = return NextBillionDataDelete
parseTagDelete t = fail $ T.unpack ("Expected a DeleteModel but got '" <> t <> "'")

data DBDeleteObject
  = AppInstallsDeleteOptions DeleteModel (Where Postgres AppInstalls.AppInstallsT)
  | BlackListOrgDeleteOptions DeleteModel (Where Postgres BlackListOrg.BlackListOrgT)
  | BookingDeleteOptions DeleteModel (Where Postgres Booking.BookingT)
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
  | CommentDeleteOptions DeleteModel (Where Postgres Comment.CommentT)
  | IssueCategoryDeleteOptions DeleteModel (Where Postgres IssueCategory.IssueCategoryT)
  | IssueOptionDeleteOptions DeleteModel (Where Postgres IssueOption.IssueOptionT)
  | IssueReportDeleteOptions DeleteModel (Where Postgres IssueReport.IssueReportT)
  | IssueTranslationDeleteOptions DeleteModel (Where Postgres IssueTranslation.IssueTranslationT)
  | PlaceNameCacheDeleteOptions DeleteModel (Where Postgres PlaceNameCache.PlaceNameCacheT)
  | MerchantDeleteOptions DeleteModel (Where Postgres Merchant.MerchantT)
  | MerchantMessageDeleteOptions DeleteModel (Where Postgres MerchantMessage.MerchantMessageT)
  | MerchantPaymentMethodDeleteOptions DeleteModel (Where Postgres MerchantPaymentMethod.MerchantPaymentMethodT)
  | MerchantServiceConfigDeleteOptions DeleteModel (Where Postgres MerchantServiceConfig.MerchantServiceConfigT)
  | MerchantServiceUsageConfigDeleteOptions DeleteModel (Where Postgres MerchantServiceUsageConfig.MerchantServiceUsageConfigT)
  | MerchantConfigDeleteOptions DeleteModel (Where Postgres MerchantConfig.MerchantConfigT)
  | MediaFileDeleteOptions DeleteModel (Where Postgres MediaFile.MediaFileT)
  | OnSearchEventDeleteOptions DeleteModel (Where Postgres OnSearchEvent.OnSearchEventT)
  | PaymentOrderDeleteOptions DeleteModel (Where Postgres PaymentOrder.PaymentOrderT)
  | PaymentTransactionDeleteOptions DeleteModel (Where Postgres PaymentTransaction.PaymentTransactionT)
  | PersonDeleteOptions DeleteModel (Where Postgres Person.PersonT)
  | PersonDefaultEmergencyNumberDeleteOptions DeleteModel (Where Postgres PersonDefaultEmergencyNumber.PersonDefaultEmergencyNumberT)
  | PersonFlowStatusDeleteOptions DeleteModel (Where Postgres PersonFlowStatus.PersonFlowStatusT)
  | QuoteDeleteOptions DeleteModel (Where Postgres Quote.QuoteT)
  | RegistrationTokenDeleteOptions DeleteModel (Where Postgres RegistrationToken.RegistrationTokenT)
  | RentalDetailsDeleteOptions DeleteModel (Where Postgres RentalDetails.RentalDetailsT)
  | RideDeleteOptions DeleteModel (Where Postgres Ride.RideT)
  | SavedReqLocationDeleteOptions DeleteModel (Where Postgres SavedReqLocation.SavedReqLocationT)
  | SearchRequestDeleteOptions DeleteModel (Where Postgres SearchRequest.SearchRequestT)
  | SosDeleteOptions DeleteModel (Where Postgres Sos.SosT)
  | SpecialZoneQuoteDeleteOptions DeleteModel (Where Postgres SpecialZoneQuote.SpecialZoneQuoteT)
  | TripTermsDeleteOptions DeleteModel (Where Postgres TripTerms.TripTermsT)
  | FeedbackFormDeleteOptions DeleteModel (Where Postgres FeedbackForm.FeedbackFormT)
  | HotSpotConfigDeleteOptions DeleteModel (Where Postgres HotSpotConfig.HotSpotConfigT)
  | BecknRequestDeleteOptions DeleteModel (Where Postgres BecknRequest.BecknRequestT)
  | LocationDeleteOptions DeleteModel (Where Postgres Location.LocationT)
  | LocationMappingDeleteOptions DeleteModel (Where Postgres LocationMapping.LocationMappingT)
  | NextBillionDataDeleteOptions DeleteModel (Where Postgres NextBillionData.NextBillionDataT)

instance ToJSON DBDeleteObject where
  toJSON = error "ToJSON not implemented for DBDeleteObject - Use getDbDeleteCommandJson instead" -- Using getDbDeleteCommandJson instead of toJSON

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
      MediaFileDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ MediaFileDeleteOptions deleteModel whereClause
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
      RentalDetailsDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ RentalDetailsDeleteOptions deleteModel whereClause
      RideDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ RideDeleteOptions deleteModel whereClause
      SavedReqLocationDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ SavedReqLocationDeleteOptions deleteModel whereClause
      SearchRequestDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ SearchRequestDeleteOptions deleteModel whereClause
      SosDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ SosDeleteOptions deleteModel whereClause
      SpecialZoneQuoteDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ SpecialZoneQuoteDeleteOptions deleteModel whereClause
      TripTermsDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ TripTermsDeleteOptions deleteModel whereClause
      FeedbackFormDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ FeedbackFormDeleteOptions deleteModel whereClause
      HotSpotConfigDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ HotSpotConfigDeleteOptions deleteModel whereClause
      BecknRequestDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ BecknRequestDeleteOptions deleteModel whereClause
      LocationDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ LocationDeleteOptions deleteModel whereClause
      LocationMappingDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ LocationMappingDeleteOptions deleteModel whereClause
      NextBillionDataDelete -> do
        whereClause <- parseDeleteCommandValues contents
        return $ NextBillionDataDeleteOptions deleteModel whereClause
