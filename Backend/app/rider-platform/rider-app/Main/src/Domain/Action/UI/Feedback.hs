{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Domain.Action.UI.Feedback
  ( FeedbackReq (..),
    FeedbackAnswer (..),
    FeedbackRes (..),
    DriverProfileResponse (..),
    feedback,
    knowYourFavDriver,
    knowYourDriver,
    audioFeedbackUpload,
  )
where

import qualified AWS.S3 as S3
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Domain.Types.Booking as DBooking
import Domain.Types.FeedbackForm (BadgeDetail (..), FeedbackForm)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as Person
import qualified Domain.Types.PersonFlowStatus as DPFS
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideStatus as DRide
import qualified Domain.Types.ServiceTierType as DVST
import qualified Domain.Types.VehicleVariant as DVeh
import Environment
import qualified Environment as App
import qualified EulerHS.Language as L
import qualified IssueManagement.Common as IC
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Ticket.Interface.Types as Ticket
import Kernel.External.Types (Language (ENGLISH))
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import SharedLogic.Person as SLP
import qualified SharedLogic.Scheduler.Jobs.SafetyCSAlert as SIVR
import qualified Slack.Flow as Slack
import Storage.Beam.IssueManagement ()
import qualified Storage.CachedQueries.FeedbackForm as CQFF
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.RiderConfig (RiderDimensions (..))
import Storage.ConfigPilot.Interface.Types (getConfig)
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Issue as QIssue
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.Ticket as Ticket

data FeedbackAnswer = FeedbackAnswer
  { questionId :: Text,
    answer :: [Text]
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data FeedbackReq = FeedbackReq
  { rideId :: Id DRide.Ride,
    rating :: Int,
    feedbackDetails :: Maybe Text,
    wasRideSafe :: Maybe Bool,
    shouldFavDriver :: Maybe Bool,
    wasOfferedAssistance :: Maybe Bool,
    mbAudio :: Maybe Text,
    feedbackAnswers :: Maybe [FeedbackAnswer] -- Question-based feedback with badge keys
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data FeedbackRes = FeedbackRes
  { bppBookingId :: Id DBooking.BPPBooking,
    ratingValue :: Int,
    feedbackDetails :: Maybe Text,
    providerId :: Text,
    providerUrl :: BaseUrl,
    transactionId :: Text,
    merchant :: DM.Merchant,
    wasOfferedAssistance :: Maybe Bool,
    city :: Context.City,
    issueId :: Maybe Text,
    vehicleVariant :: DVeh.VehicleVariant,
    vehicleServiceTierType :: DVST.ServiceTierType,
    shouldFavDriver :: Maybe Bool,
    riderPhoneNum :: Maybe Text,
    isValueAddNP :: Bool,
    filePath :: Maybe Text,
    riderName :: Text,
    badgeMetadata :: Maybe [CallBPPInternal.BadgeMetadata],
    bppRideId :: Id DRide.BPPRide
  }

data DriverProfileResponse = DriverProfileResponse
  { response :: Maybe CallBPPInternal.DriverProfileRes
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

feedback :: FeedbackReq -> Id Person.Person -> App.Flow FeedbackRes
feedback request personId = do
  let ratingValue = request.rating
  unless (ratingValue `elem` [1 .. 5]) $ throwError InvalidRatingValue
  let rideId = request.rideId
      feedbackDetails = request.feedbackDetails
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  unless (ride.status == DRide.COMPLETED) $ throwError (RideInvalidStatus "Feedback available only for completed rides.")
  booking <- QRB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  riderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId}) >>= fromMaybeM (RiderConfigDoesNotExist booking.merchantOperatingCityId.getId)
  bppBookingId <- booking.bppBookingId & fromMaybeM (BookingFieldNotPresent "bppBookingId")
  let bppRideId = ride.bppRideId
  badgeMetadata <- case request.feedbackAnswers of
    Just feedbackAnswers
      | not (null feedbackAnswers) ->
        Just <$> extractBadgeMetadata ratingValue feedbackAnswers
    _ -> pure Nothing
  filePath <- case request.mbAudio of
    Just audio -> do
      let fileType = S3.Audio
          contentType = T.pack "mp3"
      filePath <- S3.createFilePath "/feedback-media/" ("bppBookingId-" <> bppBookingId.getId) fileType contentType
      void $ audioFeedbackUpload (personId, merchant.id) audio filePath
      return $ Just filePath
    Nothing -> do
      return Nothing
  issueId' <- getIssueIdForRide booking
  _ <- QPFS.updateStatus booking.riderId DPFS.IDLE
  _ <- QRide.updateRideRating rideId ratingValue request.wasRideSafe
  let merchantOperatingCityId = booking.merchantOperatingCityId
  city <- CQMOC.findById merchantOperatingCityId >>= fmap (.city) . fromMaybeM (MerchantOperatingCityNotFound merchantOperatingCityId.getId)
  person <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  isValueAddNP <- CQVAN.isValueAddNP booking.providerId
  unencryptedMobileNumber <- mapM decrypt person.mobileNumber
  let isLOFeedback = ratingValue <= riderConfig.feedbackAlertRatingThreshold && IC.checkForLOFeedback riderConfig.sensitiveWords riderConfig.sensitiveWordsForExactMatch feedbackDetails
  when isLOFeedback $
    fork "Creating ticket and notifying on slack" $ do
      phoneNumber <- mapM decrypt person.mobileNumber
      let rideInfo = SIVR.buildRideInfo ride person phoneNumber
          queue = fromMaybe riderConfig.kaptureConfig.queue riderConfig.kaptureConfig.l0FeedbackQueue
          ticketReq = mkTicket person phoneNumber rideInfo filePath riderConfig.kaptureConfig.disposition queue
      createTicketResp <- withTryCatch "createTicket:feedback" $ Ticket.createTicket merchant.id merchantOperatingCityId ticketReq
      case createTicketResp of
        Left err -> logTagError "Create Ticket API failed - " $ show err
        Right resp -> logTagInfo "Created Ticket for Customer L0 Feedback : TicketId - " resp.ticketId
      slackConfig <- asks (.slackNotificationConfig)
      desc <- generateSlackMessage person ride unencryptedMobileNumber (T.pack $ show city) request.rating feedbackDetails
      let message = createJsonMessage desc
      void $ L.runIO $ Slack.publishMessage slackConfig message
  pure
    FeedbackRes
      { wasOfferedAssistance = request.wasOfferedAssistance,
        providerId = booking.providerId,
        providerUrl = booking.providerUrl,
        transactionId = booking.transactionId,
        issueId = issueId',
        vehicleVariant = DVeh.castServiceTierToVariant booking.vehicleServiceTierType,
        vehicleServiceTierType = booking.vehicleServiceTierType,
        shouldFavDriver = request.shouldFavDriver,
        riderPhoneNum = unencryptedMobileNumber,
        riderName = fromMaybe "User" person.firstName,
        badgeMetadata = badgeMetadata,
        ..
      }
  where
    getIssueIdForRide booking = do
      res <- QIssue.findNightIssueByBookingId booking.id
      case res of
        Just issue -> return $ Just issue.id.getId
        Nothing -> return Nothing

    createJsonMessage :: Text -> T.Text
    createJsonMessage descriptionText =
      let jsonValue :: A.Value
          jsonValue =
            A.object
              [ "version" A..= ("1.0" :: String),
                "source" A..= ("custom" :: String),
                "content"
                  A..= A.object
                    [ "description" A..= descriptionText
                    ]
              ]
       in TL.toStrict $ TLE.decodeUtf8 $ A.encode jsonValue

    mkTicket :: Person.Person -> Maybe Text -> Ticket.RideInfo -> Maybe Text -> Text -> Text -> Ticket.CreateTicketReq
    mkTicket person phoneNumber info mediaFileUrl disposition queue = do
      Ticket.CreateTicketReq
        { category = "Customer Feedback",
          subCategory = Just "L0 Feedback",
          issueId = Nothing,
          issueDescription = fromMaybe "" request.feedbackDetails,
          mediaFiles = mediaFileUrl <&> (: []),
          name = Just $ SLP.getName person,
          phoneNo = phoneNumber,
          personId = person.id.getId,
          classification = Ticket.CUSTOMER,
          rideDescription = Just info,
          disposition,
          queue,
          becknIssueId = Nothing
        }

    generateSlackMessage :: Person.Person -> DRide.Ride -> Maybe Text -> Text -> Int -> Maybe Text -> Flow Text
    generateSlackMessage person ride mbCustomerPhone city rating feedbackDetails = do
      now <- getCurrentTime
      mbDriverPhoneNumber <- mapM decrypt ride.driverPhoneNumber
      let customerPhone = fromMaybe "N/A" mbCustomerPhone
          customerName = SLP.getName person
          driverPhoneNumber = fromMaybe "N/A" mbDriverPhoneNumber
          dropLocation = fromMaybe "N/A" (ride.toLocation >>= \loc -> Just ("area - " <> fromMaybe "N/A" (loc.address.area) <> ", street - " <> fromMaybe "N/A" (loc.address.street)))
          fromLocation = ride.fromLocation
          pickupLocation = "area - " <> fromMaybe "N/A" (fromLocation.address.area) <> ", street - " <> fromMaybe "N/A" (fromLocation.address.street)
          feedbackMsg = fromMaybe "N/A" feedbackDetails
          rideStartTime = maybe "N/A" showTimeIst ride.rideStartTime
          currentTimeInIST = showTimeIst now
      return $
        "There is an L0 feedback/report given by Customer Name : " <> customerName <> "\n"
          <> "Customer Phone No : "
          <> customerPhone
          <> "\n"
          <> "Driver Name : "
          <> ride.driverName
          <> "\n"
          <> "Driver Phone No : "
          <> driverPhoneNumber
          <> "\n"
          <> "City : "
          <> city
          <> "\n"
          <> "Variant : "
          <> show ride.vehicleVariant
          <> "\n"
          <> "Pickup Location : "
          <> pickupLocation
          <> "\n"
          <> "Drop Location : "
          <> dropLocation
          <> "\n"
          <> "Ride Id : "
          <> ride.id.getId
          <> "\n"
          <> "Ride Start Time : "
          <> rideStartTime
          <> "\n"
          <> "Feedback Time : "
          <> currentTimeInIST
          <> "\n"
          <> "Feedback Rating : "
          <> show rating
          <> "\n"
          <> "Feedback Message : "
          <> feedbackMsg
          <> "\n"

knowYourDriver :: Id DRide.Ride -> Maybe Bool -> App.Flow DriverProfileResponse
knowYourDriver rideId withImages = do
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- QRB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  isValueAddNP <- CQVAN.isValueAddNP booking.providerId
  if isValueAddNP
    then do
      res <- CallBPPInternal.getknowYourDriverDetails merchant.driverOfferApiKey merchant.driverOfferBaseUrl ride.bppRideId.getId withImages
      pure $ DriverProfileResponse {response = Just res}
    else pure $ DriverProfileResponse {response = Nothing}

knowYourFavDriver :: Text -> Id DM.Merchant -> Maybe Bool -> App.Flow DriverProfileResponse
knowYourFavDriver driverId merchantId withImages = do
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  res <- CallBPPInternal.getKnowYourFavDriverDetails merchant.driverOfferApiKey merchant.driverOfferBaseUrl driverId withImages
  pure $ DriverProfileResponse {response = Just res}

audioFeedbackUpload ::
  (Id Person.Person, Id DM.Merchant) ->
  Text ->
  Text ->
  App.Flow ()
audioFeedbackUpload (_personId, merchantId) audio filePath = do
  merchant <- CQM.findById (cast merchantId) >>= fromMaybeM (MerchantNotFound merchantId.getId)
  let fileSize = getFileSize audio
  when (fileSize > merchant.mediaFileSizeUpperLimit) $
    throwError $ FileSizeExceededError (show fileSize)
  _ <- fork "S3 Put Feedback Media File" $ S3.put (T.unpack filePath) audio
  pure ()
  where
    getFileSize :: Text -> Int
    getFileSize = B.length . TE.encodeUtf8

extractBadgeMetadata :: Int -> [FeedbackAnswer] -> App.Flow [CallBPPInternal.BadgeMetadata]
extractBadgeMetadata rating feedbackAnswers = do
  feedbackForms <- CQFF.findAllFeedbackByRating rating
  let badgeMetadataList = concatMap (extractFromAnswerKeys feedbackForms) feedbackAnswers
  pure badgeMetadataList

extractFromAnswerKeys :: [FeedbackForm] -> FeedbackAnswer -> [CallBPPInternal.BadgeMetadata]
extractFromAnswerKeys feedbackForms (FeedbackAnswer questionId badgeKeys) =
  case find (\ff -> ff.id.getId == questionId) feedbackForms of
    Nothing -> []
    Just ff ->
      case ff.badges of
        Nothing -> []
        Just badgeDetails ->
          mapMaybe (findByBadgeKey badgeKeys) badgeDetails

findByBadgeKey :: [Text] -> BadgeDetail -> Maybe CallBPPInternal.BadgeMetadata
findByBadgeKey selectedBadgeKeys badgeDetail =
  if badgeDetail.key `elem` selectedBadgeKeys
    then
      Just $
        CallBPPInternal.BadgeMetadata
          { badgeKey = badgeDetail.key,
            sendPN = badgeDetail.sendPN,
            priority = badgeDetail.priority,
            badgeText = extractEnglishTranslation badgeDetail.contentWithTranslations badgeDetail.key
          }
    else Nothing

extractEnglishTranslation :: Maybe [IC.Translation] -> Text -> Text
extractEnglishTranslation mbTranslations fallbackKey =
  case mbTranslations of
    Nothing -> fallbackKey
    Just translations ->
      case find (\t -> t.language == ENGLISH) translations of
        Just t -> IC.translation t
        Nothing -> fallbackKey
