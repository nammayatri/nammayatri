module Domain.Action.UI.DriverProfileQuestions where

import qualified API.Types.UI.DriverProfileQuestions
import ChatCompletion.Interface.Types as CIT
import qualified Data.Text as T
import Data.Time.Clock (utctDay)
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Domain.Types.DocumentVerificationConfig as DTO
import qualified Domain.Types.DriverProfileQuestions as DTDPQ
import qualified Domain.Types.DriverStats as DTS
import qualified Domain.Types.Image as DImage
import Domain.Types.LlmPrompt as DTL
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DOSC
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id)
import qualified IssueManagement.Storage.Queries.MediaFile as QMF
import Kernel.Beam.Functions
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common as KUC
import Storage.Beam.IssueManagement ()
import qualified Storage.Cac.MerchantServiceUsageConfig as QOMC
import Storage.CachedQueries.LLMPrompt.LLMPrompt as SCL
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Flow as Storage
import qualified Storage.Queries.DriverProfileQuestions as DPQ
import qualified Storage.Queries.DriverStats as QDS
import qualified Storage.Queries.Image as ImageQuery
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVehicle
import Tools.ChatCompletion as TC
import Tools.Error

data ImageType = JPG | PNG | UNKNOWN deriving (Generic, Show, Eq)

postDriverProfileQues ::
  ( ( Maybe (Id SP.Person),
      Id DM.Merchant,
      Id DMOC.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverProfileQuestions.DriverProfileQuesReq ->
    Flow APISuccess
  )
postDriverProfileQues (mbPersonId, merchantId, merchantOpCityId) req@API.Types.UI.DriverProfileQuestions.DriverProfileQuesReq {..} =
  do
    driverId <- mbPersonId & fromMaybeM (PersonNotFound "No person id passed")
    person <- QP.findById driverId >>= fromMaybeM (PersonNotFound ("No person found with id" <> show driverId))
    driverStats <- QDS.findByPrimaryKey driverId >>= fromMaybeM (PersonNotFound ("No person found with id" <> show driverId))
    now <- getCurrentTime
    fork "generating about_me" $ do
      aboutMe <- generateAboutMe person driverStats req
      DPQ.upsert
        ( DTDPQ.DriverProfileQuestions
            { updatedAt = now,
              createdAt = now,
              driverId = driverId,
              hometown = hometown,
              merchantOperatingCityId = merchantOpCityId,
              pledges = pledges,
              aspirations = toMaybe aspirations,
              drivingSince = drivingSince,
              imageIds = toMaybe imageIds,
              vehicleTags = toMaybe vehicleTags,
              aboutMe = Just aboutMe
            }
        )
    pure Success
  where
    toMaybe xs = guard (not (null xs)) >> Just xs

    generateAboutMe person driverStats req' = do
      gptGenProfile <- try $ genAboutMeWithAI person driverStats req'
      either
        (\(err :: SomeException) -> logError ("Error occurred: " <> show err) *> pure (hometownDetails req'.hometown <> "I have been part of this platform since " <> (joinedNY person.createdAt) <> ". " <> writeDriverStats driverStats <> genAspirations req'.aspirations))
        pure
        gptGenProfile

    hometownDetails mHometown = case mHometown of
      Just hometown' -> "Hailing from " <> hometown' <> ", "
      Nothing -> ""

    joinedNY :: UTCTime -> Text
    joinedNY createdAt = T.pack $ formatTime defaultTimeLocale "%b %Y" (utctDay createdAt)

    writeDriverStats driverStats = ratingStat driverStats <> cancellationStat driverStats

    nonZero Nothing = 1
    nonZero (Just a)
      | a <= 0 = 1
      | otherwise = a

    ratingStat driverStats =
      if driverStats.rating > Just 4.79 && isJust driverStats.rating
        then "I rank among the top 10 percentile in terms of rating "
        else ""

    cancellationStat driverStats =
      let cancRate = div ((fromMaybe 0 driverStats.ridesCancelled) * 100 :: Int) (nonZero driverStats.totalRidesAssigned :: Int)
       in if cancRate < 7
            then if (ratingStat driverStats :: Text) == "" then "I have a very low cancellation rate that ranks among top 10 percentile. " else "I also have a very low cancellation rate that ranks among top 10 percentile. "
            else ""

    genAspirations aspirations' = if null aspirations' then "" else "With the earnings from my trips, I aspire to " <> T.toLower (T.intercalate ", " aspirations')

    genAboutMeWithAI person driverStats req' = do
      orgLLMChatCompletionConfig <- QOMC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
      prompt <-
        SCL.findByMerchantOpCityIdAndServiceNameAndUseCaseAndPromptKey merchantOpCityId (DOSC.LLMChatCompletionService $ (.llmChatCompletion) orgLLMChatCompletionConfig) DTL.DriverProfileGen DTL.AzureOpenAI_DriverProfileGen_1 >>= fromMaybeM (LlmPromptNotFound merchantOpCityId.getId (show (DOSC.LLMChatCompletionService $ (.llmChatCompletion) orgLLMChatCompletionConfig)) (show DTL.DriverProfileGen) (show DTL.AzureOpenAI_DriverProfileGen_1))
          >>= buildPrompt person driverStats req' . (.promptTemplate)
      gccresp <- TC.getChatCompletion merchantId merchantOpCityId (buildChatCompletionReq prompt)
      logDebug $ "generated - " <> gccresp.genMessage.genContent
      pure $ gccresp.genMessage.genContent

    buildPrompt person driverStats req' promptTemplate = do
      merchant <- CQM.findById merchantId
      cancRate <- calculateCancellationRate driverStats
      pure $
        T.replace "{#homeTown#}" (hometownDetails req'.hometown)
          . T.replace "{#rating#}" (show driverStats.rating)
          . T.replace "{#drivingSince#}" (maybe "" show req'.drivingSince)
          . T.replace "{#aspirations#}" (T.intercalate ", " req'.aspirations)
          . T.replace "{#vehicleTags#}" (T.intercalate ", " req'.vehicleTags)
          . T.replace "{#pledge#}" (T.intercalate ", " req'.pledges)
          . T.replace "{#onPlatformSince#}" (show person.createdAt)
          . T.replace "{#merchant#}" (maybe "" (.name) merchant)
          . T.replace "{#driverName#}" ((.firstName) person)
          . T.replace "{#cancellationRate#}" cancRate
          $ promptTemplate

    buildChatCompletionReq prompt = CIT.GeneralChatCompletionReq {genMessages = [CIT.GeneralChatCompletionMessage {genRole = "user", genContent = prompt}]}

    calculateCancellationRate :: DTS.DriverStats -> Flow Text
    calculateCancellationRate driverStats = do
      let cancelled = fromMaybe 0 driverStats.ridesCancelled
          total = nonZero driverStats.totalRidesAssigned
          cancRate = div (cancelled * 100 :: Int) (total :: Int)
          result = T.pack (show cancRate)
      logDebug $ "Cancellation rate: " <> result
      pure result

getDriverProfileQues ::
  ( ( Maybe (Id SP.Person),
      Id DM.Merchant,
      Id DMOC.MerchantOperatingCity
    ) ->
    Maybe Bool ->
    Flow API.Types.UI.DriverProfileQuestions.DriverProfileQuesRes
  )
getDriverProfileQues (mbPersonId, _merchantId, _merchantOpCityId) isImages = do
  driverId <- mbPersonId & fromMaybeM (PersonNotFound "No person id passed")
  driverProfile <- DPQ.findByPersonId driverId
  person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  mbVehicle <- QVehicle.findById driverId
  vehicleImage <- case mbVehicle of
    Just vehicle -> do
      fetchVehicleImage vehicle.vehicleImageId
    Nothing -> pure Nothing
  profileImage <- case person.faceImageId of
    Just mediaId -> do
      mediaEntry <- runInReplica $ QMF.findById mediaId >>= fromMaybeM (FileDoNotExist driverId.getId)
      case mediaEntry.s3FilePath of
        Just s3Path -> Just <$> Storage.get (T.unpack s3Path)
        _ -> fetchLegacyProfileImage driverId
    Nothing -> do
      fetchLegacyProfileImage driverId

  case driverProfile of
    Just res ->
      getImages (maybe [] (Id <$>) res.imageIds)
        >>= \images ->
          pure $
            API.Types.UI.DriverProfileQuestions.DriverProfileQuesRes
              { aspirations = fromMaybe [] res.aspirations,
                hometown = res.hometown,
                pledges = res.pledges,
                drivingSince = res.drivingSince,
                vehicleTags = fromMaybe [] res.vehicleTags,
                otherImages = if isImages == Just True then images else [], -- fromMaybe [] res.images
                profileImage = profileImage,
                otherImageIds = fromMaybe [] res.imageIds,
                vehicleImage = if isImages == Just True then vehicleImage else Nothing
              }
    Nothing ->
      pure $
        API.Types.UI.DriverProfileQuestions.DriverProfileQuesRes
          { aspirations = [],
            hometown = Nothing,
            pledges = [],
            drivingSince = Nothing,
            vehicleTags = [],
            otherImages = [],
            profileImage = profileImage,
            otherImageIds = [],
            vehicleImage = Nothing
          }
  where
    getImages imageIds = do
      mapM (QMF.findById) imageIds <&> catMaybes <&> ((.url) <$>)
        >>= mapM (Storage.get . T.unpack . extractFilePath)

    extractFilePath url = case T.splitOn "filePath=" url of
      [_before, after] -> after
      _ -> T.empty

    fetchLegacyProfileImage driverId =
      ImageQuery.findByPersonIdImageTypeAndValidationStatus driverId DTO.ProfilePhoto DImage.APPROVED
        >>= maybe (pure Nothing) (\image -> Just <$> Storage.get (T.unpack image.s3Path))

    fetchVehicleImage mbVehicleImageId = case mbVehicleImageId of
      Just mediaId -> do
        mbMediaEntry <- runInReplica $ QMF.findById mediaId
        case mbMediaEntry of
          Just mediaEntry -> do
            case mediaEntry.s3FilePath of
              Just s3Path -> Just <$> Storage.get (T.unpack s3Path)
              _ -> return Nothing
          Nothing -> return Nothing
      Nothing -> return Nothing
