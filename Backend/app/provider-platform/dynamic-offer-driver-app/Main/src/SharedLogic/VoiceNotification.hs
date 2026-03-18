module SharedLogic.VoiceNotification where

import qualified API.Types.UI.DriverVoiceUpdate as APITypes
import ChatCompletion.Interface.Types as CIT
import qualified Data.Text as T
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.VoiceTemplate as DVT
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.VoiceTemplate as QVT
import Tools.ChatCompletion as TC
import Tools.Error

-- | Core voice notification generation logic.
-- Selects a template by type and language, fills in variables,
-- then returns a response with the text content and a placeholder audio URL.
-- In production this would call a TTS provider (Google Cloud TTS / AWS Polly)
-- and upload the resulting audio to S3, returning the real CDN URL.
generateVoiceNotification ::
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  APITypes.VoiceUpdateType ->
  Text ->
  Maybe Text ->
  Flow APITypes.VoiceUpdateGenerateRes
generateVoiceNotification merchantId merchantOpCityId updateType lang mbContext = do
  let templateType = show updateType
  mbTemplate <- QVT.findByTemplateTypeAndLanguage templateType lang True
  textContent <- case mbTemplate of
    Just template -> do
      let baseText = template.templateText
      pure $ substituteVariables baseText (fromMaybe "" mbContext)
    Nothing -> do
      -- Fallback: use contextual AI generation for dynamic content
      case updateType of
        APITypes.CONTEXTUAL -> generateContextualUpdate merchantId merchantOpCityId lang mbContext
        APITypes.RIDE_REQUEST -> pure $ buildRideRequestFallback lang mbContext
        APITypes.EARNINGS_SUMMARY -> pure $ buildEarningsSummaryFallback lang mbContext
        APITypes.SAFETY_ALERT -> pure $ buildSafetyAlertFallback lang
        APITypes.NAVIGATION -> pure $ buildNavigationFallback lang
  -- In a full implementation, this is where we would:
  -- 1. Call the TTS provider (Google Cloud TTS / AWS Polly) with textContent
  -- 2. Upload the resulting audio file to S3
  -- 3. Return the CDN URL
  -- For now, return a placeholder URL with the text content
  let estimatedDurationMs = T.length textContent * 60 -- rough estimate: ~60ms per character
  pure
    APITypes.VoiceUpdateGenerateRes
      { audioUrl = "https://cdn.nammayatri.in/voice/" <> templateType <> "-" <> lang <> ".mp3",
        textContent = textContent,
        durationMs = estimatedDurationMs
      }

-- | Substitute template variables like {{pickup}}, {{drop}}, {{fare}} from a context string.
-- Context is expected as a simple key=value,key=value format for now.
substituteVariables :: Text -> Text -> Text
substituteVariables template context =
  let pairs = map (T.breakOn "=") $ T.splitOn "," context
      substitutions = [(T.strip k, T.strip $ T.drop 1 v) | (k, v) <- pairs, not (T.null v)]
   in foldl' (\acc (k, v) -> T.replace ("{{" <> k <> "}}") v acc) template substitutions

-- | Generate a contextual update using the LLM (ChatCompletion) integration.
-- Follows the pattern from DriverProfileQuestions.hs.
generateContextualUpdate ::
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Text ->
  Maybe Text ->
  Flow Text
generateContextualUpdate merchantId merchantOpCityId lang mbContext = do
  let contextInfo = fromMaybe "General driver update" mbContext
  let prompt =
        "Generate a brief, encouraging voice notification for a ride-hailing driver in "
          <> lang
          <> " language. Context: "
          <> contextInfo
          <> ". Keep it under 30 words, conversational, and motivating. "
          <> "Do not use any special characters or markup."
  result <- try $ TC.getChatCompletion merchantId merchantOpCityId (buildChatRequest prompt)
  case result of
    Right resp -> pure $ extractResponseText resp
    Left (err :: SomeException) -> do
      logError $ "LLM contextual update failed: " <> show err
      pure $ "You are doing great! Keep up the good work."

-- | Build a ChatCompletion request following the existing pattern
buildChatRequest :: Text -> CIT.GeneralChatCompletionReq
buildChatRequest prompt =
  CIT.GeneralChatCompletionReq
    { messages =
        [ CIT.ChatMessage
            { role = "system",
              content = "You are a helpful assistant that generates short, encouraging voice notifications for ride-hailing drivers in regional Indian languages."
            },
          CIT.ChatMessage
            { role = "user",
              content = prompt
            }
        ],
      maxTokens = 100,
      temperature = Just 0.7
    }

-- | Extract the text from the ChatCompletion response
extractResponseText :: CIT.GeneralChatCompletionResp -> Text
extractResponseText resp =
  case resp.choices of
    (choice : _) -> choice.message.content
    [] -> "You are doing great! Keep up the good work."

-- | Fallback text builders for when no template exists

buildRideRequestFallback :: Text -> Maybe Text -> Text
buildRideRequestFallback "hi" mbCtx =
  "Nayi sawari ki request aayi hai. " <> fromMaybe "Kripya dekhen." mbCtx
buildRideRequestFallback "kn" mbCtx =
  "Hosa savari viniyu bandide. " <> fromMaybe "Dayavittu nodiri." mbCtx
buildRideRequestFallback "ta" mbCtx =
  "Puthiya payanam kEtpu vanthullathu. " <> fromMaybe "Thayavu seithu paarunga." mbCtx
buildRideRequestFallback "te" mbCtx =
  "Kotha ride request vachindi. " <> fromMaybe "Dayachesi choodandi." mbCtx
buildRideRequestFallback _ mbCtx =
  "New ride request received. " <> fromMaybe "Please check." mbCtx

buildEarningsSummaryFallback :: Text -> Maybe Text -> Text
buildEarningsSummaryFallback "hi" mbCtx =
  "Aaj ki kamai ka update. " <> fromMaybe "Accha kaam kar rahe hain." mbCtx
buildEarningsSummaryFallback "kn" mbCtx =
  "Ivattu income update. " <> fromMaybe "Olleya kelsa maadthiddira." mbCtx
buildEarningsSummaryFallback _ mbCtx =
  "Today's earnings update. " <> fromMaybe "Great work today." mbCtx

buildSafetyAlertFallback :: Text -> Text
buildSafetyAlertFallback "hi" = "Suraksha suchana: Kripya savdhaan rahein aur surakshit chalayein."
buildSafetyAlertFallback "kn" = "Suraksha suchane: Dayavittu jagruthe indiri mattu suraksithavagi chalisi."
buildSafetyAlertFallback _ = "Safety alert: Please stay alert and drive safely."

buildNavigationFallback :: Text -> Text
buildNavigationFallback "hi" = "Navigation update uplabdh hai."
buildNavigationFallback "kn" = "Navigation update labya ide."
buildNavigationFallback _ = "Navigation update available."
