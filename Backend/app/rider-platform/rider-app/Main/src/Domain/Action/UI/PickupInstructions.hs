{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.UI.PickupInstructions
  ( postPickupinstructions,
    getPickupinstructionsClosest,
    deletePickupinstructions,
  )
where

import qualified API.Types.UI.PickupInstructions as API
import AWS.S3 as S3
import qualified Data.ByteString as BS
import qualified Data.Geohash as Geohash
import qualified Data.List as List
import qualified Data.Text as T
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.PickupInstructions as DPI
import qualified Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import EulerHS.Types (base64Encode)
import GHC.IO.Handle (hFileSize)
import qualified IssueManagement.Domain.Types.MediaFile as DMF
import qualified IssueManagement.Storage.Queries.MediaFile as MFQuery
import qualified Kernel.Prelude
import Kernel.ServantMultipart
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.IssueManagement ()
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.PickupInstructions as QPI
import Tools.Error

-- FromMultipart instance for PickupInstructionsReq
instance FromMultipart Tmp API.PickupInstructionsReq where
  fromMultipart form = do
    -- Debug: Log all form inputs

    -- Parse lat and lon as required fields
    latText <- lookupInput "lat" form
    lonText <- lookupInput "lon" form

    lat <- case reads (T.unpack latText) of
      [(val, "")] -> Right val
      _ -> Left ("Invalid latitude format: " <> T.unpack latText)

    lon <- case reads (T.unpack lonText) of
      [(val, "")] -> Right val
      _ -> Left ("Invalid longitude format: " <> T.unpack lonText)

    -- Parse instruction text (optional, default to empty)
    instructionText <- case lookupInput "instruction" form of
      Right text -> Right text
      Left _ -> Right "" -- Default to empty string if not provided

    -- Parse file (optional) - store filename with path for later MIME detection
    file <- case lookupFile "file" form of
      Right fileData -> do
        let fileName = fdFileName fileData
            filePath = fdPayload fileData
            -- Encode filename in path: "filename.ext:/tmp/path"
            encodedPath = fileName <> ":" <> T.pack filePath
        -- Validate it's an audio file based on extension
        case validateAudioContentType (detectMimeTypeFromExtension fileName) of
          Right _ -> Right (Just (T.unpack encodedPath))
          Left err -> Left err
      Left _ -> Right Nothing -- No file provided

    -- Keep instruction as pure text (no MIME type encoding)
    let instruction = instructionText

    return $
      API.PickupInstructionsReq
        { file = file,
          instruction = instruction,
          lat = lat,
          lon = lon
        }

instance ToMultipart Tmp API.PickupInstructionsReq where
  toMultipart req =
    MultipartData
      [ Input "lat" (T.pack $ show req.lat),
        Input "lon" (T.pack $ show req.lon),
        Input "instruction" req.instruction
      ]
      ( case req.file of
          Just filePath -> [FileData "file" "" "audio/mp3" filePath]
          Nothing -> []
      )

validateAudioContentType :: Text -> Either String S3.FileType
validateAudioContentType = \case
  "audio/wave" -> Right S3.Audio
  "audio/wav" -> Right S3.Audio
  "audio/mpeg" -> Right S3.Audio
  "audio/mp3" -> Right S3.Audio
  "audio/mp4" -> Right S3.Audio
  "audio/m4a" -> Right S3.Audio
  "audio/aac" -> Right S3.Audio
  "audio/ogg" -> Right S3.Audio
  "audio/webm" -> Right S3.Audio
  "audio/flac" -> Right S3.Audio
  "audio/x-flac" -> Right S3.Audio
  "audio/opus" -> Right S3.Audio
  "audio/amr" -> Right S3.Audio
  "audio/3gpp" -> Right S3.Audio
  "audio/x-wav" -> Right S3.Audio
  "audio/x-ms-wma" -> Right S3.Audio
  "audio/vnd.wave" -> Right S3.Audio
  _ -> Right S3.Audio -- Accept any audio/* MIME type

-- Map MIME type to file extension
mimeTypeToExtension :: Text -> Text
mimeTypeToExtension = \case
  "audio/wave" -> ".wav"
  "audio/wav" -> ".wav"
  "audio/mpeg" -> ".mp3"
  "audio/mp3" -> ".mp3"
  "audio/mp4" -> ".mp4"
  "audio/m4a" -> ".m4a"
  "audio/aac" -> ".aac"
  "audio/ogg" -> ".ogg"
  "audio/webm" -> ".webm"
  "audio/flac" -> ".flac"
  "audio/x-flac" -> ".flac"
  "audio/opus" -> ".opus"
  "audio/amr" -> ".amr"
  "audio/3gpp" -> ".3gp"
  "audio/x-wav" -> ".wav"
  "audio/x-ms-wma" -> ".wma"
  "audio/vnd.wave" -> ".wav"
  _ -> ".mp3" -- Default fallback

-- Detect MIME type from file extension
detectMimeTypeFromExtension :: Text -> Text
detectMimeTypeFromExtension fileName =
  case T.toLower (T.takeWhileEnd (/= '.') fileName) of
    "mp3" -> "audio/mpeg"
    "wav" -> "audio/wav"
    "m4a" -> "audio/mp4"
    "mp4" -> "audio/mp4"
    "aac" -> "audio/aac"
    "ogg" -> "audio/ogg"
    "webm" -> "audio/webm"
    "flac" -> "audio/flac"
    "opus" -> "audio/opus"
    "amr" -> "audio/amr"
    "3gp" -> "audio/3gpp"
    "wma" -> "audio/x-ms-wma"
    _ -> "audio/mpeg" -- Default to MP3

postPickupinstructions ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.PickupInstructionsReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postPickupinstructions (mbPersonId, merchantId) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  merchantConfig <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  riderConfig <- QRC.findByMerchantOperatingCityId person.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)

  logDebug $ "PickupInstructions: Received POST request - personId: " <> show personId.getId <> ", lat: " <> show req.lat <> ", lon: " <> show req.lon
  logDebug $ "PickupInstructions: Raw instruction field: " <> show req.instruction

  -- Handle optional audio file
  mediaFileId <- case req.file of
    Just audioFilePath -> do
      logDebug $ "PickupInstructions: Processing audio file: " <> show audioFilePath

      -- Extract filename and actual file path from encoded path
      let (originalFileName, actualFilePath) = case T.splitOn ":" (T.pack audioFilePath) of
            (fname : rest) -> (fname, T.unpack $ T.intercalate ":" rest)
            _ -> ("unknown.mp3", audioFilePath) -- Fallback
          mimeType = detectMimeTypeFromExtension originalFileName
          fileExtension = mimeTypeToExtension mimeType
      logDebug $ "PickupInstructions: Original filename: " <> originalFileName
      logDebug $ "PickupInstructions: Actual file path: " <> T.pack actualFilePath
      logDebug $ "PickupInstructions: Detected MIME type from extension: " <> mimeType
      logDebug $ "PickupInstructions: Mapped file extension: " <> fileExtension

      -- Validate file size
      fileSize <- L.runIO $ withFile actualFilePath ReadMode hFileSize
      logDebug $ "PickupInstructions: File size: " <> show fileSize
      logDebug $ "PickupInstructions: File path length: " <> show (T.length (show actualFilePath))
      when (fileSize > fromIntegral riderConfig.videoFileSizeUpperLimit) $
        throwError $ FileSizeExceededError (show fileSize)

      -- Read and encode file
      audioData <- L.runIO $ base64Encode <$> BS.readFile actualFilePath
      logDebug $ "PickupInstructions: Audio data length: " <> show (T.length (show audioData))
      -- Create file path for S3
      pickupInstructionsId :: Kernel.Types.Id.Id DMF.MediaFile <- generateGUID
      filePath <- S3.createFilePath "/pickup-instructions/" ("pickup-" <> pickupInstructionsId.getId) S3.Audio fileExtension
      logDebug $ "PickupInstructions: File path: " <> show filePath
      -- Create file URL
      let fileUrl =
            merchantConfig.mediaFileUrlPattern
              & T.replace "<DOMAIN>" "pickup-instructions"
              & T.replace "<FILE_PATH>" filePath
      logDebug $ "PickupInstructions: File URL: " <> show fileUrl

      -- Upload to S3
      result <- withTryCatch "S3:put:postPickupinstructions" $ S3.put (T.unpack filePath) audioData
      case result of
        Left err -> throwError $ InternalError ("S3 Upload Failed: " <> show err)
        Right _ -> pure ()
      logDebug $ "PickupInstructions: S3 upload successful"
      -- Create media file entry
      mediaFile <- createMediaFileEntry fileUrl filePath
      MFQuery.create mediaFile
      return $ Just mediaFile.id
    Nothing -> do
      logDebug "PickupInstructions: No audio file provided, creating text-only instruction"
      return Nothing

  let actualInstruction = req.instruction

  -- Validate that at least instruction text or audio file is provided
  when (T.null actualInstruction && isNothing mediaFileId) $
    throwError $ InvalidRequest "Either instruction text or audio file must be provided"

  newLocationGeohash <- Geohash.encode 8 (req.lat, req.lon) & (fromMaybeM (InvalidRequest "Invalid location"))
  logDebug $ "PickupInstructions: New location geohash: " <> show newLocationGeohash

  mbNearbyInstruction <- QPI.findByPersonIdAndGeohash personId (T.pack newLocationGeohash)
  logDebug $ "PickupInstructions: Found nearby instruction: " <> show mbNearbyInstruction

  case mbNearbyInstruction of
    Just nearbyInstruction -> do
      -- Update existing instruction using the NEW coordinates and mediaFileId
      let instructionText = case (T.null actualInstruction, mediaFileId) of
            (True, Just _) -> "Audio pickup instruction" -- No text but has audio
            (True, Nothing) -> "Pickup instruction" -- No text and no audio
            (False, _) -> actualInstruction -- Has text
      QPI.updateInstructionById nearbyInstruction.geohash instructionText mediaFileId nearbyInstruction.id
    Nothing -> do
      -- Check if we're at the limit
      existingInstructions <- QPI.findByPersonId personId
      if length existingInstructions >= riderConfig.pickupInstructionsThreshold
        then do
          -- We're at the limit, find the oldest instruction (by updatedAt) and replace it
          case List.sortBy (comparing (.updatedAt)) existingInstructions of
            [] ->
              -- This shouldn't happen since we just checked length above
              throwError $ InternalError "Expected existing instructions but found none"
            (oldestInstruction : _) -> do
              -- Delete the oldest instruction by its ID
              QPI.deleteById oldestInstruction.id
              -- Create new pickup instruction
              newPickupInstructionsId <- generateGUID
              now <- getCurrentTime
              geohash <- Geohash.encode 8 (req.lat, req.lon) & (fromMaybeM (InvalidRequest "Invalid location"))
              let instructionText = case (T.null actualInstruction, mediaFileId) of
                    (True, Just _) -> "Audio pickup instruction" -- No text but has audio
                    (True, Nothing) -> "Pickup instruction" -- No text and no audio
                    (False, _) -> actualInstruction -- Has text
                  newInstruction =
                    DPI.PickupInstructions
                      { DPI.id = newPickupInstructionsId,
                        DPI.personId = personId,
                        DPI.merchantId = merchantId,
                        DPI.merchantOperatingCityId = person.merchantOperatingCityId,
                        DPI.geohash = T.pack geohash,
                        DPI.instruction = instructionText,
                        DPI.mediaFileId = mediaFileId,
                        DPI.createdAt = now,
                        DPI.updatedAt = now
                      }
              logDebug $ "PickupInstructions: Creating replacement instruction - id: " <> show newPickupInstructionsId.getId <> ", lat: " <> show req.lat <> ", lon: " <> show req.lon <> ", mediaFileId: " <> show mediaFileId
              QPI.create newInstruction
        else do
          -- Create new pickup instruction
          newPickupInstructionsId <- generateGUID
          now <- getCurrentTime
          geohash <- Geohash.encode 8 (req.lat, req.lon) & (fromMaybeM (InvalidRequest "Invalid location"))
          let instructionText = case (T.null actualInstruction, mediaFileId) of
                (True, Just _) -> "Audio pickup instruction" -- No text but has audio
                (True, Nothing) -> "Pickup instruction" -- No text and no audio
                (False, _) -> actualInstruction -- Has text
          let newInstruction =
                DPI.PickupInstructions
                  { DPI.id = newPickupInstructionsId,
                    DPI.personId = personId,
                    DPI.merchantId = merchantId,
                    DPI.merchantOperatingCityId = person.merchantOperatingCityId,
                    DPI.geohash = T.pack geohash,
                    DPI.instruction = instructionText,
                    DPI.mediaFileId = mediaFileId,
                    DPI.createdAt = now,
                    DPI.updatedAt = now
                  }
          logDebug $ "PickupInstructions: Creating new instruction - id: " <> show newPickupInstructionsId.getId <> ", lat: " <> show req.lat <> ", lon: " <> show req.lon <> ", mediaFileId: " <> show mediaFileId
          QPI.create newInstruction

  pure Kernel.Types.APISuccess.Success

createMediaFileEntry :: Text -> Text -> Environment.Flow DMF.MediaFile
createMediaFileEntry fileUrl filePath = do
  mediaFileId <- generateGUID
  now <- getCurrentTime
  return $
    DMF.MediaFile
      { id = mediaFileId,
        _type = S3.Audio,
        url = fileUrl,
        s3FilePath = Just filePath,
        createdAt = now
      }

getPickupinstructionsClosest ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe Kernel.Prelude.Double ->
    Kernel.Prelude.Maybe Kernel.Prelude.Double ->
    Environment.Flow API.ClosestPickupInstructionResp
  )
getPickupinstructionsClosest (mbPersonId, _) mbLat mbLon = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  lat <- mbLat & fromMaybeM (InvalidRequest "Missing required parameter: lat")
  lon <- mbLon & fromMaybeM (InvalidRequest "Missing required parameter: lon")

  _person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- Generate geohash 8 for query location to filter instructions
  queryGeohash <- Geohash.encode 8 (lat, lon) & (fromMaybeM (InvalidRequest "Invalid location"))
  logDebug $ "PickupInstructions: Query location geohash: " <> show queryGeohash

  -- Find instructions only in the same geohash 8 cell
  mbPickupInstruction <- QPI.findByPersonIdAndGeohash personId (T.pack queryGeohash)

  case mbPickupInstruction of
    Nothing -> do
      logDebug $ "PickupInstructions: No instruction found in geohash cell: " <> T.pack queryGeohash
      return $
        API.ClosestPickupInstructionResp
          { instruction = Nothing,
            audioBase64 = Nothing
          }
    Just foundInstruction -> do
      logDebug $ "PickupInstructions: Found instruction in geohash cell with text: " <> show foundInstruction.instruction

      -- Get media file content from S3 if available
      mbAudioBase64 <- case foundInstruction.mediaFileId of
        Just mediaFileId -> do
          mbMediaFile <- MFQuery.findById mediaFileId
          case mbMediaFile of
            Just mediaFile -> do
              case mediaFile.s3FilePath of
                Just s3Path -> do
                  logDebug $ "PickupInstructions: Fetching audio file from S3: " <> show s3Path
                  audioContent <- S3.get (T.unpack s3Path)
                  logDebug $ "PickupInstructions: Successfully retrieved audio content from S3"
                  return (Just audioContent)
                Nothing -> do
                  logDebug $ "PickupInstructions: No S3 path found for media file: " <> show mediaFileId.getId
                  return Nothing
            Nothing -> do
              logDebug $ "PickupInstructions: Media file not found for ID: " <> show mediaFileId.getId
              return Nothing
        Nothing -> do
          logDebug "PickupInstructions: No media file associated with this instruction"
          return Nothing

      return $
        API.ClosestPickupInstructionResp
          { instruction = Just foundInstruction.instruction,
            audioBase64 = mbAudioBase64
          }

deletePickupinstructions ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Double) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Double) ->
    Kernel.Prelude.Maybe API.DeleteTarget ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
deletePickupinstructions (mbPersonId, _) mbLat mbLon mbTarget = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  lat <- mbLat & fromMaybeM (InvalidRequest "Missing required parameter: lat")
  lon <- mbLon & fromMaybeM (InvalidRequest "Missing required parameter: lon")
  target <- mbTarget & fromMaybeM (InvalidRequest "Missing required parameter: target")

  _person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)

  logDebug $ "PickupInstructions: DELETE request for personId: " <> show personId.getId <> ", lat: " <> show lat <> ", lon: " <> show lon <> ", target: " <> show target

  -- Convert query location to geohash and find exact match first
  queryGeohash <- Geohash.encode 8 (lat, lon) & (fromMaybeM (InvalidRequest "Invalid location"))
  mbPickupInstruction <- QPI.findByPersonIdAndGeohash personId (T.pack queryGeohash)

  case mbPickupInstruction of
    Just pickupInstruction -> do
      logDebug $ "PickupInstructions: Found exact geohash match for instruction id: " <> show pickupInstruction.id.getId

      case target of
        API.Instruction -> do
          logDebug $ "PickupInstructions: Deleting entire instruction with id: " <> show pickupInstruction.id.getId
          QPI.deleteById pickupInstruction.id
          logDebug $ "PickupInstructions: Successfully deleted instruction"
        API.Audio -> do
          logDebug $ "PickupInstructions: Clearing audio from instruction with id: " <> show pickupInstruction.id.getId

          -- Delete media file from media table and S3 if it exists
          case pickupInstruction.mediaFileId of
            Just mediaFileId -> do
              logDebug $ "PickupInstructions: Deleting media file with id: " <> show mediaFileId.getId

              -- Get media file details before deleting from DB
              mbMediaFile <- MFQuery.findById mediaFileId
              case mbMediaFile of
                Just mediaFile -> do
                  -- Delete actual file from S3 if path exists
                  case mediaFile.s3FilePath of
                    Just s3Path -> do
                      logDebug $ "PickupInstructions: Deleting S3 file: " <> show s3Path
                      void $ fork "S3 delete audio file" $ S3.delete (T.unpack s3Path)
                      logDebug $ "PickupInstructions: Successfully deleted S3 file"
                    Nothing ->
                      logDebug "PickupInstructions: No S3 path found for media file"
                Nothing ->
                  logDebug $ "PickupInstructions: Media file not found for ID: " <> show mediaFileId.getId

              -- Delete media file record from database
              MFQuery.deleteById mediaFileId
              logDebug $ "PickupInstructions: Successfully deleted media file from database"
            Nothing ->
              logDebug "PickupInstructions: No media file to delete"

          -- Update instruction to clear mediaFileId
          QPI.updateMediaFileById Nothing pickupInstruction.id
          logDebug $ "PickupInstructions: Successfully cleared audio from instruction"
    Nothing -> do
      -- No exact geohash match found
      logDebug $ "PickupInstructions: No pickup instruction found for geohash: " <> T.pack queryGeohash
      throwError $ InvalidRequest "No pickup instruction found at the specified location"

  pure Kernel.Types.APISuccess.Success
