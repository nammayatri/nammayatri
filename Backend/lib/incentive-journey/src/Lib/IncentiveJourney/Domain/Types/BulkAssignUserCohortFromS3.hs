{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module Lib.IncentiveJourney.Domain.Types.BulkAssignUserCohortFromS3 where

import qualified Data.Text as T
import Data.Time (defaultTimeLocale, parseTimeM)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Kernel.Prelude
import Kernel.ServantMultipart
import Kernel.Types.HideSecrets (HideSecrets (..))

-- | Multipart body for POST /assign/bulkFromS3.
-- `file` is the CSV spilled to a temp file by servant-multipart (Tmp).
-- `s3FilePath` is the object key inside the configured bucket.
data BulkAssignUserCohortFromS3Req = BulkAssignUserCohortFromS3Req
  { file :: FilePath,
    s3FilePath :: Text,
    scheduledAt :: UTCTime,
    batchSize :: Maybe Int,
    rescheduleDelaySeconds :: Maybe Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets BulkAssignUserCohortFromS3Req where
  hideSecrets = identity

instance FromMultipart Tmp BulkAssignUserCohortFromS3Req where
  fromMultipart form = do
    fileData <- lookupFile "file" form
    s3FilePath <- lookupInput "s3FilePath" form
    scheduledAtTxt <- lookupInput "scheduledAt" form
    scheduledAt <- parseScheduledAt scheduledAtTxt
    batchSize <- parseOptionalInt "batchSize" form
    rescheduleDelaySeconds <- parseOptionalInt "rescheduleDelaySeconds" form
    pure
      BulkAssignUserCohortFromS3Req
        { file = fdPayload fileData,
          s3FilePath,
          scheduledAt,
          batchSize,
          rescheduleDelaySeconds
        }

instance ToMultipart Tmp BulkAssignUserCohortFromS3Req where
  toMultipart req =
    MultipartData
      ( catMaybes
          [ Just (Input "s3FilePath" req.s3FilePath),
            Just (Input "scheduledAt" (T.pack $ iso8601Show req.scheduledAt)),
            fmap (Input "batchSize" . T.pack . show) req.batchSize,
            fmap (Input "rescheduleDelaySeconds" . T.pack . show) req.rescheduleDelaySeconds
          ]
      )
      [FileData "file" (T.pack req.file) "text/csv" req.file]

parseScheduledAt :: Text -> Either String UTCTime
parseScheduledAt raw =
  let s = T.unpack (T.strip raw)
   in case firstParse s of
        Just t -> Right t
        Nothing -> Left "scheduledAt must be an ISO-8601 UTC time"
  where
    firstParse s =
      asumMaybe
        [ iso8601ParseM s,
          parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" s,
          parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" s,
          parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%Z" s
        ]
    asumMaybe [] = Nothing
    asumMaybe (x : xs) = case x of
      Just t -> Just t
      Nothing -> asumMaybe xs

parseOptionalInt :: Text -> MultipartData Tmp -> Either String (Maybe Int)
parseOptionalInt key form =
  case lookupInput key form of
    Left _ -> Right Nothing
    Right raw
      | T.null (T.strip raw) -> Right Nothing
      | otherwise ->
        case readMaybe (T.unpack (T.strip raw)) of
          Just n -> Right (Just n)
          Nothing -> Left $ T.unpack key <> " must be an integer"
