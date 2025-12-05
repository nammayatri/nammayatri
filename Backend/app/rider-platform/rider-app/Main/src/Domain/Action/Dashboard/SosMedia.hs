{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.SosMedia
  ( getSosMediaSosMedia,
  )
where

import qualified API.Types.RiderPlatform.Management.SosMedia as Common
import qualified AWS.S3 as S3
import qualified Dashboard.Common
import qualified Data.Text as T
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Environment
import qualified IssueManagement.Storage.Queries.MediaFile as QMediaFile
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.IssueManagement ()
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Sos as QSos

getSosMediaSosMedia ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Dashboard.Common.Customer ->
  Environment.Flow Common.GetSosMediaResponse
getSosMediaSosMedia _merchantShortId _opCity customerId = do
  let personId = cast @Dashboard.Common.Customer @DP.Person customerId
  void $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  sos <- QSos.findByPersonId personId >>= fromMaybeM (PersonNotFound personId.getId)

  -- Get the media file ID (should be exactly one)
  let mediaFileIds = sos.mediaFiles
  when (null mediaFileIds) $ throwError $ InvalidRequest "No media files found for this SOS"
  when (length mediaFileIds > 1) $ throwError $ InvalidRequest "Multiple media files found, expected only one"

  let mediaFileId = head mediaFileIds
  mediaFile <- QMediaFile.findById mediaFileId >>= fromMaybeM (InvalidRequest "Media file not found")

  content <- case mediaFile.s3FilePath of
    Nothing -> throwError $ InvalidRequest "No S3 file path found for media file"
    Just s3Path -> S3.get $ T.unpack s3Path

  return $
    Common.GetSosMediaResponse
      { content = content,
        createdAt = sos.createdAt,
        updatedAt = sos.updatedAt,
        rideId = cast sos.rideId,
        ticketId = sos.ticketId
      }
