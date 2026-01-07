{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.SosMedia
  ( getSosMediaSosMedia,
  )
where

import qualified API.Types.RiderPlatform.Management.SosMedia as Common
import qualified Dashboard.Common
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Sos as DSos
import qualified Environment
import qualified IssueManagement.Storage.Queries.MediaFile as QMediaFile
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.IssueManagement ()
import qualified Storage.Flow as Storage
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Sos as QSos

getSosMediaSosMedia ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Dashboard.Common.Customer ->
  Environment.Flow [Common.GetSosMediaResponse]
getSosMediaSosMedia _merchantShortId _opCity customerId = do
  let personId = cast @Dashboard.Common.Customer @DP.Person customerId
  void $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  sosList <- QSos.findByPersonId personId
  let sosWithMediaPairs =
        [ (mediaFileId, sos)
          | sos <- sosList,
            sos.entityType == Just DSos.Ride,
            mediaFileId <- sos.mediaFiles
        ]

  when (null sosWithMediaPairs) $
    throwError $ InvalidRequest "No media files found for this person"

  let allMediaFileIds = map fst sosWithMediaPairs
  mediaFilesList <- QMediaFile.findAllIn allMediaFileIds

  let mediaFilesMap = Map.fromList [(mf.id, mf) | mf <- mediaFilesList]

  forM sosWithMediaPairs $ \(mediaFileId, sos) -> do
    case Map.lookup mediaFileId mediaFilesMap of
      Nothing ->
        throwError $ InvalidRequest "Media file record missing"
      Just mediaFile -> do
        content <- case mediaFile.s3FilePath of
          Nothing -> throwError $ InvalidRequest "No S3 file path found"
          Just s3Path -> Storage.get $ T.unpack s3Path

        let rideId = cast sos.rideId
        return $
          Common.GetSosMediaResponse
            { content = content,
              createdAt = sos.createdAt,
              updatedAt = sos.updatedAt,
              rideId = rideId,
              ticketId = sos.ticketId
            }
