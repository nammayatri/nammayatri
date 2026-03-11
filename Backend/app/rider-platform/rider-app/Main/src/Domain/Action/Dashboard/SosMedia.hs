{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.SosMedia
  ( getSosMediaSosMedia,
  )
where

import qualified API.Types.RiderPlatform.Management.SosMedia as Common
import qualified AWS.S3 as S3
import qualified Dashboard.Common
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Environment
import qualified IssueManagement.Domain.Types.MediaFile as DMF
import qualified IssueManagement.Storage.Queries.MediaFile as QMediaFile
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Safety.Domain.Action.UI.Sos as SafetySos
import qualified Safety.Domain.Types.Common as SafetyCommon
import qualified Safety.Domain.Types.Sos as SafetyDSos
import Storage.Beam.IssueManagement ()
import Storage.Beam.Sos ()
import qualified Storage.Queries.Person as QPerson

getSosMediaSosMedia ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Dashboard.Common.Customer ->
  Environment.Flow [Common.GetSosMediaResponse]
getSosMediaSosMedia _merchantShortId _opCity customerId = do
  let personId = cast @Dashboard.Common.Customer @DP.Person customerId
  void $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let safetyPersonId = cast @DP.Person @SafetyCommon.Person personId
  sosList <- SafetySos.findSosByPersonId safetyPersonId
  let sosWithMediaPairs =
        [ (mediaFileId, sos)
          | sos <- sosList,
            sos.entityType == Just SafetyDSos.Ride,
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
          Just s3Path -> S3.get $ T.unpack s3Path

        rideId <- case sos.rideId of
          Nothing -> throwError $ InvalidRequest "Ride ID not found for SOS"
          Just rid -> return $ cast @SafetyCommon.Ride @Dashboard.Common.Ride rid
        return $
          Common.GetSosMediaResponse
            { content = content,
              createdAt = sos.createdAt,
              updatedAt = sos.updatedAt,
              rideId = rideId,
              ticketId = sos.ticketId
            }
