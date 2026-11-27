-- | Helpers shared by the citizen and review operations.
module Lib.Permit.Store where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as TE
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Permit.Api.Types
import Lib.Permit.Domain.Types.PermitApplication
import Lib.Permit.Domain.Types.PermitApplicationDocument
import Lib.Permit.Domain.Types.PermitApplicationEvent
import qualified Lib.Permit.Picnic.Rules as Picnic
import Lib.Permit.Picnic.Types
import Lib.Permit.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Permit.Storage.Queries.PermitApplication as QApplication
import qualified Lib.Permit.Storage.Queries.PermitApplicationDocument as QDocument
import qualified Lib.Permit.Storage.Queries.PermitApplicationEvent as QEvent

findApplication :: BeamFlow m r => Id PermitApplication -> m PermitApplication
findApplication applicationId =
  QApplication.findById applicationId >>= fromMaybeM (InvalidRequest "Application not found.")

-- | Serialises changes to one application, so it can be decided or changed only once.
withApplicationLock :: (BeamFlow m r, MonadMask m) => Id PermitApplication -> m a -> m a
withApplicationLock applicationId = withPermitLock ("permit:applicationLock:" <> applicationId.getId)

withPermitLock :: (BeamFlow m r, MonadMask m) => Text -> m a -> m a
withPermitLock key action =
  Redis.whenWithLockRedisAndReturnValue key 60 action
    >>= either (const $ throwError $ InvalidRequest "Another change is in progress. Try again.") pure

encodeDetails :: PicnicDetails -> Text
encodeDetails = TE.decodeUtf8 . BSL.toStrict . A.encode

-- | The stored details for a picnic permit, as the exact JSON text and its parsed form.
loadDetails :: (BeamFlow m r, EncFlow m r) => PermitApplication -> m (Text, PicnicDetails)
loadDetails application = do
  json <- decrypt application.details
  parsed <-
    A.eitherDecodeStrict (TE.encodeUtf8 json)
      & either (const $ throwError $ InternalError $ "Unreadable details on permit application " <> application.id.getId) pure
  pure (json, parsed)

requirePicnic :: (BeamFlow m r) => PermitDetails -> m PicnicDetails
requirePicnic details = details.picnic & fromMaybeM (InvalidRequest "Picnic details are required.")

findDocuments :: BeamFlow m r => Id PermitApplication -> m [PermitApplicationDocument]
findDocuments = QDocument.findAllByApplicationId

documentFacts :: [PermitApplicationDocument] -> [Picnic.DocumentFact]
documentFacts = map $ \document ->
  Picnic.DocumentFact {documentType = document.documentType, validUntil = document.validUntil, lifetime = document.lifetime}

documentInfo :: PermitApplicationDocument -> PermitDocumentInfo
documentInfo document =
  PermitDocumentInfo
    { documentType = document.documentType,
      validUntil = document.validUntil,
      lifetime = document.lifetime,
      contentType = document.contentType,
      uploadedAt = document.updatedAt
    }

listItem :: PermitApplication -> PicnicDetails -> PermitApplicationListItem
listItem application details =
  PermitApplicationListItem
    { applicationId = application.id,
      referenceNo = application.referenceNo,
      permitType = application.permitType,
      status = application.status,
      approvalLevel = application.approvalLevel,
      permitDate = application.permitDate,
      submittedAt = application.submittedAt,
      picnic =
        Just
          PicnicSummary
            { leaderName = details.leaderName,
              vehicleRegistrationNo = details.vehicle.registrationNo,
              spot = details.spot
            }
    }

passInfo :: PermitApplication -> PicnicDetails -> PermitPassInfo
passInfo application details =
  PermitPassInfo
    { referenceNo = application.referenceNo,
      permitType = application.permitType,
      status = application.status,
      picnic =
        Just
          PicnicPassInfo
            { vehicleRegistrationNo = details.vehicle.registrationNo,
              driverName = details.driver.name,
              driverLicenceNo = details.driver.licenceNo,
              leaderName = details.leaderName,
              passengerNames = details.passengerNames,
              handymanName = details.handymanName,
              travelDate = details.travelDate,
              departureTime = details.departureTime,
              returnTime = details.returnTime,
              returnDeadline = Picnic.formatClock (Picnic.returnDeadline details.travelDate),
              startPoint = details.startPoint,
              spot = details.spot,
              destinationDistrict = details.destinationDistrict,
              routeOut = details.routeOut,
              routeBack = details.routeBack
            }
    }

-- | The history is only ever added to. It must not hold citizens' personal data
-- beyond their internal id.
recordEvent :: BeamFlow m r => Id PermitApplication -> PermitActorType -> Text -> Maybe Text -> Text -> Maybe Text -> m ()
recordEvent applicationId actorType actorId actorName action reason = do
  eventId <- generateGUID
  now <- getCurrentTime
  QEvent.create
    PermitApplicationEvent
      { id = eventId,
        applicationId,
        actorType,
        actorId,
        actorName,
        action,
        reason,
        createdAt = now,
        updatedAt = now
      }
