{-# OPTIONS_GHC -Wwarn=unused-imports #-}

-- | Fleet Communication Framework: create/send messages, list sent/drafts/received,
-- delivery status, and recipient resolution. WEB channel is updated in-process;
-- PUSH/SMS/WhatsApp are published to Kafka and processed by a consumer (one message
-- per delivery so multiple channels per driver are handled independently).
module Domain.Action.Dashboard.Management.Communication
  ( postCommunicationCreate,
    getCommunicationList,
    getCommunicationInfo,
    postCommunicationSend,
    putCommunicationEdit,
    deleteCommunicationDelete,
    getCommunicationDeliveryStatus,
    getCommunicationRecipients,
    getCommunicationTemplate,
    CommunicationDeliveryDispatchPayload (..),
    processFleetCommunicationDeliveryPayload,
  )
where

import qualified API.Types.ProviderPlatform.Management.Communication as CommAPI
import qualified Dashboard.Common
import qualified Data.Aeson as Aeson
import Data.Function (on)
import Data.List (nub, nubBy)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TEnc
import qualified Domain.Types.Communication as DComm
import qualified Domain.Types.CommunicationDelivery as DDelivery
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantMessage as DMM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified IssueManagement.Domain.Types.MediaFile as DMF
import IssueManagement.Storage.BeamFlow (BeamFlow)
import qualified IssueManagement.Storage.Queries.MediaFile as MFQuery
import Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt, getDbHash)
import Kernel.External.Notification.FCM.Types as FCM
import Kernel.Sms.Config (SmsConfig)
import Kernel.Streaming.Kafka.Producer (produceMessage)
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.IssueManagement ()
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantMessage as CMM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Communication as QComm
import qualified Storage.Queries.CommunicationDelivery as QDelivery
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDA
import qualified Storage.Queries.FleetOperatorAssociationExtra as QFOA
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import qualified Storage.Queries.MerchantMessage as QMM
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import qualified Tools.Notifications as Notify
import qualified Tools.SMS as Sms
import qualified Tools.Whatsapp as Whatsapp

-- | Payload for Kafka: one message per delivery (per recipient × per channel).
-- Consumer uses this to send via PUSH/SMS/WhatsApp without reading Communication from DB.
-- | Kafka payload: one delivery per recipient×channel. Used when PUSH/SMS/WhatsApp consumer processes a message.
data CommunicationDeliveryDispatchPayload = CommunicationDeliveryDispatchPayload
  { deliveryId :: Text,
    communicationId :: Text,
    channel :: Text,
    recipientId :: Text,
    merchantId :: Text,
    merchantOperatingCityId :: Text,
    title :: Text,
    body :: Text,
    htmlBody :: Maybe Text,
    templateId :: Maybe Text,
    templateName :: Maybe Text
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

-- | Resolve mediaFileIds to mediaUrls JSON. Returns Nothing if no ids; Just (toJSON [...]) otherwise.
resolveMediaFileIds ::
  (BeamFlow m r, MonadFlow m) =>
  Maybe [Kernel.Types.Id.Id Dashboard.Common.File] ->
  m (Maybe Aeson.Value)
resolveMediaFileIds Nothing = pure Nothing
resolveMediaFileIds (Just []) = pure $ Just (Aeson.toJSON ([] :: [CommAPI.CommunicationMediaFile]))
resolveMediaFileIds (Just ids) = do
  let mediaFileIds = map (Kernel.Types.Id.cast @Dashboard.Common.File @DMF.MediaFile) ids
  mediaFiles <- MFQuery.findAllIn mediaFileIds
  let items =
        map
          ( \mf ->
              CommAPI.CommunicationMediaFile
                { url = mf.url,
                  fileType = show mf._type,
                  thumbnailUrl = Nothing
                }
          )
          mediaFiles
  pure $ Just (Aeson.toJSON items)

-- | Used when creating a new communication (save as draft or send immediately).
postCommunicationCreate ::
  Kernel.Types.Id.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Dashboard.Common.Person ->
  CommAPI.CreateCommunicationRequest ->
  Environment.Flow CommAPI.CreateCommunicationResponse
postCommunicationCreate merchantShortId opCity personId req = do
  merchant <- CQM.findByShortId merchantShortId >>= fromMaybeM (MerchantNotFound merchantShortId.getShortId)
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  merchantOpCity <- CQMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
  mediaUrlsJson <- resolveMediaFileIds req.mediaFileIds
  commId <- generateGUID
  now <- getCurrentTime
  let isDraft = req.messageStatus == CommAPI.SAVE_AS_DRAFT
      channelsWithWeb = ensureWebChannel req.channels
      comm =
        DComm.Communication
          { id = commId,
            merchantId = merchant.id,
            merchantOperatingCityId = merchantOpCityId,
            domain = toDomainDomain req.domain,
            senderId = Kernel.Types.Id.cast @Dashboard.Common.Person @DP.Person personId,
            senderRole = toDomainSenderRole CommAPI.ROLE_ADMIN,
            senderDisplayName = Nothing,
            title = req.title,
            body = req.body,
            htmlBody = req.htmlBody,
            contentType = toDomainContentType req.msgContentType,
            mediaUrls = mediaUrlsJson,
            channels = map toDomainChannel channelsWithWeb,
            ctaButton = toDomainCTA <$> req.ctaButton,
            variables = Nothing,
            triggerType = DComm.TT_MANUAL,
            scheduledAt = Nothing,
            status = if isDraft then DComm.ST_DRAFT else DComm.ST_SENDING,
            templateId = req.templateId,
            templateName = req.templateName,
            createdAt = now,
            updatedAt = now
          }
  QComm.create comm
  unless isDraft $ do
    recipients <- resolveRecipientIds merchant merchantOpCity req.recipientIds req.selectAll req.selectAllRoles req.fleetOwnerId req.operatorId
    dispatchToRecipients merchant.id merchantOpCityId comm recipients now
    QComm.updateStatusById DComm.ST_SENT commId
  let responseStatus = if isDraft then CommAPI.CS_DRAFT else CommAPI.CS_SENT
  return $
    CommAPI.CreateCommunicationResponse
      { communicationId = Kernel.Types.Id.cast commId,
        status = responseStatus
      }

-- | Used when listing communications by type: SENT, DRAFT, or RECEIVED (paginated).
getCommunicationList ::
  Kernel.Types.Id.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe CommAPI.CommunicationListType ->
  Maybe CommAPI.CommunicationChannelType ->
  Maybe CommAPI.CommunicationDomainType ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  Kernel.Types.Id.Id Dashboard.Common.Person ->
  Environment.Flow CommAPI.CommunicationListResponse
getCommunicationList merchantShortId _opCity mbListType _mbChannel mbDomain _mbSearch mbLimit mbOffset personId = do
  _merchant <- CQM.findByShortId merchantShortId >>= fromMaybeM (MerchantNotFound merchantShortId.getShortId)
  let listType = fromMaybe CommAPI.LIST_SENT mbListType
  let senderId = Kernel.Types.Id.cast @Dashboard.Common.Person @DP.Person personId
      recipientId = senderId
  case listType of
    CommAPI.LIST_SENT -> do
      let domainFilter = toDomainDomain <$> mbDomain
      comms <- QComm.findBySenderIdWithLimitOffset senderId (Just DComm.ST_SENT) domainFilter mbLimit mbOffset
      let items = map mkSentListItem comms
      return $ CommAPI.CommunicationListResponse {communications = items, summary = Dashboard.Common.Summary {totalCount = length items, count = length items}}
    CommAPI.LIST_DRAFT -> do
      comms <- QComm.findBySenderIdWithLimitOffset senderId (Just DComm.ST_DRAFT) Nothing mbLimit mbOffset
      let items = map mkSentListItem comms
      return $ CommAPI.CommunicationListResponse {communications = items, summary = Dashboard.Common.Summary {totalCount = length items, count = length items}}
    CommAPI.LIST_RECEIVED -> do
      deliveries <- QDelivery.findByRecipientIdAndWebChannel recipientId mbLimit mbOffset
      items <- mapM mkReceivedListItem deliveries
      return $ CommAPI.CommunicationListResponse {communications = items, summary = Dashboard.Common.Summary {totalCount = length items, count = length items}}

-- | Used when viewing full details of a communication (title, body, channels, delivery summary, etc.).
getCommunicationInfo ::
  Kernel.Types.Id.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Dashboard.Common.Communication ->
  Environment.Flow CommAPI.CommunicationInfoResponse
getCommunicationInfo merchantShortId _opCity communicationId = do
  _merchant <- CQM.findByShortId merchantShortId >>= fromMaybeM (MerchantNotFound merchantShortId.getShortId)
  let commId = Kernel.Types.Id.cast communicationId
  comm <- QComm.findById commId >>= fromMaybeM (InvalidRequest "Communication not found")
  deliveries <- QDelivery.findByCommunicationId commId
  let deliverySummary = mkDeliverySummary deliveries
  let mediaUrls =
        comm.mediaUrls >>= \val ->
          case Aeson.fromJSON val of
            Aeson.Success urls -> Just urls
            Aeson.Error _ -> Nothing
  return $
    CommAPI.CommunicationInfoResponse
      { id = Kernel.Types.Id.cast comm.id,
        title = comm.title,
        body = comm.body,
        htmlBody = comm.htmlBody,
        msgContentType = fromDomainContentType comm.contentType,
        mediaUrls = mediaUrls,
        channels = map fromDomainChannel comm.channels,
        ctaButton = fromDomainCTA <$> comm.ctaButton,
        variables = Nothing,
        domain = fromDomainDomain comm.domain,
        senderName = comm.senderDisplayName,
        senderRole = fromDomainSenderRole comm.senderRole,
        status = fromDomainStatus comm.status,
        deliverySummary = Just deliverySummary,
        createdAt = comm.createdAt,
        updatedAt = comm.updatedAt
      }

-- | Used when sending an existing draft communication to recipients.
postCommunicationSend ::
  Kernel.Types.Id.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Dashboard.Common.Communication ->
  CommAPI.SendCommunicationRequest ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postCommunicationSend merchantShortId opCity communicationId req = do
  merchant <- CQM.findByShortId merchantShortId >>= fromMaybeM (MerchantNotFound merchantShortId.getShortId)
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let commId = Kernel.Types.Id.cast communicationId
  comm <- QComm.findById commId >>= fromMaybeM (InvalidRequest "Communication not found")
  unless (comm.status == DComm.ST_DRAFT) $ throwError (InvalidRequest "Only draft communications can be sent")
  merchantOpCity <- CQMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)

  now <- getCurrentTime
  recipients <- resolveRecipientIds merchant merchantOpCity req.recipientIds req.selectAll req.selectAllRoles req.fleetOwnerId req.operatorId
  QComm.updateStatusById DComm.ST_SENDING commId
  dispatchToRecipients merchant.id merchantOpCityId comm recipients now
  QComm.updateStatusById DComm.ST_SENT commId
  return Kernel.Types.APISuccess.Success

-- | Used when editing a draft communication (title, body, channels).
putCommunicationEdit ::
  Kernel.Types.Id.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Dashboard.Common.Communication ->
  CommAPI.EditCommunicationRequest ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
putCommunicationEdit merchantShortId _opCity communicationId req = do
  _merchant <- CQM.findByShortId merchantShortId >>= fromMaybeM (MerchantNotFound merchantShortId.getShortId)
  let commId = Kernel.Types.Id.cast communicationId
  comm <- QComm.findById commId >>= fromMaybeM (InvalidRequest "Communication not found")
  unless (comm.status == DComm.ST_DRAFT) $ throwError (InvalidRequest "Only draft communications can be edited")
  mediaUrlsJson <- resolveMediaFileIds req.mediaFileIds
  QComm.updateCommunication
    commId
    req.title
    req.body
    req.htmlBody
    (toDomainContentType <$> req.msgContentType)
    (map (map toDomainChannel) req.channels)
    mediaUrlsJson
  return Kernel.Types.APISuccess.Success

-- | Used when deleting a draft communication.
deleteCommunicationDelete ::
  Kernel.Types.Id.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Dashboard.Common.Communication ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
deleteCommunicationDelete merchantShortId _opCity communicationId = do
  _merchant <- CQM.findByShortId merchantShortId >>= fromMaybeM (MerchantNotFound merchantShortId.getShortId)
  let commId = Kernel.Types.Id.cast communicationId
  comm <- QComm.findById commId >>= fromMaybeM (InvalidRequest "Communication not found")
  unless (comm.status == DComm.ST_DRAFT) $ throwError (InvalidRequest "Only draft communications can be deleted")
  QDelivery.deleteByCommunicationId commId
  QComm.deleteById commId
  return Kernel.Types.APISuccess.Success

-- | Used when viewing delivery status of a sent communication (per recipient per channel, with filters).
getCommunicationDeliveryStatus ::
  Kernel.Types.Id.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Dashboard.Common.Communication ->
  Maybe CommAPI.CommunicationChannelType ->
  Maybe CommAPI.CommunicationDeliveryStatusType ->
  Maybe Int ->
  Maybe Int ->
  Environment.Flow CommAPI.DeliveryStatusResponse
getCommunicationDeliveryStatus merchantShortId _opCity communicationId mbChannel mbStatus mbLimit mbOffset = do
  _merchant <- CQM.findByShortId merchantShortId >>= fromMaybeM (MerchantNotFound merchantShortId.getShortId)
  let commId = Kernel.Types.Id.cast communicationId
  _comm <- QComm.findById commId >>= fromMaybeM (InvalidRequest "Communication not found")
  deliveries <-
    QDelivery.findByCommunicationIdWithFilters
      commId
      (toDomainChannel <$> mbChannel)
      (toDomainDeliveryStatus <$> mbStatus)
      mbLimit
      mbOffset
  let items = map mkDeliveryStatusItem deliveries
  return $
    CommAPI.DeliveryStatusResponse
      { deliveries = items,
        summary = Dashboard.Common.Summary {totalCount = length items, count = length items}
      }

-- | Used when fetching selectable recipients (drivers, fleet owners, operators) for compose/send UI.
getCommunicationRecipients ::
  Kernel.Types.Id.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe CommAPI.CommunicationRoleType ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Bool ->
  Maybe Int ->
  Maybe Int ->
  Environment.Flow CommAPI.RecipientsResponse
getCommunicationRecipients merchantShortId opCity mbRole mbFleetOwnerId mbOperatorId mbSearch _mbSelectAll mbLimit mbOffset = do
  merchant <- CQM.findByShortId merchantShortId >>= fromMaybeM (MerchantNotFound merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  let limit = min 100 . fromMaybe 20 $ mbLimit
      offset = fromMaybe 0 mbOffset
  mbSearchDbHash <- getDbHash `traverse` mbSearch
  drivers <-
    case (mbFleetOwnerId, mbOperatorId) of
      (Just fleetOwnerId, _) -> do
        pairs <- B.runInReplica $ QFDA.findAllActiveDriverByFleetOwnerId fleetOwnerId (Just limit) (Just offset) mbSearchDbHash mbSearch mbSearch (Just True)
        pure $ map (\(_, person) -> person) pairs
      (_, Just operatorId) -> do
        assocs <- QFOA.findAllActiveByOperatorId operatorId
        fleetOwnerIds <- pure $ nub $ map (.fleetOwnerId) assocs
        if null fleetOwnerIds
          then pure []
          else do
            pairs <- B.runInReplica $ QFDA.findAllActiveDriverByFleetOwnerIds fleetOwnerIds (Just limit) (Just offset) mbSearchDbHash mbSearch mbSearch (Just True)
            pure $ map (\(_, person) -> person) pairs
      _ -> do
        tuples <-
          B.runInReplica $
            QPerson.findAllDriversWithInfoAndVehicle
              merchant
              merchantOpCity
              limit
              offset
              Nothing
              Nothing
              Nothing
              Nothing
              mbSearchDbHash
              mbSearch
              mbSearch
        pure $ map (\(person, _, _) -> person) tuples
  recipients <- mapM (buildRecipientItem mbRole) drivers
  let count = length recipients
  return $
    CommAPI.RecipientsResponse
      { recipients,
        summary = Dashboard.Common.Summary {totalCount = count, count}
      }

buildRecipientItem ::
  (EncFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  Maybe CommAPI.CommunicationRoleType ->
  DP.Person ->
  m CommAPI.RecipientItem
buildRecipientItem _mbRole person = do
  phone <- mapM decrypt person.mobileNumber
  let name = person.firstName <> maybe "" (" " <>) person.middleName <> maybe "" (" " <>) person.lastName
  pure $
    CommAPI.RecipientItem
      { id = person.id.getId,
        name,
        role = CommAPI.ROLE_DRIVER,
        phone,
        email = person.email,
        fleetOwnerName = Nothing,
        operatorName = Nothing
      }

-- | (recipientId, recipientRole) for dispatch
type RecipientWithRole = (Text, DDelivery.CommunicationRecipientRole)

personRoleToRecipientRole :: DP.Role -> DDelivery.CommunicationRecipientRole
personRoleToRecipientRole DP.DRIVER = DDelivery.RR_DRIVER
personRoleToRecipientRole DP.FLEET_OWNER = DDelivery.RR_FLEET_OWNER
personRoleToRecipientRole DP.OPERATOR = DDelivery.RR_OPERATOR
personRoleToRecipientRole DP.ADMIN = DDelivery.RR_ADMIN
personRoleToRecipientRole _ = DDelivery.RR_DRIVER

-- | Resolve recipient (id, role) when selectAll is True. Scope: admin merchant+opCity, or fleet/operator.
-- | selectAllRoles: Nothing or [] = all allowed roles for context; Just [r1,r2] = only those roles.
resolveRecipientIdsForSelectAll ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  DM.Merchant ->
  DMOC.MerchantOperatingCity ->
  Maybe [CommAPI.CommunicationRoleType] ->
  Maybe Text ->
  Maybe Text ->
  m [RecipientWithRole]
resolveRecipientIdsForSelectAll merchant merchantOpCity mbSelectAllRoles mbFleetOwnerId mbOperatorId = do
  let defaultFleet = [CommAPI.ROLE_DRIVER, CommAPI.ROLE_OPERATOR]
      defaultOperator = [CommAPI.ROLE_DRIVER, CommAPI.ROLE_FLEET_OWNER]
      defaultAdmin = [CommAPI.ROLE_DRIVER, CommAPI.ROLE_FLEET_OWNER, CommAPI.ROLE_OPERATOR]
      rolesToFetch =
        case (mbSelectAllRoles, mbFleetOwnerId, mbOperatorId) of
          (Just roles, _, _) | not (null roles) -> roles
          _ ->
            case (mbFleetOwnerId, mbOperatorId) of
              (Just _, _) -> defaultFleet
              (_, Just _) -> defaultOperator
              _ -> defaultAdmin
  case (mbFleetOwnerId, mbOperatorId) of
    -- Fleet: can send to drivers and operators only (not fleet owners)
    (Just fleetOwnerId, _) -> do
      driverIds <-
        if CommAPI.ROLE_DRIVER `elem` rolesToFetch
          then do
            rawIds <- QFDA.getActiveDriverIdsByFleetOwnerId fleetOwnerId
            filterPersonIdsByMerchantAndOpCity (map (.getId) rawIds) merchant merchantOpCity
          else pure []
      operatorIds <-
        if CommAPI.ROLE_OPERATOR `elem` rolesToFetch
          then do
            assocs <- QFOA.findByFleetOwnerId fleetOwnerId True
            pure $ nub $ map (.operatorId) assocs
          else pure []
      let drivers = map (\id_ -> (id_, DDelivery.RR_DRIVER)) driverIds
          operators = map (\id_ -> (id_, DDelivery.RR_OPERATOR)) operatorIds
      pure $ nubBy ((==) `on` fst) $ drivers <> operators
    (_, Just operatorId) -> do
      assocs <- QFOA.findAllActiveByOperatorId operatorId
      fleetOwnerIds <- pure $ nub $ map (.fleetOwnerId) assocs
      driverIds <-
        if CommAPI.ROLE_DRIVER `elem` rolesToFetch && not (null fleetOwnerIds)
          then do
            idsList <- mapM (QFDA.getActiveDriverIdsByFleetOwnerId) fleetOwnerIds
            filterPersonIdsByMerchantAndOpCity (nub $ concat $ map (map (.getId)) idsList) merchant merchantOpCity
          else pure []
      fleetOwnerPersonIds <-
        if CommAPI.ROLE_FLEET_OWNER `elem` rolesToFetch
          then pure $ nub fleetOwnerIds
          else pure []
      let drivers = map (\id_ -> (id_, DDelivery.RR_DRIVER)) driverIds
          fleetOwners = map (\id_ -> (id_, DDelivery.RR_FLEET_OWNER)) fleetOwnerPersonIds
      pure $ nubBy ((==) `on` fst) $ drivers <> fleetOwners
    _ -> do
      recipients <-
        concat
          <$> sequence
            [ if CommAPI.ROLE_DRIVER `elem` rolesToFetch
                then do
                  drivers <- QPerson.findAllDriversWithInfoAndVehicle merchant merchantOpCity selectAllMaxLimit 0 Nothing Nothing Nothing Nothing Nothing Nothing Nothing
                  pure $ map (\(p, _, _) -> (p.id.getId, DDelivery.RR_DRIVER)) drivers
                else pure [],
              if CommAPI.ROLE_FLEET_OWNER `elem` rolesToFetch
                then do
                  fleetOwners <- QFOI.findFleetOwners merchantOpCity.id Nothing Nothing Nothing (Just selectAllMaxLimit) (Just 0)
                  pure $ map (\fo -> (fo.fleetOwnerPersonId.getId, DDelivery.RR_FLEET_OWNER)) fleetOwners
                else pure [],
              if CommAPI.ROLE_OPERATOR `elem` rolesToFetch
                then do
                  ops <- B.runInReplica $ QPerson.findAllByMerchantIdAndOpCityAndRoles merchant merchantOpCity [DP.OPERATOR] selectAllMaxLimit 0
                  pure $ map (\p -> (p.id.getId, DDelivery.RR_OPERATOR)) ops
                else pure []
            ]
      pure $ nubBy ((==) `on` fst) recipients
  where
    selectAllMaxLimit = 50000

filterPersonIdsByMerchantAndOpCity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  [Text] ->
  DM.Merchant ->
  DMOC.MerchantOperatingCity ->
  m [Text]
filterPersonIdsByMerchantAndOpCity ids merchant merchantOpCity =
  if null ids
    then pure []
    else do
      persons <- QPerson.getDriversByIdIn (map Kernel.Types.Id.Id ids)
      let inScope p =
            p.merchantId == merchant.id
              && p.merchantOperatingCityId == merchantOpCity.id
      pure $ map (\p -> p.id.getId) $ filter inScope persons

-- | Validate manual recipientIds are in scope (fleet/operator/admin) and return with role.
validateRecipientsInScope ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  [Text] ->
  DM.Merchant ->
  DMOC.MerchantOperatingCity ->
  Maybe Text ->
  Maybe Text ->
  m [RecipientWithRole]
validateRecipientsInScope recipientIds merchant merchantOpCity mbFleetOwnerId mbOperatorId =
  if null recipientIds
    then pure []
    else do
      persons <- QPerson.findAllByPersonIds recipientIds
      when (length persons /= length recipientIds) $
        throwError (InvalidRequest "One or more recipient IDs not found")
      forM persons $ \person -> do
        let id_ = person.id.getId
        unless (person.merchantId == merchant.id && person.merchantOperatingCityId == merchantOpCity.id) $
          throwError (InvalidRequest $ "Recipient " <> id_ <> " is not in merchant/operating city scope")
        case (mbFleetOwnerId, mbOperatorId) of
          (Just fleetOwnerId, _) -> do
            case person.role of
              DP.DRIVER -> do
                assoc <- QFDA.findByDriverIdAndFleetOwnerId person.id fleetOwnerId True
                unless (isJust assoc) $
                  throwError (InvalidRequest $ "Driver " <> id_ <> " is not under fleet " <> fleetOwnerId)
                pure (id_, DDelivery.RR_DRIVER)
              DP.OPERATOR -> do
                assoc <- QFOA.findByFleetOwnerIdAndOperatorId (Kernel.Types.Id.Id fleetOwnerId) person.id True
                unless (isJust assoc) $
                  throwError (InvalidRequest $ "Operator " <> id_ <> " is not associated with fleet " <> fleetOwnerId)
                pure (id_, DDelivery.RR_OPERATOR)
              _ ->
                throwError (InvalidRequest $ "Recipient " <> id_ <> " must be driver or operator for fleet scope")
          (_, Just operatorId) -> do
            case person.role of
              DP.DRIVER -> do
                assocs <- QFOA.findAllActiveByOperatorId operatorId
                fleetOwnerIds <- mapM (\a -> QFDA.findByDriverIdAndFleetOwnerId person.id a.fleetOwnerId True) assocs
                unless (any isJust fleetOwnerIds) $
                  throwError (InvalidRequest $ "Driver " <> id_ <> " is not under operator " <> operatorId)
                pure (id_, DDelivery.RR_DRIVER)
              DP.FLEET_OWNER -> do
                assoc <- QFOA.findByFleetOwnerIdAndOperatorId person.id (Kernel.Types.Id.Id operatorId) True
                unless (isJust assoc) $
                  throwError (InvalidRequest $ "Fleet owner " <> id_ <> " is not under operator " <> operatorId)
                pure (id_, DDelivery.RR_FLEET_OWNER)
              _ ->
                throwError (InvalidRequest $ "Recipient " <> id_ <> " must be driver or fleet owner for operator scope")
          _ ->
            pure (id_, personRoleToRecipientRole person.role)

-- | Resolve recipients: validate manual list or query when selectAll True.
resolveRecipientIds ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  DM.Merchant ->
  DMOC.MerchantOperatingCity ->
  Maybe [Text] ->
  Maybe Bool ->
  Maybe [CommAPI.CommunicationRoleType] ->
  Maybe Text ->
  Maybe Text ->
  m [RecipientWithRole]
resolveRecipientIds merchant merchantOpCity mbRecipientIds mbSelectAll mbSelectAllRoles mbFleetOwnerId mbOperatorId =
  if mbSelectAll == Just True
    then resolveRecipientIdsForSelectAll merchant merchantOpCity mbSelectAllRoles mbFleetOwnerId mbOperatorId
    else validateRecipientsInScope (fromMaybe [] mbRecipientIds) merchant merchantOpCity mbFleetOwnerId mbOperatorId

-- Helpers

ensureWebChannel :: [CommAPI.CommunicationChannelType] -> [CommAPI.CommunicationChannelType]
ensureWebChannel channels =
  if CommAPI.CH_WEB `elem` channels
    then channels
    else CommAPI.CH_WEB : channels

dispatchToRecipients ::
  Kernel.Types.Id.Id DM.Merchant ->
  Kernel.Types.Id.Id DMOC.MerchantOperatingCity ->
  DComm.Communication ->
  [RecipientWithRole] ->
  UTCTime ->
  Environment.Flow ()
dispatchToRecipients merchantId merchantOpCityId comm recipients now = do
  -- Step 1: Create all delivery records upfront (status = PENDING)
  deliveries <- forM recipients $ \(recipientIdText, recipientRole) -> do
    forM comm.channels $ \channel -> do
      deliveryId <- generateGUID
      let delivery =
            DDelivery.CommunicationDelivery
              { id = deliveryId,
                communicationId = comm.id,
                merchantId = merchantId,
                merchantOperatingCityId = merchantOpCityId,
                recipientId = Kernel.Types.Id.Id recipientIdText,
                recipientRole,
                channel = channel,
                status = DDelivery.DS_PENDING,
                failureReason = Nothing,
                fleetOwnerId = Nothing,
                operatorId = Nothing,
                deliveredAt = Nothing,
                readAt = Nothing,
                createdAt = now,
                updatedAt = now
              }
      QDelivery.create delivery
      return delivery
  -- Step 2: WEB in-process; PUSH/SMS/WhatsApp via Kafka (one message per delivery)
  let allDeliveries = concat deliveries
  topic <- asks (.fleetCommunicationDispatchTopic)
  forM_ allDeliveries $ \delivery ->
    if delivery.channel == DComm.CH_WEB
      then QDelivery.updateStatusById DDelivery.DS_DELIVERED delivery.id
      else do
        let payload =
              CommunicationDeliveryDispatchPayload
                { deliveryId = delivery.id.getId,
                  communicationId = comm.id.getId,
                  channel = channelToText delivery.channel,
                  recipientId = delivery.recipientId.getId,
                  merchantId = merchantId.getId,
                  merchantOperatingCityId = merchantOpCityId.getId,
                  title = comm.title,
                  body = comm.body,
                  htmlBody = comm.htmlBody,
                  templateId = comm.templateId,
                  templateName = comm.templateName
                }
        produceMessage (topic, Just $ TEnc.encodeUtf8 delivery.id.getId) payload
  where
    channelToText DComm.CH_PUSH = "PUSH"
    channelToText DComm.CH_SMS = "SMS"
    channelToText DComm.CH_EMAIL = "EMAIL"
    channelToText DComm.CH_WHATSAPP = "WHATSAPP"
    channelToText DComm.CH_WEB = "WEB"

-- | Process a fleet communication delivery from Kafka payload (called by consumer).
-- Sends via the appropriate channel and updates delivery status to SENT or FAILED.
-- | Used by Kafka consumer when processing PUSH/SMS/WhatsApp deliveries; sends via FCM/SMS/WhatsApp APIs and updates delivery status.
processFleetCommunicationDeliveryPayload ::
  ( MonadIO m,
    EsqDBFlow m r,
    MonadFlow m,
    CacheFlow m r,
    EncFlow m r,
    CoreMetrics m,
    HasField "requestId" r (Maybe Text),
    HasFlowEnv m r '["smsCfg" ::: SmsConfig],
    HasKafkaProducer r
  ) =>
  CommunicationDeliveryDispatchPayload ->
  m ()
processFleetCommunicationDeliveryPayload payload = do
  let deliveryId = Kernel.Types.Id.Id payload.deliveryId
  result <- try @_ @SomeException $ dispatchFromPayload payload
  case result of
    Right _ -> QDelivery.updateStatusById DDelivery.DS_SENT deliveryId
    Left err -> do
      logError $ "Fleet communication dispatch failed for delivery " <> payload.deliveryId <> ": " <> show err
      QDelivery.updateStatusById DDelivery.DS_FAILED deliveryId

dispatchFromPayload ::
  ( MonadIO m,
    EsqDBFlow m r,
    MonadFlow m,
    CacheFlow m r,
    EncFlow m r,
    CoreMetrics m,
    HasField "requestId" r (Maybe Text),
    HasFlowEnv m r '["smsCfg" ::: SmsConfig],
    HasKafkaProducer r
  ) =>
  CommunicationDeliveryDispatchPayload ->
  m ()
dispatchFromPayload p = do
  let recipientId = Kernel.Types.Id.Id p.recipientId
      merchantId = Kernel.Types.Id.Id p.merchantId
      merchantOpCityId = Kernel.Types.Id.Id p.merchantOperatingCityId
  case p.channel of
    "PUSH" -> do
      person <- QPerson.findById recipientId >>= fromMaybeM (InvalidRequest $ "Recipient not found: " <> Kernel.Types.Id.getId recipientId)
      Notify.sendNotificationToDriver merchantOpCityId FCM.SHOW (Just FCM.HIGH) FCM.NEW_MESSAGE p.title p.body person person.deviceToken
    "SMS" -> do
      person <- QPerson.findById recipientId >>= fromMaybeM (InvalidRequest $ "Recipient not found: " <> Kernel.Types.Id.getId recipientId)
      mobileNumber <- mapM decrypt person.mobileNumber >>= fromMaybeM (InvalidRequest "Recipient phone not available for SMS")
      let phoneNumber = fromMaybe "+91" person.mobileCountryCode <> mobileNumber
      smsCfg <- asks (.smsCfg)
      -- Look up the DLT-registered template from merchant_message by domain+channel
      mbMerchantMsg <- QMM.findByMerchantOpCityIdDomainAndChannel merchantOpCityId (Just DMM.FLEET) (Just DMM.SMS)
      case mbMerchantMsg of
        Just merchantMsg -> do
          -- Substitute {#var1#} with the body text to match the DLT template
          let smsBody = T.replace "{#var1#}" p.body merchantMsg.message
              sender = fromMaybe smsCfg.sender merchantMsg.senderHeader
          Sms.sendSMS merchantId merchantOpCityId (Sms.SendSMSReq smsBody phoneNumber sender merchantMsg.templateId merchantMsg.messageType) >>= Sms.checkSmsResult
        Nothing -> do
          -- Fallback: send as-is if no template found
          logWarning $ "No merchant_message template found for FLEET SMS, sending body as-is for delivery " <> p.deliveryId
          Sms.sendSMS merchantId merchantOpCityId (Sms.SendSMSReq p.body phoneNumber smsCfg.sender (fromMaybe "" p.templateId) Nothing) >>= Sms.checkSmsResult
    "WHATSAPP" -> do
      person <- QPerson.findById recipientId >>= fromMaybeM (InvalidRequest $ "Recipient not found: " <> Kernel.Types.Id.getId recipientId)
      mobileNumber <- mapM decrypt person.mobileNumber >>= fromMaybeM (InvalidRequest "Recipient phone not available for WhatsApp")
      let phoneNumber = fromMaybe "+91" person.mobileCountryCode <> mobileNumber
      -- Look up WhatsApp template from merchant_message
      mbMerchantMsg <- QMM.findByMerchantOpCityIdDomainAndChannel merchantOpCityId (Just DMM.FLEET) (Just DMM.WHATSAPP)
      let whatsappTemplateId = maybe (fromMaybe "" p.templateId) (.templateId) mbMerchantMsg
      let req =
            Whatsapp.SendWhatsAppMessageWithTemplateIdApIReq
              { sendTo = phoneNumber,
                templateId = whatsappTemplateId,
                variables = [Just p.title, Just p.body],
                ctaButtonUrl = Nothing,
                containsUrlButton = Nothing
              }
      result <- Whatsapp.whatsAppSendMessageWithTemplateIdAPI merchantId merchantOpCityId req
      when (result._response.status /= "success") $ throwError (InvalidRequest "WhatsApp send failed")
    "EMAIL" -> logInfo $ "EMAIL dispatch skipped for delivery " <> p.deliveryId <> " - use sendEmailWithAttachment for custom subject/body"
    _ -> throwError (InvalidRequest $ "Unknown channel: " <> p.channel)

mkSentListItem :: DComm.Communication -> CommAPI.CommunicationListItem
mkSentListItem comm =
  CommAPI.CommunicationListItem
    { id = Kernel.Types.Id.cast comm.id,
      title = comm.title,
      body = comm.body,
      msgContentType = fromDomainContentType comm.contentType,
      channels = map fromDomainChannel comm.channels,
      status = fromDomainStatus comm.status,
      domain = fromDomainDomain comm.domain,
      senderName = comm.senderDisplayName,
      senderRole = fromDomainSenderRole comm.senderRole,
      deliverySummary = Nothing,
      isRead = Nothing,
      channel = Nothing,
      createdAt = comm.createdAt,
      sentAt = Just comm.updatedAt
    }

mkReceivedListItem :: DDelivery.CommunicationDelivery -> Environment.Flow CommAPI.CommunicationListItem
mkReceivedListItem delivery = do
  comm <- QComm.findById delivery.communicationId >>= fromMaybeM (InvalidRequest "Communication not found for delivery")
  return $
    CommAPI.CommunicationListItem
      { id = Kernel.Types.Id.cast comm.id,
        title = comm.title,
        body = comm.body,
        msgContentType = fromDomainContentType comm.contentType,
        channels = map fromDomainChannel comm.channels,
        status = fromDomainStatus comm.status,
        domain = fromDomainDomain comm.domain,
        senderName = comm.senderDisplayName,
        senderRole = fromDomainSenderRole comm.senderRole,
        deliverySummary = Nothing,
        isRead = Just (delivery.status == DDelivery.DS_READ),
        channel = Just (fromDomainChannel delivery.channel),
        createdAt = comm.createdAt,
        sentAt = Just comm.updatedAt
      }

mkDeliverySummary :: [DDelivery.CommunicationDelivery] -> CommAPI.DeliverySummary
mkDeliverySummary deliveries =
  CommAPI.DeliverySummary
    { total = length deliveries,
      sent = length $ filter (\d -> d.status == DDelivery.DS_SENT) deliveries,
      delivered = length $ filter (\d -> d.status == DDelivery.DS_DELIVERED) deliveries,
      read = length $ filter (\d -> d.status == DDelivery.DS_READ) deliveries,
      failed = length $ filter (\d -> d.status == DDelivery.DS_FAILED) deliveries
    }

mkDeliveryStatusItem :: DDelivery.CommunicationDelivery -> CommAPI.DeliveryStatusItem
mkDeliveryStatusItem d =
  CommAPI.DeliveryStatusItem
    { id = Kernel.Types.Id.getId d.id,
      recipientId = Kernel.Types.Id.getId d.recipientId,
      recipientName = Nothing,
      recipientRole = fromDomainRecipientRole d.recipientRole,
      channel = fromDomainChannel d.channel,
      status = fromDomainDeliveryStatus d.status,
      failureReason = d.failureReason,
      deliveredAt = d.deliveredAt,
      readAt = d.readAt,
      createdAt = d.createdAt
    }

-- Type conversions between API types and domain types

toDomainDomain :: CommAPI.CommunicationDomainType -> DComm.CommunicationDomain
toDomainDomain CommAPI.COMM_FLEET = DComm.CD_FLEET
toDomainDomain CommAPI.COMM_RIDE_HAILING = DComm.CD_RIDE_HAILING
toDomainDomain CommAPI.COMM_GENERAL = DComm.CD_GENERAL

fromDomainDomain :: DComm.CommunicationDomain -> CommAPI.CommunicationDomainType
fromDomainDomain DComm.CD_FLEET = CommAPI.COMM_FLEET
fromDomainDomain DComm.CD_RIDE_HAILING = CommAPI.COMM_RIDE_HAILING
fromDomainDomain DComm.CD_GENERAL = CommAPI.COMM_GENERAL

toDomainSenderRole :: CommAPI.CommunicationRoleType -> DComm.CommunicationSenderRole
toDomainSenderRole CommAPI.ROLE_ADMIN = DComm.SR_ADMIN
toDomainSenderRole CommAPI.ROLE_OPERATOR = DComm.SR_OPERATOR
toDomainSenderRole CommAPI.ROLE_FLEET_OWNER = DComm.SR_FLEET_OWNER
toDomainSenderRole CommAPI.ROLE_DRIVER = DComm.SR_ADMIN

fromDomainSenderRole :: DComm.CommunicationSenderRole -> CommAPI.CommunicationRoleType
fromDomainSenderRole DComm.SR_ADMIN = CommAPI.ROLE_ADMIN
fromDomainSenderRole DComm.SR_OPERATOR = CommAPI.ROLE_OPERATOR
fromDomainSenderRole DComm.SR_FLEET_OWNER = CommAPI.ROLE_FLEET_OWNER

toDomainContentType :: CommAPI.CommunicationContentTypeEnum -> DComm.CommunicationContentType
toDomainContentType CommAPI.CONTENT_TEXT = DComm.CT_TEXT
toDomainContentType CommAPI.CONTENT_IMAGE = DComm.CT_IMAGE
toDomainContentType CommAPI.CONTENT_VIDEO = DComm.CT_VIDEO

fromDomainContentType :: DComm.CommunicationContentType -> CommAPI.CommunicationContentTypeEnum
fromDomainContentType DComm.CT_TEXT = CommAPI.CONTENT_TEXT
fromDomainContentType DComm.CT_IMAGE = CommAPI.CONTENT_IMAGE
fromDomainContentType DComm.CT_VIDEO = CommAPI.CONTENT_VIDEO

toDomainChannel :: CommAPI.CommunicationChannelType -> DComm.ChannelType
toDomainChannel CommAPI.CH_PUSH = DComm.CH_PUSH
toDomainChannel CommAPI.CH_SMS = DComm.CH_SMS
toDomainChannel CommAPI.CH_EMAIL = DComm.CH_EMAIL
toDomainChannel CommAPI.CH_WHATSAPP = DComm.CH_WHATSAPP
toDomainChannel CommAPI.CH_WEB = DComm.CH_WEB

fromDomainChannel :: DComm.ChannelType -> CommAPI.CommunicationChannelType
fromDomainChannel DComm.CH_PUSH = CommAPI.CH_PUSH
fromDomainChannel DComm.CH_SMS = CommAPI.CH_SMS
fromDomainChannel DComm.CH_EMAIL = CommAPI.CH_EMAIL
fromDomainChannel DComm.CH_WHATSAPP = CommAPI.CH_WHATSAPP
fromDomainChannel DComm.CH_WEB = CommAPI.CH_WEB

toDomainDeliveryStatus :: CommAPI.CommunicationDeliveryStatusType -> DDelivery.DeliveryStatus
toDomainDeliveryStatus CommAPI.DLV_PENDING = DDelivery.DS_PENDING
toDomainDeliveryStatus CommAPI.DLV_SENT = DDelivery.DS_SENT
toDomainDeliveryStatus CommAPI.DLV_DELIVERED = DDelivery.DS_DELIVERED
toDomainDeliveryStatus CommAPI.DLV_READ = DDelivery.DS_READ
toDomainDeliveryStatus CommAPI.DLV_FAILED = DDelivery.DS_FAILED

fromDomainDeliveryStatus :: DDelivery.DeliveryStatus -> CommAPI.CommunicationDeliveryStatusType
fromDomainDeliveryStatus DDelivery.DS_PENDING = CommAPI.DLV_PENDING
fromDomainDeliveryStatus DDelivery.DS_SENT = CommAPI.DLV_SENT
fromDomainDeliveryStatus DDelivery.DS_DELIVERED = CommAPI.DLV_DELIVERED
fromDomainDeliveryStatus DDelivery.DS_READ = CommAPI.DLV_READ
fromDomainDeliveryStatus DDelivery.DS_FAILED = CommAPI.DLV_FAILED

fromDomainStatus :: DComm.CommunicationStatus -> CommAPI.CommunicationStatusType
fromDomainStatus DComm.ST_DRAFT = CommAPI.CS_DRAFT
fromDomainStatus DComm.ST_QUEUED = CommAPI.CS_QUEUED
fromDomainStatus DComm.ST_SENDING = CommAPI.CS_SENDING
fromDomainStatus DComm.ST_SENT = CommAPI.CS_SENT
fromDomainStatus DComm.ST_FAILED = CommAPI.CS_FAILED
fromDomainStatus DComm.ST_SCHEDULED = CommAPI.CS_SCHEDULED

fromDomainRecipientRole :: DDelivery.CommunicationRecipientRole -> CommAPI.CommunicationRoleType
fromDomainRecipientRole DDelivery.RR_DRIVER = CommAPI.ROLE_DRIVER
fromDomainRecipientRole DDelivery.RR_FLEET_OWNER = CommAPI.ROLE_FLEET_OWNER
fromDomainRecipientRole DDelivery.RR_OPERATOR = CommAPI.ROLE_OPERATOR
fromDomainRecipientRole DDelivery.RR_ADMIN = CommAPI.ROLE_ADMIN

toDomainCTA :: CommAPI.CTAButtonReq -> DComm.CTAButton
toDomainCTA cta = DComm.CTAButton {label = cta.label, url = cta.url, linkType = cta.linkType}

fromDomainCTA :: DComm.CTAButton -> CommAPI.CTAButtonReq
fromDomainCTA cta = CommAPI.CTAButtonReq {label = cta.label, url = cta.url, linkType = cta.linkType}

-- | GET /communication/template?domain=...&channel=...
-- Returns templateId, messageBody and templateName from MerchantMessage
-- for the given communication domain and channel.
getCommunicationTemplate ::
  Kernel.Types.Id.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  CommAPI.CommunicationDomainType ->
  CommAPI.CommunicationChannelType ->
  Environment.Flow CommAPI.CommunicationTemplateResponse
getCommunicationTemplate merchantShortId opCity apiDomain apiChannel = do
  merchant <- CQM.findByShortId merchantShortId >>= fromMaybeM (MerchantNotFound merchantShortId.getShortId)
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let mmDomain :: DMM.MessageDomain
      mmDomain =
        case apiDomain of
          CommAPI.COMM_FLEET -> DMM.FLEET
          CommAPI.COMM_RIDE_HAILING -> DMM.RIDE_HAILING
          CommAPI.COMM_GENERAL -> DMM.GENERAL
      mmChannel :: DMM.MediaChannel
      mmChannel =
        case apiChannel of
          CommAPI.CH_SMS -> DMM.SMS
          CommAPI.CH_WHATSAPP -> DMM.WHATSAPP
          _ -> DMM.SMS -- fallback for unsupported channels
  mbTemplate <- QMM.findByMerchantOpCityIdDomainAndChannel merchantOpCityId (Just mmDomain) (Just mmChannel)
  template <- fromMaybeM (InvalidRequest "Template not found for given domain & channel") mbTemplate
  pure
    CommAPI.CommunicationTemplateResponse
      { templateId = template.templateId,
        messageBody = template.message,
        templateName = template.templateName
      }
