module SharedLogic.Fleet where

import qualified Domain.Types.FleetMemberAssociation as DFMA
import qualified "lib-dashboard" Domain.Types.FleetScopeMode as DFSM
import qualified Domain.Types.Person as DP
import Environment
import Kernel.Beam.Functions (runInDashboardDb)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import "lib-dashboard" Storage.Beam.SchemaInstances ()
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDAExtra
import qualified Storage.Queries.FleetMemberAssociation as FMA
import qualified Storage.Queries.FleetOperatorAssociationExtra as QFOAExtra
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import qualified "lib-dashboard" Storage.Queries.Merchant as QDashMerchant
import qualified Storage.Queries.Person as QP
import qualified "lib-dashboard" Storage.Queries.Person as QDashPerson

--------------------------------------- Single Fleet Owners Access --------------------------------------
getFleetOwnerId :: Text -> Maybe Text -> Flow Text
getFleetOwnerId memberPersonId mbFleetOwnerId = do
  maybe
    ( FMA.findAllActiveByfleetMemberId memberPersonId True
        >>= \case
          [] -> return memberPersonId
          [DFMA.FleetMemberAssociation {..}] -> return fleetOwnerId
          _ -> throwError AccessDenied
    )
    identity
    ((verifyFleetOwnerAccess memberPersonId) <$> mbFleetOwnerId)

------------------------------------- Multiple Fleet Owners Access --------------------------------------
getFleetOwnerIds :: Text -> Maybe Text -> Flow [(Text, Text)]
getFleetOwnerIds memberPersonId mbFleetOwnerId = do
  maybe
    ( FMA.findAllActiveByfleetMemberId memberPersonId True
        >>= \case
          [] -> do
            person <- QP.findById (Id memberPersonId) >>= fromMaybeM (PersonNotFound memberPersonId)
            return [(memberPersonId, person.firstName <> maybe "" (" " <>) person.lastName)]
          fleetMemberAssociations -> do
            mapM
              ( \DFMA.FleetMemberAssociation {..} -> do
                  person <- QP.findById (Id fleetOwnerId) >>= fromMaybeM (PersonNotFound fleetOwnerId)
                  return (fleetOwnerId, person.firstName <> maybe "" (" " <>) person.lastName)
              )
              fleetMemberAssociations
    )
    identity
    ( ( \fleetOwnerId -> do
          -- A fleet made as a scope has no member association: it is its own NO_LOGIN person. Outside ENFORCE
          -- every other fleet still needs the association.
          mbFleetMemberAssociation <- FMA.findOneByFleetOwnerId fleetOwnerId True
          person <- QP.findById (Id fleetOwnerId) >>= fromMaybeM (PersonNotFound fleetOwnerId)
          unless (isJust mbFleetMemberAssociation || person.identifierType == DP.NO_LOGIN) $ do
            allowed <- mayBeNamedWithoutAssociation memberPersonId fleetOwnerId
            unless allowed $ throwError (PersonNotFound fleetOwnerId)
          return [(fleetOwnerId, person.firstName <> maybe "" (" " <>) person.lastName)]
      )
        <$> mbFleetOwnerId
    )

-- | Whether a fleet with no member association may still be named. Only once the fleet's merchant enforces
-- fleet scope, where new fleets have no such row: then an operator has to operate the fleet, a fleet owner
-- may name only their own, and anyone else is held by their capabilities, as on a fleet that has the row.
mayBeNamedWithoutAssociation :: Text -> Text -> Flow Bool
mayBeNamedWithoutAssociation memberPersonId fleetOwnerId = do
  enforced <- runInDashboardDb $ do
    mbDashboardFleet <- QDashPerson.findById (Id fleetOwnerId)
    mbMerchant <- maybe (pure Nothing) QDashMerchant.findById (mbDashboardFleet >>= (.merchantId))
    pure $ ((.fleetScopeMode) <$> mbMerchant) == Just DFSM.ENFORCE
  if not enforced
    then pure False
    else
      QP.findById (Id memberPersonId) >>= \case
        Just member
          | member.role == DP.OPERATOR -> isJust <$> QFOAExtra.findByFleetOwnerIdAndOperatorId (Id fleetOwnerId) member.id True
          | member.role `elem` [DP.FLEET_OWNER, DP.FLEET_BUSINESS] -> pure (memberPersonId == fleetOwnerId)
        _ -> pure True

------------------------------------- Verify Fleet Owners Access --------------------------------------
verifyFleetOwnerAccess :: Text -> Text -> Flow Text
verifyFleetOwnerAccess fleetMemberId accessedFleetOwnerId = do
  fleetOwnerIds <- getFleetOwnerIds fleetMemberId Nothing
  (fleetOwnerId, _) <- find (\(fleetOwnerId, _) -> fleetOwnerId == accessedFleetOwnerId) fleetOwnerIds & fromMaybeM AccessDenied
  return fleetOwnerId

-------------------------------------- get Fleet Owner Info merchant based ---------------------------------------

data FleetOwnerInfo = FleetOwnerInfo
  { fleetOwnerId :: Text,
    fleetOwnerName :: Text,
    fleetName :: Maybe Text,
    requestorId :: Text
  }
  deriving (Show, Generic, Read, Eq, Ord, ToJSON, FromJSON)

getFleetOwnersInfoMerchantBased :: Maybe Text -> Maybe Text -> Maybe Bool -> Maybe Bool -> Flow [FleetOwnerInfo]
getFleetOwnersInfoMerchantBased mbFleetOwnerId mbRequestorId hasFleetMemberHierarchy mbIsRequestorFleerOwner = do
  requestorId <- mbRequestorId & fromMaybeM (InvalidRequest "requestorId required")
  case hasFleetMemberHierarchy of
    Just False -> do
      requestor <- QP.findById (Id requestorId)
      case requestor of
        Nothing -> do
          fleetOwnerId <- mbFleetOwnerId & fromMaybeM (InvalidRequest "fleetOwnerId required")
          fleetOwner <- QP.findById (Id fleetOwnerId) >>= fromMaybeM (PersonNotFound fleetOwnerId)
          mbFleetOwnerInfo <- QFOI.findByPrimaryKey (Id fleetOwnerId)
          let fleetOwnerName = fleetOwner.firstName <> maybe "" (" " <>) fleetOwner.lastName
          return [FleetOwnerInfo {fleetOwnerId, fleetOwnerName, fleetName = mbFleetOwnerInfo >>= (.fleetName), requestorId = fleetOwnerId}] -- requesterId is not being used in parent functions add fleetOwnerId for backward compatibility
        Just requestorVal -> do
          if fromMaybe False mbIsRequestorFleerOwner
            then do
              -- requestor is fleet owner
              whenJust mbFleetOwnerId $ \fleetOwnerId ->
                unless (fleetOwnerId == requestorId) $ throwError AccessDenied
              let fleetOwnerName = requestorVal.firstName <> maybe "" (" " <>) requestorVal.lastName
              mbFleetOwnerInfo <- QFOI.findByPrimaryKey requestorVal.id
              return [FleetOwnerInfo {fleetOwnerId = requestorVal.id.getId, fleetOwnerName, fleetName = mbFleetOwnerInfo >>= (.fleetName), requestorId = requestorVal.id.getId}]
            else do
              -- requestor is operator
              fleetOwnerId <- mbFleetOwnerId & fromMaybeM (InvalidRequest "fleetOwnerId required")
              fleetOwner <- QP.findById (Id fleetOwnerId) >>= fromMaybeM (PersonNotFound fleetOwnerId)
              mbFleetOwnerInfo <- QFOI.findByPrimaryKey (Id fleetOwnerId)
              let fleetOwnerName = fleetOwner.firstName <> maybe "" (" " <>) fleetOwner.lastName
              return [FleetOwnerInfo {fleetOwnerId, fleetOwnerName, fleetName = mbFleetOwnerInfo >>= (.fleetName), requestorId}]
    _ -> do
      -- Existing flow: consider requestor the same as fleet owner, fleet member operates on befalf of fleet owner
      fleetOwnerIds <- getFleetOwnerIds requestorId mbFleetOwnerId
      forM fleetOwnerIds $ \(fleetOwnerId, fleetOwnerName) -> do
        mbFleetOwnerInfo <- QFOI.findByPrimaryKey (Id fleetOwnerId)
        pure FleetOwnerInfo {fleetOwnerId, fleetOwnerName, fleetName = mbFleetOwnerInfo >>= (.fleetName), requestorId = fleetOwnerId}

-------------------------------- merchant based fleet owner resolution --------------------------------

-- | The resolution provider-dashboard performed before forwarding a fleet
-- request (@getMbFleetOwnerAndRequestorIdMerchantBased@ there). Routes served
-- here directly have to do the same, or a fleet member acting on behalf of an
-- owner would be treated as the owner themselves.
--
-- The two leading arguments come from the caller's dashboard session:
-- their merchant's @hasFleetMemberHierarchy@ and whether their role is a fleet
-- owner's.
getMbFleetOwnerAndRequestorIdMerchantBased :: Maybe Bool -> Bool -> Text -> Maybe Text -> Flow (Maybe Text, Text)
getMbFleetOwnerAndRequestorIdMerchantBased hasFleetMemberHierarchy isRequestorFleetOwner requestorId mbFleetOwnerId =
  case hasFleetMemberHierarchy of
    Just False ->
      -- MSIL: requestor is fleet owner or operator, access check happens here
      if isRequestorFleetOwner
        then do
          whenJust mbFleetOwnerId $ \fleetOwnerId ->
            unless (fleetOwnerId == requestorId) $ throwError AccessDenied
          return (Just requestorId, requestorId)
        else return (mbFleetOwnerId, requestorId)
    _ -> do
      -- fleet member operates on behalf of a fleet owner
      fleetOwnerId <- getFleetOwnerId requestorId mbFleetOwnerId
      return (Just fleetOwnerId, fleetOwnerId)

-- | As 'getMbFleetOwnerAndRequestorIdMerchantBased', for endpoints where the
-- fleet owner is mandatory.
getFleetOwnerAndRequestorIdMerchantBased :: Maybe Bool -> Bool -> Text -> Maybe Text -> Flow (Text, Text)
getFleetOwnerAndRequestorIdMerchantBased hasFleetMemberHierarchy isRequestorFleetOwner requestorId mbFleetOwnerId = do
  (mbFleetOwnerId', requestorId') <- getMbFleetOwnerAndRequestorIdMerchantBased hasFleetMemberHierarchy isRequestorFleetOwner requestorId mbFleetOwnerId
  fleetOwnerId <- mbFleetOwnerId' & fromMaybeM (InvalidRequest "fleetOwnerId required")
  return (fleetOwnerId, requestorId')

------------------------------------- Who a fleet caller may act on --------------------------------------

-- | A fleet owner or member may act on a subject, a driver or the fleet itself, only when it is a fleet
-- of theirs or a driver in one. The plan and subscription endpoints take the subject as an id in the
-- URL, which nothing otherwise ties to the caller. Callers that are not fleet owners, such as admins,
-- are authorised by capability and are not narrowed here.
assertRequestorMaySeeSubject :: Maybe Text -> Text -> Flow ()
assertRequestorMaySeeSubject Nothing _ = pure ()
assertRequestorMaySeeSubject (Just requestorId) subjectId =
  QP.findById (Id requestorId) >>= \case
    Just requestor | requestor.role `elem` [DP.FLEET_OWNER, DP.FLEET_BUSINESS],
                     subjectId /= requestorId -> do
      fleetIds <- map fst <$> getFleetOwnerIds requestorId Nothing
      unless (subjectId `elem` fleetIds) $ do
        mbAssociation <- QFDAExtra.findByDriverId (Id subjectId) True
        unless (maybe False (\association -> association.fleetOwnerId `elem` fleetIds) mbAssociation) $
          throwError AccessDenied
    _ -> pure ()
