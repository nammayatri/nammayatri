module SharedLogic.Fleet where

import qualified Domain.Types.FleetMemberAssociation as DFMA
import Environment
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.FleetMemberAssociation as FMA
import qualified Storage.Queries.Person as QP

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
          fleetMemberAssociation <- FMA.findOneByFleetOwnerId fleetOwnerId True >>= fromMaybeM (PersonNotFound fleetOwnerId)
          person <- QP.findById (Id fleetMemberAssociation.fleetOwnerId) >>= fromMaybeM (PersonNotFound fleetMemberAssociation.fleetOwnerId)
          return [(fleetOwnerId, person.firstName <> maybe "" (" " <>) person.lastName)]
      )
        <$> mbFleetOwnerId
    )

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
    requestorId :: Text
  }
  deriving (Show, Generic, Read, Eq, Ord, ToJSON, FromJSON)

getFleetOwnersInfoMerchantBased :: Maybe Text -> Maybe Text -> Maybe Bool -> Maybe Bool -> Flow [FleetOwnerInfo]
getFleetOwnersInfoMerchantBased mbFleetOwnerId mbRequestorId hasFleetMemberHierarchy mbIsRequestorFleerOwner = do
  requestorId <- mbRequestorId & fromMaybeM (InvalidRequest "requestorId required")
  case hasFleetMemberHierarchy of
    Just False -> do
      -- MSIL: requestor is fleet owner or operator, access check on bpp side required!
      requestor <- QP.findById (Id requestorId) >>= fromMaybeM (PersonNotFound requestorId)
      if fromMaybe False mbIsRequestorFleerOwner
        then do
          -- requestor is fleet owner
          whenJust mbFleetOwnerId $ \fleetOwnerId ->
            unless (fleetOwnerId == requestorId) $ throwError AccessDenied
          let fleetOwnerName = requestor.firstName <> maybe "" (" " <>) requestor.lastName
          return [FleetOwnerInfo {fleetOwnerId = requestorId, fleetOwnerName, requestorId}]
        else do
          -- requestor is operator
          fleetOwnerId <- mbFleetOwnerId & fromMaybeM (InvalidRequest "fleetOwnerId required")
          fleetOwner <- QP.findById (Id fleetOwnerId) >>= fromMaybeM (PersonNotFound fleetOwnerId)
          let fleetOwnerName = fleetOwner.firstName <> maybe "" (" " <>) fleetOwner.lastName
          return [FleetOwnerInfo {fleetOwnerId, fleetOwnerName, requestorId}]
    _ -> do
      -- Existing flow: consider requestor the same as fleet owner, fleet member operates on befalf of fleet owner
      fleetOwnerIds <- getFleetOwnerIds requestorId mbFleetOwnerId
      return $ (\(fleetOwnerId, fleetOwnerName) -> FleetOwnerInfo {fleetOwnerId, fleetOwnerName, requestorId = fleetOwnerId}) <$> fleetOwnerIds
