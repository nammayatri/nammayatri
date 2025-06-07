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
                  fleetMemberAssociation <- FMA.findOneByFleetOwnerId fleetOwnerId True >>= fromMaybeM (PersonNotFound fleetOwnerId)
                  person <- QP.findById (Id fleetMemberAssociation.fleetMemberId) >>= fromMaybeM (PersonNotFound fleetMemberAssociation.fleetMemberId)
                  return (fleetOwnerId, person.firstName <> maybe "" (" " <>) person.lastName)
              )
              fleetMemberAssociations
    )
    identity
    ( ( \fleetOwnerId -> do
          fleetMemberAssociation <- FMA.findOneByFleetOwnerId fleetOwnerId True >>= fromMaybeM (PersonNotFound fleetOwnerId)
          person <- QP.findById (Id fleetMemberAssociation.fleetMemberId) >>= fromMaybeM (PersonNotFound fleetMemberAssociation.fleetMemberId)
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
