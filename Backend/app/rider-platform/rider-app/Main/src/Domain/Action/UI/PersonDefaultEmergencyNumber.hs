{-# LANGUAGE ApplicativeDo #-}

module Domain.Action.UI.PersonDefaultEmergencyNumber
  ( PersonDefaultEmergencyNumberAPIEntity (..),
    PersonDefaultEmergencyNumber,
    fromSafetyRideShare,
    makePersonDefaultEmergencyNumberAPIEntity,
    findpersonENListWithFallBack,
    toSafetyRideShare,
  )
where

import Domain.Types.Merchant as DM
import Domain.Types.Person (Person, RideShareOptions (..))
import Kernel.Beam.Functions (runInReplica)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM)
import qualified Safety.Domain.Types.Common as SafetyCommon
import qualified Safety.Domain.Types.PersonDefaultEmergencyNumber as SafetyPDEN
import Safety.Storage.BeamFlow
import qualified Safety.Storage.Queries.PersonDefaultEmergencyNumber as Lib
import Storage.Beam.Sos ()
import qualified Storage.Queries.Person as QPerson
import Tools.Error

-- Type alias for API/spec compatibility (table and domain live in Safety.*).
type PersonDefaultEmergencyNumber = PersonDefaultEmergencyNumberAPIEntity

data PersonDefaultEmergencyNumberAPIEntity = PersonDefaultEmergencyNumberAPIEntity
  { personId :: Id Person,
    name :: Text,
    mobileCountryCode :: Text,
    mobileNumber :: Text,
    priority :: Int,
    contactPersonId :: Maybe (Id Person),
    merchantId :: Maybe (Id DM.Merchant),
    enableForFollowing :: Bool,
    enableForShareRide :: Bool,
    onRide :: Bool,
    shareTripWithEmergencyContactOption :: Maybe RideShareOptions
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema, Eq)

fromSafetyRideShare :: SafetyCommon.RideShareOptions -> RideShareOptions
fromSafetyRideShare = \case
  SafetyCommon.ALWAYS_SHARE -> ALWAYS_SHARE
  SafetyCommon.SHARE_WITH_TIME_CONSTRAINTS -> SHARE_WITH_TIME_CONSTRAINTS
  SafetyCommon.NEVER_SHARE -> NEVER_SHARE

toSafetyRideShare :: RideShareOptions -> SafetyCommon.RideShareOptions
toSafetyRideShare = \case
  ALWAYS_SHARE -> SafetyCommon.ALWAYS_SHARE
  SHARE_WITH_TIME_CONSTRAINTS -> SafetyCommon.SHARE_WITH_TIME_CONSTRAINTS
  NEVER_SHARE -> SafetyCommon.NEVER_SHARE

-- | Rider: ensure PDEN list has share option; if not set, use Person.shareTripWithEmergencyContactOption.
findpersonENListWithFallBack :: (BeamFlow m r) => Id Person -> Maybe Person -> m [SafetyPDEN.PersonDefaultEmergencyNumber]
findpersonENListWithFallBack personId mbPerson = do
  personENList <- Lib.findAllByPersonId (cast personId)
  let isUpdated = any (\item -> isJust (SafetyPDEN.shareTripWithEmergencyContactOption item)) personENList
  if isUpdated
    then return personENList
    else do
      person <- maybe (runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound (getId personId))) return mbPerson
      let safetyOpt = toSafetyRideShare <$> person.shareTripWithEmergencyContactOption
      Lib.updateShareTripWithEmergencyContactOptions (cast personId) safetyOpt
      return $ map (\p -> p {SafetyPDEN.shareTripWithEmergencyContactOption = safetyOpt}) personENList

makePersonDefaultEmergencyNumberAPIEntity :: Bool -> SafetyPDEN.DecryptedPersonDefaultEmergencyNumber -> PersonDefaultEmergencyNumberAPIEntity
makePersonDefaultEmergencyNumberAPIEntity onRide SafetyPDEN.PersonDefaultEmergencyNumber {..} =
  PersonDefaultEmergencyNumberAPIEntity
    { personId = cast personId,
      name,
      mobileCountryCode,
      mobileNumber,
      priority,
      contactPersonId = cast <$> contactPersonId,
      merchantId = cast <$> merchantId,
      enableForFollowing,
      enableForShareRide,
      onRide,
      shareTripWithEmergencyContactOption = fromSafetyRideShare <$> shareTripWithEmergencyContactOption
    }
