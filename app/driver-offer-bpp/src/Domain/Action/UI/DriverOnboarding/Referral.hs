{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Action.UI.DriverOnboarding.Referral where

import Beckn.External.Encryption (encrypt)
import Beckn.Prelude
import Beckn.Storage.Esqueleto hiding (isNothing)
import Beckn.Types.APISuccess
import Beckn.Types.Id
import qualified Domain.Types.Person as Person
import Environment
import qualified Storage.Queries.Person as Person

newtype ReferralReq = ReferralReq
  {value :: Text}
  deriving (Generic, ToSchema, ToJSON, FromJSON)

type ReferralRes = APISuccess

addReferral ::
  Id Person.Person ->
  ReferralReq ->
  Flow ReferralRes
addReferral personId req = do
  value <- encrypt req.value -- no requset validation required as referral code could be anything
  runTransaction $ Person.addReferralCode personId value
  return Success
