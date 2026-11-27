{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Who to reach for a person. A fleet is reached on its own contact (the phone and email set on
-- the fleet, see @fleet_owner_information@), which payments, payouts and alerts read, and which
-- belongs to no one's login. A fleet with no contact set, which is every existing fleet, falls back
-- to the owner's own phone and email, so nothing changes until a contact is set. Anyone who is not
-- a fleet owner is reached on their own phone and email.
module SharedLogic.FleetContact
  ( Contact (..),
    contactOf,
  )
where

import Control.Applicative ((<|>))
import qualified Domain.Types.Person as DP
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import Kernel.Utils.Common
import qualified Storage.Queries.FleetOwnerInformation as QFOI

-- | A contact, decrypted. Each part can be missing.
data Contact = Contact
  { mobileCountryCode :: Maybe Text,
    mobileNumber :: Maybe Text,
    email :: Maybe Text
  }
  deriving (Show, Eq)

contactOf :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r) => DP.Person -> m Contact
contactOf person = do
  ownMobileNumber <- mapM decrypt person.mobileNumber
  let own = Contact {mobileCountryCode = person.mobileCountryCode, mobileNumber = ownMobileNumber, email = person.email}
  if person.role `elem` [DP.FLEET_OWNER, DP.FLEET_BUSINESS]
    then
      QFOI.findByPrimaryKey person.id >>= \case
        Nothing -> pure own
        Just fleet -> do
          fleetMobileNumber <- mapM decrypt fleet.contactMobileNumber
          fleetEmail <- mapM decrypt fleet.contactEmail
          pure
            Contact
              { mobileCountryCode = if isJust fleetMobileNumber then fleet.contactMobileCountryCode <|> own.mobileCountryCode else own.mobileCountryCode,
                mobileNumber = fleetMobileNumber <|> own.mobileNumber,
                email = fleetEmail <|> own.email
              }
    else pure own
