{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.CallbackRequest where

import Domain.Types.CallbackRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption (Encrypted (..), EncryptedHashed (..))
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Storage.Beam.CallbackRequest as BeamCR

create :: MonadFlow m => CallbackRequest -> m ()
create = createWithKV

instance FromTType' BeamCR.CallbackRequest CallbackRequest where
  fromTType' BeamCR.CallbackRequestT {..} = do
    pure $
      Just
        CallbackRequest
          { id = Id id,
            merchantId = Id merchantId,
            customerName = customerName,
            customerPhone = EncryptedHashed (Encrypted customerPhoneEncrypted) customerPhoneHash,
            customerMobileCountryCode = customerMobileCountryCode,
            status = status,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' BeamCR.CallbackRequest CallbackRequest where
  toTType' CallbackRequest {..} = do
    BeamCR.CallbackRequestT
      { BeamCR.id = getId id,
        BeamCR.merchantId = getId merchantId,
        BeamCR.customerName = customerName,
        BeamCR.customerPhoneEncrypted = customerPhone & unEncrypted . (.encrypted),
        BeamCR.customerPhoneHash = customerPhone & (.hash),
        BeamCR.customerMobileCountryCode = customerMobileCountryCode,
        BeamCR.status = status,
        BeamCR.createdAt = createdAt,
        BeamCR.updatedAt = updatedAt
      }
