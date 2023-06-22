{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Merchant.MerchantMessage
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Merchant as DOrg
import Domain.Types.Merchant.MerchantMessage
import Kernel.Prelude
import Kernel.Storage.Esqueleto hiding (findById)
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Storage.Beam.Merchant.MerchantMessage as BeamMM
import Storage.Tabular.Merchant.MerchantMessage ()

findByMerchantIdAndMessageKey :: Transactionable m => Id Merchant -> MessageKey -> m (Maybe MerchantMessage)
findByMerchantIdAndMessageKey merchantId messageKey =
  Esq.findById (merchantId, messageKey)

transformBeamMerchantMessageToDomain :: BeamMM.MerchantMessage -> MerchantMessage
transformBeamMerchantMessageToDomain BeamMM.MerchantMessageT {..} = do
  MerchantMessage
    { merchantId = Id merchantId,
      messageKey = messageKey,
      message = message,
      updatedAt = updatedAt,
      createdAt = createdAt
    }

transformDomainMerchantMessageToBeam :: MerchantMessage -> BeamMM.MerchantMessage
transformDomainMerchantMessageToBeam MerchantMessage {..} =
  BeamMM.defaultMerchantMessage
    { BeamMM.merchantId = getId merchantId,
      BeamMM.messageKey = messageKey,
      BeamMM.message = message,
      BeamMM.updatedAt = updatedAt,
      BeamMM.createdAt = createdAt
    }
