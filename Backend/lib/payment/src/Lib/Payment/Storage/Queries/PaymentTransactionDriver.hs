{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Payment.Storage.Queries.PaymentTransactionDriver where

import Kernel.Beam.Functions (FromTType' (fromTType'), ToTType' (toTType'), createWithKV, findAllWithOptionsKV, findOneWithKV, updateOneWithKV)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow, getCurrentTime)
import Lib.Payment.Domain.Types.PaymentOrder (PaymentOrder)
import Lib.Payment.Domain.Types.PaymentTransaction
import qualified Lib.Payment.Storage.Beam.PaymentTransactionDriver as BeamPT
import qualified Sequelize as Se

create :: MonadFlow m => PaymentTransaction -> m ()
create = createWithKV

updateMultiple :: MonadFlow m => PaymentTransaction -> m ()
updateMultiple transaction = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamPT.statusId transaction.statusId,
      Se.Set BeamPT.status transaction.status,
      Se.Set BeamPT.paymentMethodType transaction.paymentMethodType,
      Se.Set BeamPT.paymentMethod transaction.paymentMethod,
      Se.Set BeamPT.respMessage transaction.respMessage,
      Se.Set BeamPT.respCode transaction.respCode,
      Se.Set BeamPT.gatewayReferenceId transaction.gatewayReferenceId,
      Se.Set BeamPT.amount transaction.amount,
      Se.Set BeamPT.currency transaction.currency,
      Se.Set BeamPT.juspayResponse transaction.juspayResponse,
      Se.Set BeamPT.mandateStatus transaction.mandateStatus,
      Se.Set BeamPT.mandateStartDate transaction.mandateStartDate,
      Se.Set BeamPT.mandateEndDate transaction.mandateEndDate,
      Se.Set BeamPT.mandateId transaction.mandateId,
      Se.Set BeamPT.mandateFrequency transaction.mandateFrequency,
      Se.Set BeamPT.mandateMaxAmount transaction.mandateMaxAmount,
      Se.Set BeamPT.updatedAt now
    ]
    [Se.Is BeamPT.id $ Se.Eq $ transaction.id.getId]

findByTxnUUID :: MonadFlow m => Text -> m (Maybe PaymentTransaction)
findByTxnUUID txnUUID = findOneWithKV [Se.Is BeamPT.txnUUID $ Se.Eq $ Just txnUUID]

findAllByOrderId :: MonadFlow m => Id PaymentOrder -> m [PaymentTransaction]
findAllByOrderId (Id orderId) = findAllWithOptionsKV [Se.Is BeamPT.orderId $ Se.Eq orderId] (Se.Desc BeamPT.createdAt) Nothing Nothing

findNewTransactionByOrderId :: MonadFlow m => Id PaymentOrder -> m (Maybe PaymentTransaction)
findNewTransactionByOrderId (Id orderId) = findAllWithOptionsKV [Se.Is BeamPT.txnUUID $ Se.Eq Nothing, Se.Is BeamPT.orderId $ Se.Eq orderId] (Se.Desc BeamPT.createdAt) (Just 1) Nothing <&> listToMaybe

instance FromTType' BeamPT.PaymentTransaction PaymentTransaction where
  fromTType' BeamPT.PaymentTransactionT {..} = do
    pure $
      Just
        PaymentTransaction
          { id = Id id,
            orderId = Id orderId,
            merchantId = Id merchantId,
            ..
          }

instance ToTType' BeamPT.PaymentTransaction PaymentTransaction where
  toTType' PaymentTransaction {..} =
    BeamPT.PaymentTransactionT
      { BeamPT.id = getId id,
        BeamPT.orderId = getId orderId,
        BeamPT.merchantId = merchantId.getId,
        ..
      }
