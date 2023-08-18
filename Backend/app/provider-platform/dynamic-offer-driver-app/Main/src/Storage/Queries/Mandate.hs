{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Mandate where

import Domain.Types.Mandate as Domain
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import Storage.Beam.Mandate as BeamM hiding (Id)

-- create :: Mandate -> SqlDB ()
-- create = Esq.create

create :: (L.MonadFlow m, Log m) => Domain.Mandate -> m ()
create = createWithKV

-- findById :: Transactionable m => Id Mandate -> m (Maybe Mandate)
-- findById = Esq.findById

findById :: (L.MonadFlow m, Log m) => Id Domain.Mandate -> m (Maybe Domain.Mandate)
findById (Id mandateId) = findOneWithKV [Se.Is BeamM.id $ Se.Eq mandateId]

-- findByStatus :: Transactionable m => Id Mandate -> [MandateStatus] -> m (Maybe Mandate)
-- findByStatus mandateId status =
--   Esq.findOne $ do
--     mandate <- from $ table @MandateT
--     where_ $
--       mandate ^. MandateTId ==. val (toKey mandateId)
--         &&. mandate ^. MandateStatus `in_` valList status
--     return mandate

findByStatus :: (L.MonadFlow m, Log m) => Text -> [MandateStatus] -> m (Maybe Domain.Mandate)
findByStatus mandateId status = findOneWithKV [Se.And [Se.Is BeamM.id $ Se.Eq mandateId, Se.Is BeamM.status $ Se.In status]]

-- updateStatus :: Id Mandate -> MandateStatus -> SqlDB ()
-- updateStatus mandateId status = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ MandateStatus =. val status,
--         MandateUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. MandateTId ==. val (toKey mandateId)

updateMandateDetails :: (L.MonadFlow m, Log m, MonadTime m) => Id Domain.Mandate -> MandateStatus -> Maybe Text -> Maybe Text -> Maybe Text -> m ()
updateMandateDetails (Id mandateId) status payerVpa payerApp payerAppName = do
  now <- getCurrentTime
  updateOneWithKV
    ( [Se.Set BeamM.status status, Se.Set BeamM.updatedAt now]
        <> [Se.Set BeamM.payerVpa payerVpa | isJust payerVpa]
        <> [Se.Set BeamM.payerApp payerApp | isJust payerApp]
        <> [Se.Set BeamM.payerAppName payerAppName | isJust payerAppName]
    )
    [Se.Is BeamM.id (Se.Eq mandateId)]

updateStatus :: (L.MonadFlow m, Log m, MonadTime m) => Id Domain.Mandate -> MandateStatus -> m ()
updateStatus (Id mandateId) status = do
  now <- getCurrentTime
  updateOneWithKV
    [Se.Set BeamM.status status, Se.Set BeamM.updatedAt now]
    [Se.Is BeamM.id (Se.Eq mandateId)]

instance FromTType' BeamM.Mandate Domain.Mandate where
  fromTType' BeamM.MandateT {..} = do
    pure $
      Just
        Mandate
          { id = Id id,
            status = status,
            startDate = startDate,
            endDate = endDate,
            maxAmount = maxAmount,
            createdAt = createdAt,
            updatedAt = updatedAt,
            payerVpa = payerVpa,
            payerApp = payerApp,
            payerAppName = payerAppName
          }

instance ToTType' BeamM.Mandate Domain.Mandate where
  toTType' Mandate {..} = do
    BeamM.MandateT
      { BeamM.id = getId id,
        BeamM.status = status,
        BeamM.startDate = startDate,
        BeamM.endDate = endDate,
        BeamM.maxAmount = maxAmount,
        BeamM.createdAt = createdAt,
        BeamM.updatedAt = updatedAt,
        BeamM.payerVpa = payerVpa,
        BeamM.payerApp = payerApp,
        BeamM.payerAppName = payerAppName
      }
