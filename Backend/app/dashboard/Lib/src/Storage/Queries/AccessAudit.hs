{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.AccessAudit where

import qualified Domain.Types.Capability as DC
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Sequelize as Se
import qualified Storage.Beam.AccessAudit as BeamAA
import Storage.Beam.BeamFlow

create :: BeamFlow m r => DC.AccessAudit -> m ()
create = createWithKV

findAllByTarget :: BeamFlow m r => Text -> Text -> m [DC.AccessAudit]
findAllByTarget targetType targetId =
  findAllWithKV
    [ Se.And
        [ Se.Is BeamAA.targetType $ Se.Eq targetType,
          Se.Is BeamAA.targetId $ Se.Eq (Just targetId)
        ]
    ]

instance FromTType' BeamAA.AccessAudit DC.AccessAudit where
  fromTType' BeamAA.AccessAuditT {..} = do
    return $
      Just
        DC.AccessAudit
          { id = Id id,
            actorId = Id <$> actorId,
            ..
          }

instance ToTType' BeamAA.AccessAudit DC.AccessAudit where
  toTType' DC.AccessAudit {..} =
    BeamAA.AccessAuditT
      { id = getId id,
        actorId = getId <$> actorId,
        ..
      }
