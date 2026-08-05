{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.PersonTier where

import qualified Domain.Types.Capability as DC
import qualified Domain.Types.Person as DP
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Sequelize as Se
import Storage.Beam.BeamFlow
import qualified Storage.Beam.PersonTier as BeamPT

-- Read-only projection of person.(id, admin_tier); see Storage.Beam.PersonTier.

findByPersonId :: BeamFlow m r => Id DP.Person -> m (Maybe DC.PersonTier)
findByPersonId personId = findOneWithKV [Se.Is BeamPT.id $ Se.Eq $ getId personId]

superAdminExists :: BeamFlow m r => m Bool
superAdminExists = do
  mbSuperAdmin :: Maybe DC.PersonTier <-
    findOneWithKV [Se.Is BeamPT.adminTier $ Se.Eq DC.superAdminTier]
  pure $ isJust mbSuperAdmin

instance FromTType' BeamPT.PersonTier DC.PersonTier where
  fromTType' BeamPT.PersonTierT {..} = do
    return $
      Just
        DC.PersonTier
          { id = Id id,
            ..
          }

instance ToTType' BeamPT.PersonTier DC.PersonTier where
  toTType' DC.PersonTier {..} =
    BeamPT.PersonTierT
      { id = getId id,
        ..
      }
