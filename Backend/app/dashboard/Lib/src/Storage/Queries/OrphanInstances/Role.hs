{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.OrphanInstances.Role where

import Domain.Types.Role as Role
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import qualified Storage.Beam.Role as BeamR

instance FromTType' BeamR.Role Role.Role where
  fromTType' BeamR.RoleT {..} =
    pure $
      Just
        Role.Role
          { id = Id id,
            parentRoleId = Id <$> parentRoleId,
            ..
          }

instance ToTType' BeamR.Role Role.Role where
  toTType' Role.Role {..} =
    BeamR.RoleT
      { id = getId id,
        parentRoleId = getId <$> parentRoleId,
        ..
      }

