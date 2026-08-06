{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Beam.Common where

import qualified Database.Beam as B
import Kernel.Prelude (Generic)
import Storage.Beam.AccessMatrix as BeamAM
import Storage.Beam.BeamFlow
import Storage.Beam.Merchant as BeamM
import Storage.Beam.MerchantAccess as BeamMA
import Storage.Beam.Person as BeamP
import Storage.Beam.RegistrationToken as BeamRT
import Storage.Beam.Role as BeamR
import Storage.Beam.Transaction as BeamT

atlasDB :: BeamFlow' => B.DatabaseSettings be AtlasDB
atlasDB =
  B.defaultDbSettings
    `B.withDbModification` B.dbModification
      { accessMatrix = accessMatrixTable,
        merchant = merchantTable,
        merchantAccess = merchantAccessTable,
        person = personTable,
        registrationToken = registrationTokenTable,
        role = roleTable,
        transaction = transactionTable
      }

data AtlasDB f = AtlasDB
  { accessMatrix :: f (B.TableEntity BeamAM.AccessMatrixT),
    merchant :: f (B.TableEntity BeamM.MerchantT),
    merchantAccess :: f (B.TableEntity BeamMA.MerchantAccessT),
    person :: f (B.TableEntity BeamP.PersonT),
    registrationToken :: f (B.TableEntity BeamRT.RegistrationTokenT),
    role :: f (B.TableEntity BeamR.RoleT),
    transaction :: f (B.TableEntity BeamT.TransactionT)
  }
  deriving (Generic, B.Database be)
