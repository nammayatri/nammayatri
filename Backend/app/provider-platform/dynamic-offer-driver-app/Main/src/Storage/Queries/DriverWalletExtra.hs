{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.DriverWalletExtra where

import Data.Time
import Domain.Types.DriverWallet
import Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverWallet as BeamDW
import Storage.Queries.OrphanInstances.DriverWallet ()

-- Extra code goes here --
findAllByDriverIdRangeAndTransactionType :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Driver -> UTCTime -> UTCTime -> Maybe TransactionType -> Maybe Int -> Maybe Int -> m [DriverWallet]
findAllByDriverIdRangeAndTransactionType driverId fromDate toDate transactionType mbLimit mbOffset = do
  findAllWithOptionsKV
    [ Se.And
        ( [ Se.Is BeamDW.driverId $ Se.Eq $ driverId.getId,
            Se.Is BeamDW.createdAt $ Se.GreaterThanOrEq fromDate,
            Se.Is BeamDW.createdAt $ Se.LessThanOrEq toDate
          ]
            <> [Se.Is BeamDW.transactionType $ Se.Eq (fromJust transactionType) | isJust transactionType]
        )
    ]
    (Se.Desc BeamDW.createdAt)
    mbLimit
    mbOffset

findLatestByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id Driver -> m (Maybe DriverWallet)
findLatestByDriverId driverId =
  findAllWithOptionsKV
    [Se.Is BeamDW.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]
    (Se.Desc BeamDW.createdAt)
    (Just 1)
    Nothing
    <&> listToMaybe
