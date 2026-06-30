module Storage.Queries.Coins.CoinsConfigExtra where

import Domain.Types.Coins.CoinsConfig
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Coins.CoinsConfig as BeamDC
import Storage.Queries.Coins.CoinsConfig ()

findAllByMerchantOptCityIdWithLimitOffset ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Maybe Int ->
  Maybe Int ->
  m [CoinsConfig]
findAllByMerchantOptCityIdWithLimitOffset (Id merchantOptCityId) limit offset =
  findAllWithOptionsKV
    [Se.Is BeamDC.merchantOptCityId $ Se.Eq merchantOptCityId]
    (Se.Asc BeamDC.id)
    limit
    offset
