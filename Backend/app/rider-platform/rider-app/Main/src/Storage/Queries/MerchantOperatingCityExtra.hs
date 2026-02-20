{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MerchantOperatingCityExtra where

import Control.Lens ((^?), _head)
import Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Beckn.Context
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import Sequelize as Se
import qualified Storage.Beam.MerchantOperatingCity as Beam
import Storage.Queries.OrphanInstances.MerchantOperatingCity

findByCity :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => City -> m (Maybe MerchantOperatingCity)
findByCity city = do findAllWithOptionsKV [Se.And [Se.Is Beam.city $ Se.Eq city]] (Se.Desc Beam.createdAt) (Just 1) (Just 0) <&> (^? _head)
