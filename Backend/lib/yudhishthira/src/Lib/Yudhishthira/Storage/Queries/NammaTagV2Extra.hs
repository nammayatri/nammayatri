{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Yudhishthira.Storage.Queries.NammaTagV2Extra where

import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Lib.Yudhishthira.Storage.Beam.BeamFlow
import qualified Lib.Yudhishthira.Storage.Beam.NammaTagV2 as Beam
import Lib.Yudhishthira.Storage.Queries.OrphanInstances.NammaTagV2 ()
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.NammaTagV2
import Sequelize as Se

findAllByChakra ::
  BeamFlow m r =>
  Id Lib.Yudhishthira.Types.MerchantOperatingCity ->
  Lib.Yudhishthira.Types.Chakra ->
  m [Lib.Yudhishthira.Types.NammaTagV2.NammaTagV2]
findAllByChakra merchantOperatingCityId chakra =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (getId merchantOperatingCityId),
          Se.Is Beam.chakra $ Se.Eq (Just chakra)
        ]
    ]

-- | Fetch all tags for a city whose names are in the given list. Single DB round-trip.
findAllByMerchantOperatingCityIdAndNames ::
  BeamFlow m r =>
  Id Lib.Yudhishthira.Types.MerchantOperatingCity ->
  [Text] ->
  m [Lib.Yudhishthira.Types.NammaTagV2.NammaTagV2]
findAllByMerchantOperatingCityIdAndNames _ [] = pure []
findAllByMerchantOperatingCityIdAndNames merchantOperatingCityId tagNames =
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (getId merchantOperatingCityId),
          Se.Is Beam.name $ Se.In tagNames
        ]
    ]
