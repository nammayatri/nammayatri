{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.IncentiveJourney.Storage.Queries.IncentiveJourneyExtra where

import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.IncentiveJourney.Domain.Types.Common as Common
import qualified Lib.IncentiveJourney.Domain.Types.IncentiveJourney as DIJ
import Lib.IncentiveJourney.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.IncentiveJourney.Storage.Beam.IncentiveJourney as Beam
import Lib.IncentiveJourney.Storage.Queries.OrphanInstances.IncentiveJourney ()
import qualified Sequelize as Se

findEnabledByMerchantIdAndMerchantOperatingCityId ::
  (BeamFlow m r) =>
  Maybe Int ->
  Maybe Int ->
  Id Common.Merchant ->
  Id Common.MerchantOperatingCity ->
  Bool ->
  m [DIJ.IncentiveJourney]
findEnabledByMerchantIdAndMerchantOperatingCityId limit offset merchantId merchantOperatingCityId enabled =
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq (getId merchantId),
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (getId merchantOperatingCityId),
          Se.Is Beam.enabled $ Se.Eq enabled
        ]
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset
