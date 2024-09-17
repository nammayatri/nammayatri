{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MultiModalFareLegRulesExtra where

import qualified API.Types.UI.MultiModalFareComputation as API
import qualified Domain.Types.MultiModalFareLegRules as Domain
import qualified Domain.Types.MultiModalNetwork as MMN
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Common as Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime, throwError)
import qualified Sequelize as Se
import qualified Storage.Beam.MultiModalFareLegRules as Beam
import Storage.Queries.OrphanInstances.MultiModalFareLegRules
import Tools.Error

-- Extra code goes here --

dummyInput :: API.GetFareReq
dummyInput =
  API.GetFareReq
    { networkId = "yellow00-0000-0000-0000-000000000000",
      distance = 1000,
      -- startTime = "3:00:00",
      -- endTime = "4:00:00",
      media = Domain.Cash,
      passengerType = Domain.Adult
    }

dummyInput2 :: API.GetFareReq
dummyInput2 =
  API.GetFareReq
    { networkId = "yellow00-0000-0000-0000-000000000000",
      distance = 4000,
      -- startTime = "3:00:00",
      -- endTime = "4:00:00",
      media = Domain.TransitCard,
      passengerType = Domain.Adult
    }

getFareRule :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => API.GetFareReq -> m (Maybe Domain.MultiModalFareLegRules)
getFareRule getFareReq = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.paymentMedia $ Se.Eq $ getFareReq.media,
          Se.Is Beam.networkId $ Se.Eq $ getId getFareReq.networkId,
          Se.Is Beam.minDist $ Se.LessThanOrEq $ getFareReq.distance,
          Se.Is Beam.maxDist $ Se.GreaterThanOrEq $ getFareReq.distance,
          Se.Is Beam.passengerType $ Se.Eq $ getFareReq.passengerType
        ]
    ]

getFare :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => API.GetFareReq -> m API.MultiModalFare
getFare getFareReq = do
  fareRule <- getFareRule getFareReq
  case fareRule of
    Nothing -> throwError $ InternalError "No data found"
    Just rule -> return $ API.MultiModalFare {fare = rule.amount, currency = rule.currency}
