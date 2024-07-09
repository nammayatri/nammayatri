{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Queries.PayoutOrderExtra where

import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Payment.Domain.Types.Common
import Lib.Payment.Domain.Types.PaymentOrder (PaymentOrder)
import Lib.Payment.Domain.Types.PayoutOrder
import Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Beam.PayoutOrder as Beam
import Lib.Payment.Storage.Queries.OrphanInstances.PayoutOrder
import qualified Sequelize as Se

findAllByEntityNameAndEntityIds ::
  (BeamFlow m r) =>
  (Maybe Int -> Maybe Int -> Maybe EntityName -> [Maybe Text] -> m [PayoutOrder])
findAllByEntityNameAndEntityIds limit offset entityName entityIds = do
  let mbIds = Just $ catMaybes entityIds
  findAllWithOptionsKV
    [ Se.And
        [Se.Is Beam.entityName $ Se.Eq entityName, Se.Is Beam.entityIds $ Se.Eq mbIds]
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset
