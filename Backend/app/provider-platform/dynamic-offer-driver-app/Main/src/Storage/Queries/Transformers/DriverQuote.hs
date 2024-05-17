{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Transformers.DriverQuote where

import qualified Domain.Types.FareParameters
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import Storage.Queries.FareParameters as BeamQFP

getFareParams :: (MonadFlow m, MonadFlow m, EsqDBFlow m r, CacheFlow m r) => (Kernel.Prelude.Text -> m (Domain.Types.FareParameters.FareParameters))
getFareParams fareParametersId = do
  fp <- BeamQFP.findById (Id fareParametersId) >>= fromMaybeM (InternalError $ "FareParameters not found in DriverQuote for id: " <> show fareParametersId)
  pure fp
