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
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import Storage.Queries.FareParameters as BeamQFP

getFareParams :: KvDbFlow m r => (Kernel.Prelude.Text -> m Domain.Types.FareParameters.FareParameters)
getFareParams fareParametersId = BeamQFP.findById (Id fareParametersId) >>= fromMaybeM (InternalError $ "FareParameters not found in DriverQuote for id: " <> show fareParametersId)
