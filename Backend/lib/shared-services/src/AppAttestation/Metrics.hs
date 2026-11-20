{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module AppAttestation.Metrics
  ( recordAttestationResult,
  )
where

import Kernel.Prelude
import qualified Prometheus as P

{-# NOINLINE attestationResultCounter #-}
attestationResultCounter :: P.Vector P.Label6 P.Counter
attestationResultCounter =
  P.unsafeRegister . P.vector ("service", "merchant_operating_city_id", "platform", "endpoint", "outcome", "decision") . P.counter $
    P.Info "app_attestation_result_total" "App attestation checks by service, city, platform, endpoint, outcome and decision"

recordAttestationResult :: MonadIO m => Text -> Text -> Text -> Text -> Text -> Text -> m ()
recordAttestationResult service merchantOperatingCityId platform endpoint outcome decision =
  liftIO $ P.withLabel attestationResultCounter (service, merchantOperatingCityId, platform, endpoint, outcome, decision) P.incCounter
