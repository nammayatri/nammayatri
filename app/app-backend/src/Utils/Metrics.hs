{-# LANGUAGE OverloadedLabels #-}

module Utils.Metrics where

import App.Types
import qualified Beckn.Types.Storage.Case as Case
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Prometheus as P

caseCounter :: P.Vector P.Label2 P.Counter
caseCounter = P.unsafeRegister $ P.vector ("status", "type") $ P.counter $ P.Info "case_count" ""

incrementCaseCount :: Case.CaseStatus -> Case.CaseType -> Flow ()
incrementCaseCount caseStatus caseType =
  L.runIO $ P.withLabel caseCounter (show caseStatus, show caseType) P.incCounter
