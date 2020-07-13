module App.Types
  ( Flow,
    FlowDomainResult,
  )
where

import Beckn.Types.App
import Beckn.Types.Common
import EulerHS.Prelude
import Beckn.Types.Error

type Flow = FlowR ()

type FlowDomainResult a = Flow (Either DomainError a)