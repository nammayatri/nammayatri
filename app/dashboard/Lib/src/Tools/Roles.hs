{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Tools.Roles where

import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common
import Data.Singletons.TH
import Domain.Types.Person as DP
import Tools.Servant.HeaderAuth

data ApiAccessType = READ_ACCESS | WRITE_ACCESS

genSingletons [''ApiAccessType]

data ApiEntity = CUSTOMERS | DRIVERS | RIDES | MONITORING

genSingletons [''ApiEntity]

-- AccessLevel is used only on type level, ApiAccessLevel for working with real data
data ApiAccessLevel = ApiAccessLevel
  { apiAccessType :: ApiAccessType,
    apiEntity :: ApiEntity
  }

data AccessLevel at ae

instance
  forall (at :: ApiAccessType) (ae :: ApiEntity).
  (SingI at, SingI ae) =>
  (VerificationPayload ApiAccessLevel) (AccessLevel at ae)
  where
  toPayloadType _ =
    ApiAccessLevel
      { apiAccessType = fromSing (sing @at),
        apiEntity = fromSing (sing @ae)
      }

verifyAccessLevel :: EsqDBFlow m r => ApiAccessLevel -> Id DP.Person -> m (Id DP.Person)
verifyAccessLevel _requiredAccessLevel _personId = error "TODO"
