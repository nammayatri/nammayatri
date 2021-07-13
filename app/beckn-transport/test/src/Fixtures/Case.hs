module Fixtures.Case (defaultCase) where

import Beckn.Types.Id
import EulerHS.Prelude
import qualified Fixtures.Time as Fixtures
import qualified Types.Storage.Case as Case

defaultCase :: Case.Case
defaultCase =
  Case.Case
    { id = Id "1",
      name = Nothing,
      description = Nothing,
      shortId = "",
      industry = Case.MOBILITY,
      _type = Case.RIDESEARCH,
      exchangeType = Case.FULFILLMENT,
      status = Case.COMPLETED,
      startTime = Fixtures.defaultTime,
      endTime = Nothing,
      validTill = Fixtures.defaultTime,
      provider = Nothing,
      providerType = Nothing,
      requestor = Nothing,
      requestorType = Nothing,
      fromLocationId = "",
      toLocationId = "",
      udf1 = Nothing,
      udf2 = Nothing,
      udf3 = Nothing,
      udf4 = Nothing,
      udf5 = Nothing,
      info = Nothing,
      createdAt = Fixtures.defaultTime,
      updatedAt = Fixtures.defaultTime
    }
