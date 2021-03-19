module Fixtures.Case (defaultCase) where

import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import EulerHS.Prelude
import qualified Fixtures.Time as Fixtures

defaultCase :: Case.Case
defaultCase =
  Case.Case
    { _id = Id "1",
      _name = Nothing,
      _description = Nothing,
      _shortId = "",
      _industry = Case.MOBILITY,
      _type = Case.RIDEORDER,
      _exchangeType = Case.FULFILLMENT,
      _status = Case.COMPLETED,
      _startTime = Fixtures.defaultTime,
      _endTime = Nothing,
      _validTill = Fixtures.defaultTime,
      _provider = Nothing,
      _providerType = Nothing,
      _requestor = Nothing,
      _requestorType = Nothing,
      _parentCaseId = Nothing,
      _fromLocationId = "",
      _toLocationId = "",
      _udf1 = Nothing,
      _udf2 = Nothing,
      _udf3 = Nothing,
      _udf4 = Nothing,
      _udf5 = Nothing,
      _info = Nothing,
      _createdAt = Fixtures.defaultTime,
      _updatedAt = Fixtures.defaultTime
    }
