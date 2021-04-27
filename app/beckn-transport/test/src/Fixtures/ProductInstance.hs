module Fixtures.ProductInstance where

import qualified Beckn.Types.Amount as Amount
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import Data.Ratio ((%))
import EulerHS.Prelude
import qualified Fixtures.Time as Fixtures

defaultProductInstance :: ProductInstance.ProductInstance
defaultProductInstance =
  ProductInstance.ProductInstance
    { _id = Id "1",
      _caseId = Id "1",
      _productId = Id "1",
      _personId = Just $ Id "1",
      _personUpdatedAt = Nothing,
      _shortId = "",
      _entityType = ProductInstance.VEHICLE,
      _entityId = Nothing,
      _quantity = 0,
      _price = Just . Amount.Amount $ 100 % 1,
      _type = Case.RIDEORDER,
      _status = ProductInstance.COMPLETED,
      _startTime = Fixtures.defaultTime,
      _endTime = Nothing,
      _validTill = Fixtures.defaultTime,
      _fromLocation = Nothing,
      _toLocation = Nothing,
      _organizationId = "",
      _parentId = Nothing,
      _udf1 = Nothing,
      _udf2 = Nothing,
      _udf3 = Nothing,
      _udf4 = Nothing,
      _udf5 = Nothing,
      _info = Nothing,
      _createdAt = Fixtures.defaultTime,
      _updatedAt = Fixtures.defaultTime
    }
