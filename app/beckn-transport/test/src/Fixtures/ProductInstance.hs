module Fixtures.ProductInstance where

import qualified Beckn.Types.Amount as Amount
import Beckn.Types.App (ProductsId (..))
import Beckn.Types.ID
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import Data.Ratio ((%))
import EulerHS.Prelude
import qualified Fixtures.Time as Fixtures

defaultProductInstance :: ProductInstance.ProductInstance
defaultProductInstance =
  ProductInstance.ProductInstance
    { _id = ID "1",
      _caseId = ID "1",
      _productId = ProductsId "1",
      _personId = Just $ ID "1",
      _personUpdatedAt = Nothing,
      _shortId = "",
      _entityType = ProductInstance.VEHICLE,
      _entityId = Nothing,
      _quantity = 0,
      _price = Amount.Amount $ 100 % 1,
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
