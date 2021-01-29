{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Product.CustomerSupport where

import App.Types
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App
import Beckn.Types.Storage.Case as C
import Beckn.Types.Storage.Person as SP
import Beckn.Utils.Common
import EulerHS.Prelude
import Storage.Queries.Case as Case
import qualified Storage.Queries.Location as Location
import Storage.Queries.Person as Person
import Types.API.CustomerSupport as T

listOrder :: Text -> FlowHandler [T.Order]
listOrder mobileNumber = withFlowHandler $ do
  person <-
    Person.findByRoleAndMobileNumberWithoutCC SP.ADMIN mobileNumber --TODO: Change ADMIN to USER
      >>= fromMaybeM400 "Invalid MobileNumber"
  let personId = person ^. #_id
  searchcases <-
    Case.findAllByTypeAndStatuses personId C.RIDESEARCH [C.NEW, C.INPROGRESS, C.CONFIRMED, C.COMPLETED, C.CLOSED] Nothing Nothing
      >>= either DB.throwDBError pure
  traverse makeCaseToOrder searchcases

makeCaseToOrder :: C.Case -> Flow T.Order
makeCaseToOrder C.Case {..} = do
  fromLocation <- Location.findLocationById $ LocationId _fromLocationId
  toLocation <- Location.findLocationById $ LocationId _toLocationId
  pure $ T.Order (_getCaseId _id) _status _createdAt _updatedAt _startTime _endTime fromLocation toLocation Nothing
