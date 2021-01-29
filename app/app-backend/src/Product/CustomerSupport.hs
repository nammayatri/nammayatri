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

listOrder :: Text -> FlowHandler [T.OrderResp]
listOrder mobileNumber = withFlowHandler $ do
  person <-
    Person.findByRoleAndMobileNumberWithoutCC SP.ADMIN mobileNumber --TODO: Change ADMIN to USER
      >>= fromMaybeM400 "Invalid MobileNumber"
  let personId = person ^. #_id
  searchcases <-
    Case.findAllByTypeAndStatuses personId C.RIDESEARCH [C.NEW, C.INPROGRESS, C.CONFIRMED, C.COMPLETED, C.CLOSED] Nothing Nothing
      >>= either DB.throwDBError pure
  -- let serachIds = map (\x-> x ^. #_id) searchcases
  -- Want to use this and create a Hashmap so we can modify in O(n)
  -- confiremedOrders <- Case.findAllByParentIdsAndCaseType serachIds C.RIDEORDER
  -- mapOrder <- foldl createhashMap  empty confiremedOrders empty
  -- L.logInfo @Text "confiremedOrder" (show confiremedOrders)
  traverse makeCaseToOrder searchcases

makeCaseToOrder :: C.Case -> Flow T.OrderResp
makeCaseToOrder C.Case {..} = do
  (confiremedOrder :: Maybe C.Case) <-
    Case.findOneByParentIdAndCaseType _id C.RIDEORDER
      >>= either DB.throwDBError pure
  let (status :: Maybe CaseStatus) = maybe Nothing (\x -> Just $ x ^. #_status) confiremedOrder <|> (Just _status)
  fromLocation <- Location.findLocationById $ LocationId _fromLocationId
  toLocation <- Location.findLocationById $ LocationId _toLocationId
  --  Info: udf1 is vechicle variant
  -- TODO: Use Record Syntax here
  let details = T.OrderDetails (_getCaseId _id) status _createdAt _updatedAt _startTime _endTime fromLocation toLocation _udf1 Nothing
  pure $ T.OrderResp {_order = details}

-- makeTripDetails caseId = do
--     getProduct

-- { _id :: Text, -- Product Instance ID
--   _status :: SP.ProductInstanceStatus,
--   _driver ::  Driver, -- _info -> driver
--   _vehicle :: Vehicle,
--   _provider :: Provider,
--   _price :: Text
-- }

-- createhashMap :: C.Case ->  HashMap Text C.Case ->  HashMap Text C.Case
-- createhashMap cases = do
