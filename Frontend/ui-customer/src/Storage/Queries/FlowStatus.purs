module Storage.Queries.FlowStatus where

import Prelude

findFlowStatus :: String
findFlowStatus = "SELECT * FROM FlowStatus"

deleteFlowStatus :: String
deleteFlowStatus = "DELETE FROM FlowStatus"