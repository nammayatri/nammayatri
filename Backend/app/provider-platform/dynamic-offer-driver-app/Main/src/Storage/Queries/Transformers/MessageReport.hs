module Storage.Queries.Transformers.MessageReport where

import qualified Data.Aeson
import qualified Data.Aeson as A
import qualified Data.Map as Map
import qualified Domain.Types.Extra.MessageReport

getMessageDynamicFields :: (Data.Aeson.Value -> Domain.Types.Extra.MessageReport.MessageDynamicFieldsType)
getMessageDynamicFields messageDynamicFields = case A.fromJSON messageDynamicFields of
  A.Success val -> val
  _ -> Map.empty
