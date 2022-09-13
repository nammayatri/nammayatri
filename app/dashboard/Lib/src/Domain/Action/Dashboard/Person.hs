module Domain.Action.Dashboard.Person where

import Beckn.External.Encryption (decrypt)
import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Types.Id
import Domain.Types.Person as DP
import qualified Storage.Queries.Person as QP

newtype ListPersonRes = ListPersonRes
  {list :: [PersonAPIEntity]}
  deriving (Generic, ToJSON, FromJSON, ToSchema)

listPerson ::
  (EsqDBFlow m r, EncFlow m r) =>
  Id DP.Person ->
  Maybe Text ->
  Maybe Integer ->
  Maybe Integer ->
  m ListPersonRes
listPerson _ mbSearchString mbLimit mbOffset = do
  personList <- QP.findAllWithLimitOffset mbSearchString mbLimit mbOffset
  decPersonList <- decrypt `mapM` personList
  let respPersonList = DP.makePersonAPIEntity <$> decPersonList
  return $ ListPersonRes respPersonList
