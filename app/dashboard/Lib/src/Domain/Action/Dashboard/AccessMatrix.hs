module Domain.Action.Dashboard.AccessMatrix where

import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Domain.Types.AccessMatrix as DMatrix
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Role as DRole
import qualified Storage.Queries.AccessMatrix as QMatrix
import qualified Storage.Queries.Role as QRole

-- TODO limit offset
getAccessMatrix ::
  EsqDBFlow m r =>
  Id DP.Person ->
  Maybe (Id DRole.Role) ->
  m DMatrix.AccessMatrixAPIEntity
getAccessMatrix _ mbRole = do
  case mbRole of
    Just _role -> error "TODO"
    Nothing -> do
      roles <- QRole.findAll
      DMatrix.mkAccessMatrixAPIEntity roles <$> QMatrix.findAll
