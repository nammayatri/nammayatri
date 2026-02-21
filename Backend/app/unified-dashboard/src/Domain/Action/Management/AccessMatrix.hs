{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Management.AccessMatrix
  ( getAccessMatrix,
    getMerchantWithCityList,
  )
where

import qualified API.Types.Management.AccessMatrix
import qualified Domain.Types.AccessMatrix as DMatrix
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Role as DRole
import qualified Environment
import EulerHS.Prelude hiding (join)
import qualified Kernel.Beam.Functions as B
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as City
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import Storage.Beam.BeamFlow
import qualified Storage.Beam.Role as BeamRole
import qualified Storage.Queries.AccessMatrix as QMatrix
import qualified Storage.Queries.MerchantExtra as QMerchantExtra
import qualified Storage.Queries.Role as QRole
import Tools.Auth.Api
import Tools.Error

getAccessMatrix ::
  ( Kernel.Types.Id.ShortId DMerchant.Merchant ->
    City.City ->
    ApiTokenInfo ->
    Kernel.Prelude.Maybe Kernel.Prelude.Integer ->
    Kernel.Prelude.Maybe Kernel.Prelude.Integer ->
    Kernel.Prelude.Maybe (Kernel.Types.Id.Id DRole.Role) ->
    Environment.Flow API.Types.Management.AccessMatrix.AccessMatrixAPIEntity
  )
getAccessMatrix _ _ _ mbLimit mbOffset mbRoleId = do
  case mbRoleId of
    Just roleId -> do
      role <- B.runInReplica $ QRole.findById roleId >>= fromMaybeM (InvalidRequest $ "Role with id " <> show roleId.getId <> " does not exist")
      accessMatrixItems <- B.runInReplica $ QMatrix.findAllByRoleId roleId
      let row = mkAccessMatrixRowAPIEntity accessMatrixItems role
      pure $ API.Types.Management.AccessMatrix.AccessMatrixAPIEntity [row]
    Nothing -> do
      roles <- B.runInReplica $ findAllRolesByLimitOffset mbLimit mbOffset
      accessMatrixItems <- B.runInReplica $ traverse QMatrix.findAllByRoles (map (.id) roles) >>= pure . concat
      let rows = mkAccessMatrixRowAPIEntity accessMatrixItems <$> roles
      pure $ API.Types.Management.AccessMatrix.AccessMatrixAPIEntity rows

findAllRolesByLimitOffset :: (BeamFlow m r) => Kernel.Prelude.Maybe Integer -> Kernel.Prelude.Maybe Integer -> m [DRole.Role]
findAllRolesByLimitOffset mbLimit mbOffset = do
  let limitVal = fromIntegral $ fromMaybe 10 mbLimit
      offsetVal = fromIntegral $ fromMaybe 0 mbOffset
  B.findAllWithOptionsKV [Se.Is BeamRole.id $ Se.Not $ Se.Eq ""] (Se.Asc BeamRole.name) (Just limitVal) (Just offsetVal)

getMerchantWithCityList ::
  ( Kernel.Types.Id.ShortId DMerchant.Merchant ->
    City.City ->
    ApiTokenInfo ->
    Environment.Flow [API.Types.Management.AccessMatrix.MerchantCityList]
  )
getMerchantWithCityList _ _ _ = do
  merchantList <- B.runInReplica $ QMerchantExtra.findAllMerchants
  let merchantCityList = map (\merchant -> API.Types.Management.AccessMatrix.MerchantCityList merchant.supportedOperatingCities merchant.shortId) merchantList
  pure merchantCityList

mkAccessMatrixRowAPIEntity :: [DMatrix.AccessMatrix] -> DRole.Role -> API.Types.Management.AccessMatrix.AccessMatrixRowAPIEntity
mkAccessMatrixRowAPIEntity items role = do
  let filteredItems = filter (\item -> item.roleId == role.id) items
  API.Types.Management.AccessMatrix.AccessMatrixRowAPIEntity
    { role = mkRoleAPIEntity role,
      accessMatrixRow = mkAccessMatrixItemAPIEntity <$> filteredItems
    }

mkAccessMatrixItemAPIEntity :: DMatrix.AccessMatrix -> API.Types.Management.AccessMatrix.AccessMatrixItemAPIEntity
mkAccessMatrixItemAPIEntity accessMatrix =
  API.Types.Management.AccessMatrix.AccessMatrixItemAPIEntity
    { serverName = accessMatrix.serverName,
      userActionType = accessMatrix.userActionType,
      additionalUserActions = accessMatrix.additionalUserActions
    }

mkRoleAPIEntity :: DRole.Role -> DRole.RoleAPIEntity
mkRoleAPIEntity role =
  DRole.RoleAPIEntity
    { id = role.id,
      name = role.name,
      description = role.description,
      needsBppAccountCreation = role.needsBppAccountCreation
    }
