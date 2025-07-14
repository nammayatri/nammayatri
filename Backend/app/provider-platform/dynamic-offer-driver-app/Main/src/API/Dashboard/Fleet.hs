{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module API.Dashboard.Fleet where

import qualified API.Dashboard.Fleet.BulkAssociation as BulkAssociation
import qualified API.Dashboard.Fleet.Registration as Registration
import Control.Exception (Exception)
import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import qualified Domain.Types.FleetMemberAssociation as DFMA
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Environment
import GHC.Generics (Generic)
import Kernel.Prelude (Bool, Int, Maybe (..), Show (..), Text, pure, void, whenJust, ($), (<>), (>>=))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error.BaseError.HTTPError (HttpCode (..), IsAPIError, IsBaseError (..), IsBecknAPIError, IsHTTPError (..))
import Kernel.Types.Id (Id (..), ShortId)
import qualified Kernel.Utils.Common as Utils
import Servant hiding (throwError)
import qualified Storage.Queries.FleetMemberAssociation as QFMA
import qualified Storage.Queries.Person as QP
import Tools.Auth

type API =
  DashboardTokenAuth
    :> ( Registration.API
           :<|> "fleet" :> BulkAssociation.API
           :<|> "admin" :> "member-association"
             :> ReqBody '[JSON] FleetMemberAssociationReq
             :> Post '[JSON] FleetMemberAssociationRes
       )

data FleetMemberAssociationReq = FleetMemberAssociationReq
  { fleetMemberId :: Text,
    fleetOwnerId :: Text,
    enabled :: Bool,
    groupCode :: Maybe Text,
    isFleetOwner :: Bool,
    level :: Maybe Int,
    order :: Maybe Int,
    parentGroupCode :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data FleetMemberAssociationRes = FleetMemberAssociationRes
  { result :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data FleetMemberAssociationError
  = MemberNotFound Text
  | FleetOwnerNotFound Text
  | AssociationAlreadyExists
  deriving (Generic, Show, IsBecknAPIError, Exception)

instance IsBaseError FleetMemberAssociationError where
  toMessage = \case
    MemberNotFound memberId -> Just $ "Fleet member with id " <> Utils.show memberId <> " not found"
    FleetOwnerNotFound ownerId -> Just $ "Fleet owner with id " <> Utils.show ownerId <> " not found"
    AssociationAlreadyExists -> Just "Fleet member association already exists"

instance IsHTTPError FleetMemberAssociationError where
  toErrorCode = \case
    MemberNotFound _ -> "MEMBER_NOT_FOUND"
    FleetOwnerNotFound _ -> "FLEET_OWNER_NOT_FOUND"
    AssociationAlreadyExists -> "ASSOCIATION_EXISTS"
  toHttpCode = \case
    MemberNotFound _ -> E404
    FleetOwnerNotFound _ -> E404
    AssociationAlreadyExists -> E409

instance IsAPIError FleetMemberAssociationError

handler :: ShortId DM.Merchant -> Context.City -> FlowServer API
handler merchantId city _ =
  Registration.handler merchantId city
    :<|> BulkAssociation.handler merchantId city
    :<|> createFleetMemberAssociation merchantId

createFleetMemberAssociation :: ShortId DM.Merchant -> FleetMemberAssociationReq -> FlowHandler FleetMemberAssociationRes
createFleetMemberAssociation _ req = Utils.withFlowHandlerAPI $ do
  let memberId = Id req.fleetMemberId :: Id DP.Person
  memberRes <- QP.findById memberId
  void $ pure memberRes >>= Utils.fromMaybeM (MemberNotFound req.fleetMemberId)

  let ownerId = Id req.fleetOwnerId :: Id DP.Person
  ownerRes <- QP.findById ownerId
  void $ pure ownerRes >>= Utils.fromMaybeM (FleetOwnerNotFound req.fleetOwnerId)

  mExistingAssoc <- QFMA.findByPrimaryKey req.fleetMemberId req.fleetOwnerId
  whenJust mExistingAssoc $ \_ -> do
    Utils.throwError AssociationAlreadyExists

  now <- Utils.getCurrentTime
  let association =
        DFMA.FleetMemberAssociation
          { fleetMemberId = req.fleetMemberId,
            fleetOwnerId = req.fleetOwnerId,
            level = req.level,
            parentGroupCode = req.parentGroupCode,
            groupCode = req.groupCode,
            order = req.order,
            isFleetOwner = req.isFleetOwner,
            enabled = req.enabled,
            createdAt = now,
            updatedAt = now
          }
  void $ QFMA.create association
  pure $ FleetMemberAssociationRes "Fleet member association created successfully"
