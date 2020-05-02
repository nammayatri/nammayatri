module Beckn.Product.PassApplication.Update where

import qualified Beckn.Data.Accessor                   as Accessor
import qualified Beckn.Storage.Queries.Pass            as Pass
import qualified Beckn.Storage.Queries.PassApplication as DB
import           Beckn.Types.API.PassApplication
import           Beckn.Types.App
import           Beckn.Types.Common
import qualified Beckn.Types.Common                    as Location (Location (..),
                                                                    LocationType)
import qualified Beckn.Types.Storage.Pass              as Pass
import           Beckn.Types.Storage.PassApplication
import qualified Beckn.Types.Storage.PassApplication   as PassApplication
import qualified Beckn.Types.Storage.RegistrationToken as RegistrationToken
import           Beckn.Utils.Common
import           Beckn.Utils.Routes
import           Beckn.Utils.Storage
import           Data.Aeson
import qualified EulerHS.Language                      as L
import           EulerHS.Prelude
import qualified EulerHS.Types                         as T
import           Servant
import qualified Test.RandomStrings                    as RS
import qualified Data.Text                             as DT

updatePassApplication ::
  Maybe Text ->
  PassApplicationId ->
  UpdatePassApplicationReq ->
  FlowHandler PassApplicationRes
updatePassApplication regToken passApplicationId UpdatePassApplicationReq{..} = withFlowHandler $ do
  token <- verifyToken regToken
  allowOnlyUser token

  pA <- fromMaybeM400 "Pass Application not found" =<< DB.findById passApplicationId
  verifyIfStatusUpdatable (PassApplication._status pA) _status

  case _status of
    REVOKED -> do
      Pass.revokeByPassApplicationId passApplicationId
      -- _approvedCount remains unchanged as part of history
      DB.update passApplicationId _status Nothing _remarks

    APPROVED -> do
      when (isNothing _approvedCount)
        (L.throwException $ err400 {errBody = "Approved count cannot be empty"})
      let
        count = PassApplication._count pA
        approvedCount = validApprovedCount count $ fromJust _approvedCount
      -- Create passes
      replicateM approvedCount (createPass pA)
      DB.update passApplicationId _status (Just approvedCount) _remarks

    _ -> DB.update passApplicationId _status Nothing _remarks

  DB.findById passApplicationId
    >>= fromMaybeM400 "Pass Application not found"
    >>= return . PassApplicationRes
  where
    validApprovedCount count approvedCount =
      if approvedCount > count then count else approvedCount

verifyIfStatusUpdatable :: Status -> Status -> L.Flow ()
verifyIfStatusUpdatable currStatus newStatus =
  case (currStatus, newStatus) of
    (PENDING, APPROVED) -> return ()
    (PENDING, REJECTED) -> return ()
    (PENDING, EXPIRED) -> return ()
    -- Blocked APPROVED going to REJECTED
    (APPROVED, EXPIRED) -> return ()
    (APPROVED, REVOKED) -> return ()
    _ -> L.throwException $ err400 {errBody = "Invalid status update"}

createPass :: PassApplication -> L.Flow ()
createPass PassApplication{..} = do
  id <- generateGUID
  shortId <- L.runIO $ RS.randomString (RS.onlyAlphaNum RS.randomASCII) 16
  currTime <- getCurrTime
  let pass = Pass.Pass
              { _id = id
              , _ShortId = DT.pack shortId
              , _status = Pass.ACTIVE
              , _PassApplicationId = _id
              , _createdAt = currTime
              , _updatedAt = currTime
              , ..
              }
  Pass.create pass

allowOnlyUser :: RegistrationToken.RegistrationToken -> L.Flow ()
allowOnlyUser RegistrationToken.RegistrationToken{..} =
  case _entityType of
    RegistrationToken.USER -> return ()
    RegistrationToken.CUSTOMER ->
      L.throwException $ err400 {errBody = "OPERATION_NOT_ALLOWED"}
