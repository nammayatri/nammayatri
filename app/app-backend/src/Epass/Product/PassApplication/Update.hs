module Epass.Product.PassApplication.Update where

import qualified Beckn.Types.Storage.Case              as Case
import qualified Beckn.Types.Storage.RegistrationToken as RegistrationToken
import           Data.Aeson
import qualified Data.Text                             as DT
import qualified Epass.Data.Accessor                   as Accessor
import qualified Epass.Storage.Queries.Pass            as Pass
import qualified Epass.Storage.Queries.PassApplication as DB
import           Epass.Types.API.PassApplication
import           Epass.Types.App
import           Epass.Types.Common
import qualified Epass.Types.Common                    as Location (Location (..),
                                                                    LocationType)
import qualified Epass.Types.Storage.Pass              as Pass
import           Epass.Types.Storage.PassApplication
import qualified Epass.Types.Storage.PassApplication   as PassApplication
import           Epass.Utils.Common
import           Epass.Utils.Routes
import           Epass.Utils.Storage
import qualified EulerHS.Language                      as L
import           EulerHS.Prelude
import qualified EulerHS.Types                         as T
import           Servant
import qualified Storage.Queries.Case                  as QC
import qualified Storage.Queries.CaseProduct           as QCP
import qualified Storage.Queries.Products              as QProd
import qualified Test.RandomStrings                    as RS
    -- case' <- QC.findById (SCP._caseId caseProduct)

updatePassApplication ::
  Maybe Text ->
  CaseId ->
  UpdatePassApplicationReq ->
  FlowHandler PassApplicationRes'
updatePassApplication regToken caseId UpdatePassApplicationReq {..} = withFlowHandler $ do
  token <- verifyToken regToken
  allowOnlyUser token
  pA <- QC.findById caseId
  -- verifyIfStatusUpdatable (PassApplication._status pA) _status
  case _status of
    REVOKED -> do
  --     Pass.revokeByPassApplicationId passApplicationId
  --     -- _approvedCount remains unchanged as part of history
      -- TODO: mark passes(product) as revoked
      QC.updateStatus caseId Case.CLOSED
    APPROVED -> do
      -- TODO: create Passes
  --     when
  --       (isNothing _approvedCount)
  --       (L.throwException $ err400 {errBody = "Approved count cannot be empty"})
  --     let count = PassApplication._count pA
  --         approvedCount = validApprovedCount count $ fromJust _approvedCount
  --     -- Create passes
  --     replicateM approvedCount (createPass pA)
  --     DB.update passApplicationId _status (Just approvedCount) _remarks
      QC.updateStatus caseId Case.CONFIRMED
    PENDING -> QC.updateStatus caseId Case.INPROGRESS
    _ -> return ()
  QC.findById caseId
    >>= return . PassApplicationRes'
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
createPass PassApplication {..} = do
  id <- generateGUID
  shortId <- L.runIO $ RS.randomString (RS.onlyAlphaNum RS.randomASCII) 16
  currTime <- getCurrTime
  let pass =
        Pass.Pass
          { _id = id,
            _ShortId = DT.pack shortId,
            _status = Pass.ACTIVE,
            _PassApplicationId = _id,
            _createdAt = currTime,
            _updatedAt = currTime,
            ..
          }
  Pass.create pass

allowOnlyUser :: RegistrationToken.RegistrationToken -> L.Flow ()
allowOnlyUser RegistrationToken.RegistrationToken {..} =
  case _entityType of
    RegistrationToken.USER -> return ()
    RegistrationToken.CUSTOMER ->
      L.throwException $ err400 {errBody = "OPERATION_NOT_ALLOWED"}
