{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.Student
  ( getGetOrganizations,
    postStudentUpdate,
    getStudentData,
    getStudentVerificationStatus,
    postStudentActivate,
  )
where

import qualified API.Types.UI.Student
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Domain.Types.Pass
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

getGetOrganizations ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.Flow [API.Types.UI.Student.GetOrganizationResp]
  )
getGetOrganizations = do error "Logic yet to be decided"

postStudentUpdate ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.Student.StudentUpdateReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postStudentUpdate = do error "Logic yet to be decided"

getStudentData :: ((Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.Flow API.Types.UI.Student.StudentDataResp)
getStudentData = do error "Logic yet to be decided"

getStudentVerificationStatus ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.Flow API.Types.UI.Student.StudentStatusResp
  )
getStudentVerificationStatus = do error "Logic yet to be decided"

postStudentActivate ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Pass.Pass ->
    API.Types.UI.Student.StudentActivateReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postStudentActivate = do error "Logic yet to be decided"
