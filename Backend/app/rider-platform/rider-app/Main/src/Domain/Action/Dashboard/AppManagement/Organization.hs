{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.AppManagement.Organization
  ( getOrganizationGetOrganizationId,
    getOrganizationStudentOrganization,
    postOrganizationStudentVerify,
    postOrganizationOrganizationUpdate,
  )
where

import qualified API.Types.Dashboard.AppManagement.Organization
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Organization
import qualified "this" Domain.Types.Person
import qualified "this" Domain.Types.Student
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant
import Tools.Auth

getOrganizationGetOrganizationId :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.Flow (Kernel.Types.Id.Id Domain.Types.Organization.Organization))
getOrganizationGetOrganizationId _merchantShortId _opCity personId = do error "Logic yet to be decided" personId

getOrganizationStudentOrganization :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Organization.Organization -> Kernel.Prelude.Maybe (Domain.Types.Student.VerificationStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.Flow API.Types.Dashboard.AppManagement.Organization.StudentsListResp)
getOrganizationStudentOrganization _merchantShortId _opCity organizationId status limit offset = do error "Logic yet to be decided" organizationId status limit offset

postOrganizationStudentVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.Organization.VerifyStudentsReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postOrganizationStudentVerify _merchantShortId _opCity req = do error "Logic yet to be decided" req

postOrganizationOrganizationUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.Dashboard.AppManagement.Organization.OrganizationUpdateReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postOrganizationOrganizationUpdate _merchantShortId _opCity personId req = do error "Logic yet to be decided" personId req
