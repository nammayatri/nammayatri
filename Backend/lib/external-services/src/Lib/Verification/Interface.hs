{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Verification.Interface
  ( module Reexport,
    verifyDLAsync,
    verifyRCAsync,
    validateImage,
    extractRCImage,
    extractDLImage,
  )
where

import Kernel.Tools.Metrics.CoreMetrics.Types
-- import Kernel.Types.Common
import Lib.Encryption
import Lib.Verification.Idfy.Config as Reexport
import qualified Lib.Verification.Interface.Idfy as Idfy
import Lib.Verification.Interface.Types as Reexport
import Lib.Verification.Types as Reexport

verifyDLAsync ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  VerificationServiceConfig ->
  VerifyDLAsyncReq ->
  m VerifyDLAsyncResp
verifyDLAsync serviceConfig req = case serviceConfig of
  IdfyConfig cfg -> Idfy.verifyDLAsync cfg req

verifyRCAsync ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  VerificationServiceConfig ->
  VerifyRCAsyncReq ->
  m VerifyRCAsyncResp
verifyRCAsync serviceConfig req = case serviceConfig of
  IdfyConfig cfg -> Idfy.verifyRCAsync cfg req

validateImage ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  VerificationServiceConfig ->
  ValidateImageReq ->
  m ValidateImageResp
validateImage serviceConfig req = case serviceConfig of
  IdfyConfig cfg -> Idfy.validateImage cfg req

extractRCImage ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  VerificationServiceConfig ->
  ExtractRCImageReq ->
  m ExtractRCImageResp
extractRCImage serviceConfig req = case serviceConfig of
  IdfyConfig cfg -> Idfy.extractRCImage cfg req

extractDLImage ::
  ( EncFlow m r,
    CoreMetrics m
  ) =>
  VerificationServiceConfig ->
  ExtractDLImageReq ->
  m ExtractDLImageResp
extractDLImage serviceConfig req = case serviceConfig of
  IdfyConfig cfg -> Idfy.extractDLImage cfg req
