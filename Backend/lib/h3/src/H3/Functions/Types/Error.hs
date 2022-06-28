{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module H3.Functions.Types.Error (H3Error (..), throwOnError) where

import Control.Exception
import Control.Monad.Catch
import Data.Text
import H3.Functions.Types.Internal

newtype H3Error = H3Error Text
  deriving stock (Show)
  deriving anyclass (Exception)

throwOnError :: MonadThrow m => H3ResultCode -> m ()
throwOnError errCode = do
  case checkForError errCode of
    Left a -> throwM a
    Right _ -> return ()

checkForError :: H3ResultCode -> Either H3Error ()
checkForError n = case n of
  ESuccess -> Right ()
  EFailed -> Left . H3Error $ "The operation failed but a more specific error is not available"
  EDomain -> Left . H3Error $ "Argument was outside of acceptable range (when a more specific error code is not available)"
  ELatlngDomain -> Left . H3Error $ "Latitude or longitude arguments were outside of acceptable range"
  EResDomain -> Left . H3Error $ "Resolution argument was outside of acceptable range"
  ECellInvalid -> Left . H3Error $ "`H3Cell` cell argument was not valid"
  EDirEdgeInvalid -> Left . H3Error $ "`H3Cell` directed edge argument was not valid"
  EUndirEdgeInvalid -> Left . H3Error $ "`H3Cell` undirected edge argument was not valid"
  EVertexInvalid -> Left . H3Error $ "`H3Cell` vertex argument was not valid"
  EPentagon -> Left . H3Error $ "Pentagon distortion was encountered which the algorithm could not handle it"
  EDuplicateInput -> Left . H3Error $ "Duplicate input was encountered in the arguments and the algorithm could not handle it"
  ENotNeighbors -> Left . H3Error $ "`H3Cell` cell arguments were not neighbors"
  EResMismatch -> Left . H3Error $ "`H3Cell` cell arguments had incompatible resolutions"
  EMemory -> Left . H3Error $ "Necessary memory allocation failed"
  EMemoryBounds -> Left . H3Error $ "Bounds of provided memory were not large enough"
  EOptionInvalid -> Left . H3Error $ "Mode or flags argument was not valid"
