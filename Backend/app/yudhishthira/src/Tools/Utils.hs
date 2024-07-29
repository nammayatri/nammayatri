module Tools.Utils where

import qualified Environment
import Kernel.Prelude
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Queries.ChakraQueries as SQCQ

mandatoryChakraFields :: [Text]
mandatoryChakraFields = ["userId"]

getChakraQueryFields :: LYT.Chakra -> Environment.Flow [Text]
getChakraQueryFields chakra = do
  queries <- SQCQ.findAllByChakra chakra
  return $ filter (\field -> field `notElem` mandatoryChakraFields) $ concatMap (.queryResults) queries
