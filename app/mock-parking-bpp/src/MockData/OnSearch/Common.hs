module MockData.OnSearch.Common where

import Core.OnSearch.Category
import Relude hiding (id, state)

cat4wheeler :: Text
cat4wheeler = "4-wheeler-parking"

mockProviderCategories :: Category
mockProviderCategories =
  let id_ = Just cat4wheeler
      descriptor_ = Just $ CategoryDescriptor {name = "4 wheeler parking"}
   in Category
        { id = id_,
          descriptor = descriptor_
        }
