module CAC where

import Prelude (show, (<>))
import Data.Maybe (Maybe(..), fromMaybe)
import CAC.Types (City(..), Merchant(..), Context)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)

foreign import getCurrentContext :: String -> Context

foreign import getExistingCAC :: forall a. Context -> a

foreign import isContextChanged :: Context -> Boolean

foreign import storeConfigWithContextInCache :: forall a. Fn2 a Context a

foreign import getCACFromCacheForContext :: forall a. Fn3 Context (Maybe a) (a -> (Maybe a)) (Maybe a)

getFromCAC :: forall a. Maybe City -> Maybe Merchant -> (City -> Merchant -> a) -> a
getFromCAC mbCity mbMerchant getCacConfig = do
  let
    city = fromMaybe AnyCity mbCity
    merchant = fromMaybe AnyMerchant mbMerchant
    context = getContextKey city merchant
    shouldRenewConfig = isContextChanged context
  if shouldRenewConfig then
    fetchFromCAC city merchant context getCacConfig
  else
    getExistingCAC context

fetchFromCAC :: forall a. City -> Merchant -> Context -> (City -> Merchant -> a) -> a
fetchFromCAC city merchant context getCacConfig = case runFn3 getCACFromCacheForContext context Nothing Just of
  Nothing ->
    let
      config = getCacConfig city merchant
    in
      runFn2 storeConfigWithContextInCache config context
  Just config -> config

getContextKey :: City -> Merchant -> Context
getContextKey city merchant = show city <> "_" <> show merchant
