module Storage.Queries.Products where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Products
import Storage.Tabular.Products

create :: Products -> SqlDB ()
create = create'

findAllById :: Transactionable m => [Id Products] -> m [Products]
findAllById ids =
  Esq.findAll $ do
    products <- from $ table @ProductsT
    where_ $
      products ^. ProductsTId `in_` valList (toKey <$> ids)
    return products

findById :: Transactionable m => Id Products -> m (Maybe Products)
findById = Esq.findById

findByName :: Transactionable m => Text -> m (Maybe Products)
findByName name =
  Esq.findOne $ do
    products <- from $ table @ProductsT
    where_ $
      products ^. ProductsName ==. val name
    return products
