module Util.Persistent
    ( RawQuery(..)
    , GetEntitiesWithCountQuery(..)
    , EntitiesWithCount(..)
    , getEntitiesWithCount
    , LimitOffset(..)
    ) where

import Import
import qualified Database.Persist.Sql as Sql
import Data.Function ((&))


data RawQuery = RawQuery
    { query :: Text
    , queryValues :: [PersistValue]
    }


data GetEntitiesWithCountQuery = GetEntitiesWithCountQuery
    { getEntities :: RawQuery
    , countEntities :: RawQuery
    }


data EntitiesWithCount a = EntitiesWithCount
    { entities :: [a]
    , entitiesCount :: Int64
    }


getEntitiesWithCount :: Sql.RawSql a => GetEntitiesWithCountQuery -> Handler (EntitiesWithCount a)
getEntitiesWithCount GetEntitiesWithCountQuery{..} = do
    (entities, entitiesCount) <- runDB $ do
        entities <- Sql.rawSql (query getEntities) (queryValues getEntities)
        entityCount <- Sql.rawSql (query countEntities) (queryValues countEntities)
        pure (entities, entityCount)
    pure $ EntitiesWithCount
        { entities = entities
        , entitiesCount =
            entitiesCount
                & listToMaybe
                & fmap Sql.unSingle
                & fromMaybe 0
        }


data LimitOffset = LimitOffset
    { limit :: Int
    , offset :: Int
    }
