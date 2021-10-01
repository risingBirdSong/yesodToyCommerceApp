{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

-- module DB.ExampleEsqueleto where 
-- import MyEsquleto


-- aaa :: DB [Entity Person]
-- aaa = select $ do
--     people <- from $ Table @Person
--     where_ (people ^. PersonName ==. val "John")
--     pure people

-- bbb = select $ do
--     (people :& blogPosts) <-
--         from $ Table @Person
--         `leftJoin` Table @BlogPost
--         `on` (\(people :& blogPosts) ->
--                 people ^. PersonId ==. blogPosts ?. BlogPostAuthorId)
--     where_ (people ^. PersonAge >. val 18)
--     pure (people, blogPosts)




module DB.ExampleEsqueleto where

import MyEsquleto
import Database.Esqueleto (renderQuerySelect)


aaa :: DB [Entity Person]
aaa = select $ do
      people <- from $ Table @Person
      where_ (people ^. PersonName ==. val "John")
      pure people
      
aaaRender :: DB (Text, [PersistValue])
aaaRender = renderQuerySelect $ do
      people <- from $ Table @Person
      where_ (people ^. PersonName ==. val "John")
      pure people

-- bbb :: DB [(Entity Person, Maybe (Entity BlogPost))]
-- bbb = select $ do
--   (people :& blogPosts) <-
--       from $ Table @Person
--       `LeftJoin` Table @BlogPost
--       `on` (\(people :& blogPosts) ->
--               just (people ^. PersonId) ==. blogPosts ?. BlogPostAuthorId)
--   where_ ((people ^. PersonAge) >. just (val 18))
--   pure (people, blogPosts)

ccc :: DB [(Entity Person, Entity BlogPost)]
ccc = select $ do
  (people :& blogPosts) <-
      from $ Table @Person
      `InnerJoin` Table @BlogPost
      `on` (\(people :& blogPosts) ->
              people ^. PersonId ==. blogPosts ^. BlogPostAuthorId)
  where_ (people ^. PersonAge >. just (val 18))
  pure (people, blogPosts)




ddd :: DB [(Entity Follow, Entity Person)]
ddd = select $ do
  (people1 :& followers :& people2) <-
      from $ Table @Person
      `InnerJoin` Table @Follow
      `on` (\(people1 :& followers) ->
              people1 ^. PersonId ==. followers ^. FollowFollowed)
      `InnerJoin` Table @Person
      `on` (\(_ :& followers :& people2) ->
              followers ^. FollowFollower ==. people2 ^. PersonId)
  where_ (people1 ^. PersonName ==. val "John")
  pure (followers, people2)

-- reducing this type
-- exampleD :: (MonadIO m, Num typ, Num a,
--                    BackendCompatible SqlBackend backend, PersistQueryRead backend,
--                    PersistUniqueRead backend, PersistField a, PersistField typ) =>
--                   typ -> ReaderT backend m [Value a]

-- exampleD :: (Num typ, Num a, PersistField a, PersistField typ) => typ -> DB [Value a]
-- reducing this type
-- exampleD :: (Num a, PersistField a) => a -> DB [Value a]
-- reducing this type
-- exampleD :: (DBNum a) => a -> DB [Value a]

