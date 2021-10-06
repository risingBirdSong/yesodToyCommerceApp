{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}



module Handler.Products where

import Import.Lifted hiding (Proxy)
import Data.Proxy
import Data.Coerce
import Database.Persist.Sql (toSqlKey)
import Myutils
import qualified Faker as FK
import qualified Faker.Book as FK
import qualified Faker.Address as FK
import qualified Faker.Food as FK
import System.Random
import Data.List ((!!))

data ABook = ABook {aBook :: Entity Book}
    deriving (Generic)

instance FromJSON ABook
instance ToJSON ABook

-- getStoreAquiresBookR :: Handler ()
-- getStoreAquiresBookR = do 
--     liftIO $ print "hitting" 
--     pure ()

-- so how can we abstract these two functions? Where they're mostly similar. Can we pass in the type and Constructor in as arguments?


toBook :: CreateBook -> ProductId -> Book
toBook CreateBook {..} prodId = Book prodId title author genre pageCount cost


-- the API version of book
data CreateBook = CreateBook
    { title :: Text
    , author :: Text
    , genre :: Text
    , pageCount :: Int
    , cost :: Int
    }
    deriving stock Generic
    deriving anyclass FromJSON
    deriving Show

-- (genRandom FK.author) (genRandom FK.genre) (randomRIO (50, 500) :: IO Int) (randomRIO (5, 50) :: IO Int)

-- TODO maybe try to pull the specific generateFakeBook and generateFakeFood into a generic helper function? 

generateFakeBook :: IO CreateBook
generateFakeBook = do
    title <- genRandom FK.title
    auth <- genRandom FK.author
    genre <- genRandom FK.genre
    pages <- randomRIO (50, 500)
    cost <- randomRIO (5, 50)
    pure $ CreateBook title auth genre pages cost 


toFood :: CreateFood -> ProductId -> Food
toFood CreateFood {..} prodId = Food prodId foodType name weight description

data CreateFood = CreateFood
    {   foodType :: FoodTypes 
    ,   name :: Text
    ,   weight :: Double
    ,   description :: Text        
    }  
    deriving stock Generic
    deriving anyclass FromJSON
    deriving Show

generateFakeFood :: IO CreateFood  
generateFakeFood = do 
    let foodTypes = [Beverage , Snack , Dinner ]
    foodTypeIndex :: Int <- randomRIO (0,length foodTypes) 
    name <- genRandom (FK.dish)
    weight :: Double <- randomRIO (1.0, 10.0)
    description <- genRandom (FK.descriptions)
    pure $ CreateFood (foodTypes !! foodTypeIndex) name weight description

-- possibleFoods = [Beverage , Snack , Dinner]

-- generateFakeFood :: IO CreateFood
-- generateFakeFood = do
--     pure 

-- hmm don't know why this wasnt exported... generateNonDeterministic
genRandom =  FK.generateWithSettings $ FK.setNonDeterministic FK.defaultFakerSettings


-- also consider the reverse where we convert the BD representation to the API representation

data Products = Books Book | Foods Food

insertProduct :: (PersistEntity a, PersistEntityBackend a ~ SqlBackend) => (ProductId -> a) -> UTCTime -> Int64  ->  DB ()
insertProduct f now locationId = do
    prodId <- insert $ Product (toSqlKey locationId) now
    void $ insert (f prodId)

insertProductBy f now locationId = do
    prodId <- insert $ Product (toSqlKey locationId) now
    void $ insertBy (f prodId)

--  where
--     defaultLocationId = toSqlKey (1 :: Int64)

-- newtype Sum = Sum { unSum :: Int }

-- coerce 2 :: Sum
-- coerce (Sum 2) :: Int

-- requireCheckJsonBody :: forall a m. (MonadHandler m, FromJSON a) => m ()

 
postWharehouseAquiresBookR :: Handler Value 
postWharehouseAquiresBookR = do
    gotYesod <- getYesod
    let whareHouseId = appWharehouseLocation . appSettings $ gotYesod
    -- :: type annotation
    -- @ type application
    theTime <- getCurrentTime
    apiBook :: CreateBook <- requireCheckJsonBody
    _ <- runDB $ insertProduct (toBook apiBook) theTime whareHouseId
    -- liftIO $ print apiBook
    --myTodo bring the below line back from comment
    -- _ <- runDB $ insertProductBy (toBook apiBook) theTime whareHouseId

    -- contrasted with the longer 
    -- _ <- runDB $ do
    --     prodId <- insert $ Product defaultLocationId theTime 
    -- --let theNewBook = Book aNewProduct  (partialBookWithLocationAuthor partialbook) (partialBookWithLocationGenre partialbook) (partialBookWithLocationPageCount partialbook) (partialBookWithLocationCost partialbook)
    --     insert $ toBook apiBook prodId
    sendResponseStatus status201 ("BOOK stocked in store" :: Text)

-- herehere
postWhareHouseAcquiresGenericProductR :: Handler Value 
postWhareHouseAcquiresGenericProductR = do 
    gotYesod <- getYesod
    let whareHouseId = appWharehouseLocation . appSettings $ gotYesod 
    theTime <- getCurrentTime
    --example of query parameters
    getParameters <- reqGetParams <$> getRequest -- Params: [("product","book")]
    let mybProduct = lookup "product" getParameters -- Just "book"
    case mybProduct of
        Nothing -> sendResponseStatus status201 ("BOOK stocked in store" :: Text)
        Just "book" -> do
            apiBook :: CreateBook <- requireCheckJsonBody  
            _ <- runDB $ insertProduct (toBook apiBook) theTime whareHouseId
            sendResponseStatus status201 ("book inserted into wharehouse" :: Text)

        Just "food" -> do
            apiFood :: CreateFood <- requireCheckJsonBody
            _ <- runDB $ insertProduct (toFood apiFood) theTime whareHouseId
            sendResponseStatus status201 ("food inserted into wharehouse" :: Text)

    -- mytodo remove the dleete, its jsut for cleanup
    -- _ <- runDB $ deleteTypeOfProduct'' @Book
    

postWharehouseAquiresFoodR :: Handler Value 
postWharehouseAquiresFoodR = do
    gotYesod <- getYesod 
    let whareHouseId = appWharehouseLocation . appSettings $ gotYesod
    theTime <- getCurrentTime
    apiFood <- requireCheckJsonBody
    theInsertion <- runDB $ insertProduct (toFood apiFood) theTime whareHouseId
    print theInsertion
    -- aNewProduct <- runDB $ insert $ Product (partialFoodWithLocationLocationId partialFood) theTime
    -- let theNewFood = Food aNewProduct (partialFoodWithLocationFoodType partialFood) (partialFoodWithLocationBrand partialFood) (partialFoodWithLocationName partialFood) (partialFoodWithLocationWeight partialFood) (partialFoodWithLocationDescription partialFood)
    -- insertedbook <- runDB $ insert $ theNewFood 
    -- print "food inserted"
    sendResponseStatus status201 ("FOOD stocked in store" :: Text)

-- ok so postWharehouseAquiresProductR is interesting but not practical, how would you pass in toProduct?

-- inferred type
-- postWharehouseAquiresProductR
--   :: (PersistEntity a, FromJSON t,
--       PersistEntityBackend a ~ SqlBackend) =>
--      (t -> ProductId -> a) -> HandlerFor App b
-- postWharehouseAquiresProductR toProduct = do 
--     gotYesod <- getYesod 
--     let whareHouseId = appWharehouseLocation . appSettings $ gotYesod    
--     theTime <- getCurrentTime
--     apiThing <- requireCheckJsonBody
--     theInsertion <- runDB $ insertProduct (toProduct apiThing) theTime whareHouseId
--     print theInsertion
--     sendResponseStatus status201 ("PRODUCT stocked in store" :: Text)

-- postWharehouseNewRandomProduct
--   :: (PersistEntity a, Show b,
--       PersistEntityBackend a ~ SqlBackend) =>
--      (b -> ProductId -> a) -> IO b -> HandlerFor App () 





-- so this works but is very slow
postWharehouseNewRandomProductBy toProduct generateRandomFakeProduct = do
    gotYesod <- getYesod
    let whareHouseId = appWharehouseLocation . appSettings $ gotYesod
    theTime <- getCurrentTime
    apiProduct <- liftIO generateRandomFakeProduct
    _ <- runDB $ insertProductBy (toProduct apiProduct) theTime whareHouseId
    print $ apiProduct


postWharehouseNewRandomProduct toProduct generateRandomFakeProduct = do
    gotYesod <- getYesod
    let whareHouseId = appWharehouseLocation . appSettings $ gotYesod
    theTime <- getCurrentTime
    apiProduct <- liftIO generateRandomFakeProduct
    _ <- runDB $ insertProduct (toProduct apiProduct) theTime whareHouseId
    _ <- runDB deleteAllBooks'
    print $ apiProduct


-- SqlPersistT m ~ ReaderT SqlBackend m
-- type SqlPersistT = ReaderT SqlBackend
deleteAllBooks' :: MonadIO m => ReaderT SqlBackend m  ()
deleteAllBooks' = deleteWhere ([] :: [Filter Book])

-- deleteAllBooks :: Handler ()

-- QQQ 1
--so without the above type, it is ambigious and get this error... is there a way to constrain the type to only a SqlBackend since I'm only using PostgreSQL

--    • Couldn't match type ‘BaseBackend backend0’ with ‘SqlBackend’
--         arising from a use of ‘deleteWhere’
--       The type variable ‘backend0’ is ambiguous
--     • In the expression: deleteWhere ([] :: [Filter Book])
--       In an equation for ‘deleteAllBooks’:
--           deleteAllBooks = deleteWhere ([] :: [Filter Book])
--     • Relevant bindings include
--         deleteAllBooks :: ReaderT backend0 m0 ()
--           (bound at src/Handler/Products.hs:165:1)

-- runDB
--   :: YesodPersist site =>
--      ReaderT (YesodPersistBackend site) (HandlerFor site) a
--      -> HandlerFor site a

-- :t deleteWhere
-- (MonadIO m, PersistQueryWrite backend, PersistEntity record,
--       PersistEntityBackend record ~ BaseBackend backend) =>
--      [Filter record] -> ReaderT backend m ()

-- deleteWhere ([] :: [Filter Book])
--   :: (MonadIO m, PersistQueryWrite backend,
--       BaseBackend backend ~ SqlBackend) =>
--    ReaderT backend m ()
-- type YesodDB site = ReaderT (YesodPersistBackend site) (HandlerFor site)

-- runDB
-- YesodDB site a -> HandlerFor site a
-- HandleFor
-- HandlerFor { unHandlerFor :: HandlerData site site -> IO a }
-- ReaderT (YesodPersistBackend site) (HandlerFor site) a -> HandlerData site site -> IO a

-- the a is really only the thing that we have control over 
-- how could we fill in the other type params? Type applications on runDB 

-- Handler is a type alias that is generated 

---- ReaderT (YesodPersistBackend site) (HandlerFor site) a -> HandlerData site site -> IO a


--deleteAllBooks :: HandlerFor site () -- this compiles because this is just an alias for Handler ()
-- deleteAllBooks = runDB @App @() $ deleteWhere ([] :: [Filter Book]) 
-- ~
-- deleteAllBooks = (runDB :: YesodDB App () -> HandlerFor App ()) $ deleteWhere ([] :: [Filter Book])

-- deleteWhere, a function like this is only operating on the polymorphic a, but it is constrained backend 
deleteAllBooks :: Handler ()
deleteAllBooks = runDB $ deleteWhere ([] :: [Filter Book])
    -- sendResponseStatus status201 ("FOOD stocked in store" :: Text)

--type instance PersistEntityBackend Book = SqlBackend


-- class (PersistField (Key record), ToJSON (Key record),
--        FromJSON (Key record), Show (Key record), Read (Key record),
--        Eq (Key record), Ord (Key record)) =>
--       PersistEntity record where
--   ...
--   data family Key record


deleteAllFoods :: Handler ()
deleteAllFoods = runDB $ deleteWhere ([] :: [Filter Food])


-- QQQ 2 how can we pass this product in as an argument, the types get tricky  

-- deleteTypeOfProduct :: (MonadIO m, PersistQueryWrite backend,
--         BaseBackend backend ~ SqlBackend) =>
--         record -> ReaderT backend m ()

-- deleteTypeOfProduct :: Handler ()
--  deleteTypeOfProduct :: forall site record p.
--                                           (PersistQueryWrite (YesodPersistBackend site),
--                                            YesodPersist site, PersistEntity record,
--                                            PersistEntityBackend record
--                                            ~ BaseBackend (YesodPersistBackend site)) =>
--      
-- the Proxy a only exists at type level, not at value level                                     p -> Handler ()
-- data Proxy a = Proxy
deleteTypeOfProduct :: forall a m. (PersistEntityBackend a ~ SqlBackend, MonadIO m, PersistEntity a)=> Proxy a -> SqlPersistT m ()
deleteTypeOfProduct _ = deleteWhere ([] :: [Filter a])

-- deleteTypeOfProduct' (Proxy :: Proxy Book)
-- deleteTypeOfProduct' (Proxy @Book)    

deleteTypeOfProduct'' :: forall a m. (PersistEntityBackend a ~ SqlBackend, MonadIO m, PersistEntity a) => SqlPersistT m ()
deleteTypeOfProduct'' = deleteWhere @_ @_ @a []

-- call this in src/Application.hs 
handleDeleteAllBook :: Handler ()
handleDeleteAllBook = do
    _ <- runDB $ deleteTypeOfProduct'' @Book
    liftIO $ putStrLn "i just deleted all books"
    pure ()
