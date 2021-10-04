{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}



module Handler.Products where

import Import.Lifted 
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
    apiBook <- requireCheckJsonBody
    -- _ <- runDB $ insertProduct (toBook apiBook) theTime whareHouseId
    _ <- runDB $ insertProductBy (toBook apiBook) theTime whareHouseId

    -- contrasted with the longer 
    -- _ <- runDB $ do
    --     prodId <- insert $ Product defaultLocationId theTime 
    -- --let theNewBook = Book aNewProduct  (partialBookWithLocationAuthor partialbook) (partialBookWithLocationGenre partialbook) (partialBookWithLocationPageCount partialbook) (partialBookWithLocationCost partialbook)
    --     insert $ toBook apiBook prodId
    sendResponseStatus status201 ("BOOK stocked in store" :: Text)


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

-- inferred type
-- postWharehouseAquiresProductR
--   :: (PersistEntity a, FromJSON t,
--       PersistEntityBackend a ~ SqlBackend) =>
--      (t -> ProductId -> a) -> HandlerFor App b
postWharehouseAquiresProductR toProduct = do 
    gotYesod <- getYesod 
    let whareHouseId = appWharehouseLocation . appSettings $ gotYesod    
    theTime <- getCurrentTime
    apiThing <- requireCheckJsonBody
    theInsertion <- runDB $ insertProduct (toProduct apiThing) theTime whareHouseId
    print theInsertion
    sendResponseStatus status201 ("PRODUCT stocked in store" :: Text)


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
    print $ apiProduct




-- deleteAllBooks :: (MonadIO m, PersistQueryWrite backend,
--         BaseBackend backend ~ SqlBackend) =>
--         Handler ()

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
deleteAllBooks :: Handler ()
deleteAllBooks = runDB $ deleteWhere ([] :: [Filter Book])
    -- sendResponseStatus status201 ("FOOD stocked in store" :: Text)


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
--                                           p -> Handler ()
-- deleteTypeOfProduct product = runDB $ deleteWhere ([] :: [Filter product])
    