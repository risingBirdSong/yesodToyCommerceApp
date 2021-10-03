{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}


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
    foodTypeIndex :: Int <- randomRIO (0,2) 
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
    _ <- runDB $ insertProduct (toBook apiBook) theTime whareHouseId
    -- contrasted with the longer 
    -- _ <- runDB $ do
    --     prodId <- insert $ Product defaultLocationId theTime 
    -- --let theNewBook = Book aNewProduct  (partialBookWithLocationAuthor partialbook) (partialBookWithLocationGenre partialbook) (partialBookWithLocationPageCount partialbook) (partialBookWithLocationCost partialbook)
    --     insert $ toBook apiBook prodId
    sendResponseStatus status201 ("BOOK stocked in store" :: Text)

postWharehouseNewRandomProduct
  :: (PersistEntity a, Show b,
      PersistEntityBackend a ~ SqlBackend) =>
     (b -> ProductId -> a) -> IO b -> HandlerFor App () 
postWharehouseNewRandomProduct toProduct generateRandomFakeProduct = do
    gotYesod <- getYesod
    let whareHouseId = appWharehouseLocation . appSettings $ gotYesod
    theTime <- getCurrentTime
    apiProduct <- liftIO generateRandomFakeProduct
    _ <- runDB $ insertProduct (toProduct apiProduct) theTime whareHouseId
    print $ apiProduct




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
