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
import DB.TransferProductsLocation
import Database.Esqueleto.Experimental as E hiding (Value)
-- import Database.Esqueleto.Experimental.From.Join

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

-- hmm don't know why this wasnt exported... generateNonDeterministic
genRandom =  FK.generateWithSettings $ FK.setNonDeterministic FK.defaultFakerSettings


-- also consider the reverse where we convert the BD representation to the API representation

-- not being used currently 
data Products = Books Book | Foods Food


-- inferredTypeInsertProduct :: (PersistStoreWrite backend, MonadIO m, PersistEntity record, PersistEntityBackend record ~ BaseBackend backend,  BaseBackend backend ~ SqlBackend) => (Key Product -> record) -> UTCTime -> Int64 -> ReaderT backend m ()

-- insertProduct :: (PersistEntity a, PersistEntityBackend a ~ SqlBackend) => (ProductId -> a) -> UTCTime -> Int64  ->  DB ()
insertProduct :: (PersistEntity a, PersistEntityBackend a ~ SqlBackend) => (ProductId -> a) -> ProductTypes -> UTCTime -> Int64  ->  DB ()
insertProduct f productType now locationId = do
    prodId <- insert $ Product productType (toSqlKey locationId) now
    void $ insert (f prodId)

insertProductBy f productTypes now locationId = do
    prodId <- insert $ Product productTypes (toSqlKey locationId) now
    void $ insertBy (f prodId)
 
postWharehouseAquiresBookR :: Handler Value 
postWharehouseAquiresBookR = do
    gotYesod <- getYesod
    let whareHouseId = appWharehouseLocation . appSettings $ gotYesod
    theTime <- getCurrentTime
    apiBook :: CreateBook <- requireCheckJsonBody
    _ <- runDB $ insertProduct (toBook apiBook) BookProduct theTime whareHouseId
    sendResponseStatus status201 ("BOOK stocked in store" :: Text)

-- so this is using query params but maybe better to put this info in an apiJson?
-- postWhareHouseAcquiresGenericProductR :: Handler Value 
-- postWhareHouseAcquiresGenericProductR = do 
--     gotYesod <- getYesod
--     let whareHouseId = appWharehouseLocation . appSettings $ gotYesod 
--     theTime <- getCurrentTime
--     --example of query parameters
--     getParameters <- reqGetParams <$> getRequest -- Params: [("product","book")]
--     let mybProduct = lookup "product" getParameters -- Just "book"
--     case mybProduct of
--         Nothing -> sendResponseStatus status201 ("BOOK stocked in store" :: Text)
--         Just "book" -> do
--             apiBook :: CreateBook <- requireCheckJsonBody  
--             _ <- runDB $ insertProduct (toBook apiBook) theTime whareHouseId
--             sendResponseStatus status201 ("book inserted into wharehouse" :: Text)

--         Just "food" -> do
--             apiFood :: CreateFood <- requireCheckJsonBody
--             _ <- runDB $ insertProduct (toFood apiFood) theTime whareHouseId
--             sendResponseStatus status201 ("food inserted into wharehouse" :: Text)


-- myTodo bring this back
postWharehouseAquiresFoodR :: Handler Value 
postWharehouseAquiresFoodR = do
    gotYesod <- getYesod 
    let whareHouseId = appWharehouseLocation . appSettings $ gotYesod
    theTime <- getCurrentTime
    apiFood <- requireCheckJsonBody
    theInsertion <- runDB $ insertProduct (toFood apiFood) FoodProduct theTime whareHouseId
    print theInsertion
    sendResponseStatus status201 ("FOOD stocked in store" :: Text)

-- so this works but is very slow myTodo bring this code back
-- postWharehouseNewRandomProductBy toProduct generateRandomFakeProduct = do
--     gotYesod <- getYesod
--     let whareHouseId = appWharehouseLocation . appSettings $ gotYesod
--     theTime <- getCurrentTime
--     apiProduct <- liftIO generateRandomFakeProduct
--     _ <- runDB $ insertProductBy (toProduct apiProduct) theTime whareHouseId
--     print $ apiProduct

--myTodo bring this code back
-- postWharehouseNewRandomProduct toProduct generateRandomFakeProduct = do
--     gotYesod <- getYesod
--     let whareHouseId = appWharehouseLocation . appSettings $ gotYesod
--     theTime <- getCurrentTime
--     apiProduct <- liftIO generateRandomFakeProduct
--     _ <- runDB $ insertProduct (toProduct apiProduct) theTime whareHouseId
--     _ <- runDB deleteAllBooks'
--     print $ apiProduct


-- SqlPersistT m ~ ReaderT SqlBackend m
-- type SqlPersistT = ReaderT SqlBackend
deleteAllBooks' :: MonadIO m => ReaderT SqlBackend m  ()
deleteAllBooks' = deleteWhere ([] :: [Filter Book])

deleteAllBooks :: Handler ()
deleteAllBooks = runDB $ deleteWhere ([] :: [Filter Book])


deleteAllFoods :: Handler ()
deleteAllFoods = runDB $ deleteWhere ([] :: [Filter Food])



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


-- findProduct prodId = do 
--     select $ do
--         (prod :& food) <- 
--             from $ Table @Product 
--                 `LeftJoin` Table @Food
--                 `E.on` (\(prod :& food) -> 
--                         just (prod ^. ProductId) E.==. food ?. FoodProductId
--                     )
--         pure (prod ^. ProductId , food ?. FoodProductId)

-- transferAProdFromLocAtoB :: todo
-- transferAProdFromLocAtoB prodId locA locB = do
--     runDB $ do
--        maybeProd <- selectFirst [] []  
