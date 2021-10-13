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
import Data.Text.Read
-- import Database.Esqueleto.Experimental as E hiding (Value)
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
    foodTypeIndex :: Int <- randomRIO (0,length foodTypes -1 ) 
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

makeMainWharehouse :: Handler () 
makeMainWharehouse = do
    runDB $ do    
        wharehouseStocklocationId <- insert $ StockLocation "main wharehouse location"
        mainWharehouse <- insert $ Wharehouse wharehouseStocklocationId "main wharehouse"
        pure ()

makeAStore :: StockLocationId -> Text -> Int -> Handler () 
makeAStore locId name balance = do 
    runDB $ do
        insert $ StockLocation name 
        insert $ Store locId name balance
    pure ()

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
postWharehouseNewRandomProductBy toProduct theProductType generateRandomFakeProduct = do
    gotYesod <- getYesod
    let whareHouseId = appWharehouseLocation . appSettings $ gotYesod
    theTime <- getCurrentTime
    apiProduct <- liftIO generateRandomFakeProduct
    _ <- runDB $ insertProductBy (toProduct apiProduct) theProductType theTime whareHouseId
    print $ apiProduct

postWharehouseNewRandomProduct toProduct theProductType generateRandomFakeProduct = do
    gotYesod <- getYesod
    let whareHouseId = appWharehouseLocation . appSettings $ gotYesod
    theTime <- getCurrentTime
    apiProduct <- liftIO generateRandomFakeProduct
    _ <- runDB $ insertProduct (toProduct apiProduct) theProductType theTime whareHouseId
    _ <- runDB deleteAllBooks'
    print $ apiProduct


-- SqlPersistT m ~ ReaderT SqlBackend m
-- type SqlPersistT = ReaderT SqlBackend
deleteAllBooks' :: MonadIO m => ReaderT SqlBackend m  ()
deleteAllBooks' = deleteWhere ([] :: [Filter Book])

deleteAllBooks :: Handler ()
deleteAllBooks = runDB $ deleteWhere ([] :: [Filter Book])


deleteAllFoods :: Handler ()
deleteAllFoods = runDB $ deleteWhere ([] :: [Filter Food])

deleteAllProducts :: Handler ()
deleteAllProducts = runDB $ deleteWhere ([] :: [Filter Product])


-- deleteTypeOfProduct :: forall a m. (PersistEntityBackend a ~ SqlBackend, MonadIO m, PersistEntity a)=> Proxy a -> SqlPersistT m ()
-- deleteTypeOfProduct _ = deleteWhere ([] :: [Filter a])

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


postLocationsInventoryR :: Handler Value
postLocationsInventoryR = do
    locationId <- lookupGetParam "locationid"
    case locationId of
        Nothing -> sendResponseStatus status404 ("that location was not found" :: Text)
        Just locId -> do
            case decimal locId of
                Left fail -> sendResponseStatus status404 ("we couldnt parse that number" :: Text)
                Right (theint, _) -> do
                    productsAtLocation <- runDB $ selectList [ProductStockLocationId ==. (toSqlKey theint) ] []
                    returnJson productsAtLocation

-- transferAProdFromLocAtoB :: todo
-- place these args in json
develTransferAProdFromLocAtoB prodId locB = do
    runDB $ do
        maybeProd <- updateWhere [ProductId ==. prodId] [ProductStockLocationId =. locB]
        pure maybeProd

data TransferProdLocationFromAToBJson = TransferProdLocationFromAToBJson {
-- so both these needs to be used with toSqKey 
        productId :: Int64 ,
        transferLocation :: Int64
    }
    deriving stock Generic
    deriving anyclass FromJSON
    deriving Show

postTransferAProdFromLocAtoBR :: Handler Value
postTransferAProdFromLocAtoBR = do
    transfer :: TransferProdLocationFromAToBJson <- requireCheckJsonBody
    let TransferProdLocationFromAToBJson {..} = transfer
    void $ runDB $ updateWhere [ProductId ==. (toSqlKey productId)] [ProductStockLocationId =. (toSqlKey transferLocation)]
    sendResponseStatus status201 ("UPDATE ATTEMPTED" :: Text)

-- batch transfer, transferring a list of products from one location to another (like an equivalent for a truckload delivery from wharehouse to store)
data TransferListProdLocationFromAToBJson = TransferListProdLocationFromAToBJson {
-- so both these needs to be used with toSqKey 
        productIds :: [Int64] ,
        transferLocationForList :: Int64
    }
    deriving stock Generic
    deriving anyclass FromJSON
    deriving Show
-- is there a better way to handle this batch update? it seems ok and I didn't see a custom function for it
postTransferListProdFromLocAtoBR :: Handler Value 
postTransferListProdFromLocAtoBR = do 
    transferList :: TransferListProdLocationFromAToBJson <- requireCheckJsonBody
    let TransferListProdLocationFromAToBJson {..} = transferList
    void $ runDB $ do
       void $ mapM_ (\prodId -> updateWhere [ProductId ==. (toSqlKey prodId)] [ProductStockLocationId =. (toSqlKey transferLocationForList)]) productIds
    sendResponseStatus status201 ("MANY UPDATE ATTEMPTED" :: Text)



-- myTodo updateWhere returns () but maybe there should be a version that returns whether it updated or not
-- the function to do this is updateWhereCount 