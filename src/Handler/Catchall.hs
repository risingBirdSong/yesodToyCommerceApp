{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}



module Handler.Catchall where

import Import.Lifted hiding (Proxy)
import Data.Proxy
import Data.Coerce
-- import Database.Persist.Sql (toSqlKey)
import Myutils
import qualified Faker as FK
import qualified Faker.Book as FK
import qualified Faker.Address as FK
import qualified Faker.Food as FK
import System.Random
import Data.List ((!!))
import DB.TransferProductsLocation
import Data.Text.Read
import Database.Persist.Sql
import DB.StoreInventory
import qualified Data.List as L
import Data.Aeson
import Data.Aeson.Casing
import Control.Monad.Trans.Maybe
import Control.Monad.Validate
import qualified Data.Validation as Vld
import Control.Lens ((#))
import Control.Concurrent


data ABook = ABook {aBook :: Entity Book}
    deriving (Generic)

instance FromJSON ABook
instance ToJSON ABook


toBook :: CreateBook -> ProductId -> Book
toBook CreateBook {..} prodId = Book prodId bookTitle bookAuthor bookGenre bookPageCount


-- the API version of book
data CreateBook = CreateBook
    { bookTitle :: Text
    , bookAuthor :: Text
    , bookGenre :: Text
    , bookPageCount :: Int
    , bookCost :: Int
    }
    deriving stock (Show, Generic)

instance ToJSON CreateBook
instance FromJSON CreateBook


-- instance ToJSON CreateBook where 
--     toJSON = genericToJSON defaultOptions
--         {
--             fieldLabelModifier = snakeCase . drop (length ("book" :: String))
--         }

-- instance FromJSON CreateBook where 
--     parseJSON = genericParseJSON defaultOptions
--         {
--         fieldLabelModifier = snakeCase . drop (length ("book" :: String))
--         }

-- consider placing the random logic into monadRandom , more restricted
generateFakeBook :: IO CreateBook
generateFakeBook = do
    title <- genRandom FK.title
    auth <- genRandom FK.author
    genre <- genRandom FK.genre
    pages <- randomRIO (50, 500)
    cost <- randomRIO (5, 50)
    pure $ (CreateBook title auth genre pages cost)


toFood :: CreateFood -> ProductId -> Food
toFood CreateFood {..} prodId = Food prodId foodFoodType foodName foodWeight foodDescription

data CreateFood = CreateFood
    {   foodFoodType :: FoodTypes 
    ,   foodName :: Text
    ,   foodWeight :: Double
    ,   foodDescription :: Text 
    ,   foodCost :: Int 
    }  
    deriving stock Generic
    deriving Show

instance ToJSON CreateFood
instance FromJSON CreateFood


-- instance ToJSON CreateFood where 
--     toJSON = genericToJSON defaultOptions
--         {
--             fieldLabelModifier = snakeCase . drop (length "food")
--         }

-- instance FromJSON CreateFood where 
--     parseJSON = genericParseJSON defaultOptions
--         {
--         fieldLabelModifier = snakeCase . drop (length "food")
--         }

generateFakeFood :: IO CreateFood  
generateFakeFood = do 
    let foodTypes = [Beverage , Snack , Dinner ]
    foodTypeIndex :: Int <- randomRIO (0,length foodTypes -1 ) 
    name <- genRandom (FK.dish)
    weight :: Double <- randomRIO (1.0, 10.0)
    cost :: Int <- randomRIO (1, 20)
    description <- genRandom (FK.descriptions)
    pure $ (CreateFood (foodTypes !! foodTypeIndex) name weight description cost)

-- hmm don't know why this wasnt exported... generateNonDeterministic
genRandom =  FK.generateWithSettings $ FK.setNonDeterministic FK.defaultFakerSettings


-- also consider the reverse where we convert the BD representation to the API representation

-- not being used currently 
data Products = Books Book | Foods Food



-- inferredTypeInsertProduct :: (PersistStoreWrite backend, MonadIO m, PersistEntity record, PersistEntityBackend record ~ BaseBackend backend,  BaseBackend backend ~ SqlBackend) => (Key Product -> record) -> UTCTime -> Int64 -> ReaderT backend m ()
insertProduct :: (PersistEntity a, PersistEntityBackend a ~ SqlBackend) => (ProductId -> a) -> ProductTypes -> UTCTime -> Int -> Key StockLocation ->  DB ()
insertProduct f productType now cost locationId = do
    prodId <- insert $ Product productType locationId cost now
    void $ insert (f prodId)

insertProductBy f productTypes now cost locationId = do
    prodId <- insert $ Product productTypes locationId cost now
    void $ insertBy (f prodId)

makeMainWharehouseDB :: DB ()
makeMainWharehouseDB = do
    wharehouseStocklocationId <- insert $ StockLocation "main wharehouse location"
    insert_ $ Wharehouse wharehouseStocklocationId "main wharehouse"

makeMainWharehouseHandler :: Handler () 
makeMainWharehouseHandler = runDB $ makeMainWharehouseDB

makeAStore :: StockLocationId -> Text -> Int -> Handler () 
makeAStore locId name balance = do 
    runDB $ do
        insert $ StockLocation name 
        insert $ Store locId name balance
    pure ()

-- overloadedRecordDot
-- NoFieldSelectors

postWharehouseAquiresBookR :: Handler Text 
postWharehouseAquiresBookR = do
    gotYesod <- getYesod
    let whareHouseId = appWharehouseLocation . appSettings $ gotYesod
    theTime <- getCurrentTime
    apiBook :: CreateBook <- requireCheckJsonBody
    _ <- runDB $ insertProduct (toBook apiBook) BookProduct theTime (bookCost apiBook) whareHouseId
    sendResponseStatus status201 ("BOOK stocked in store" :: Text)

-- myTodo bring this back
postWharehouseAquiresFoodR :: Handler Value 
postWharehouseAquiresFoodR = do
    gotYesod <- getYesod 
    let whareHouseId = appWharehouseLocation . appSettings $ gotYesod
    theTime <- getCurrentTime
    apiFood <- requireCheckJsonBody
    theInsertion <- runDB $ insertProduct (toFood apiFood) FoodProduct theTime (foodCost apiFood) whareHouseId
    print theInsertion
    sendResponseStatus status201 ("FOOD stocked in store" :: Text)

-- so this works but is very slow myTodo bring this code back
postWharehouseNewRandomProductBy toProduct theProductType generateRandomFakeProduct = do
    gotYesod <- getYesod
    let whareHouseId = appWharehouseLocation . appSettings $ gotYesod
    theTime <- getCurrentTime
    apiProduct <- liftIO generateRandomFakeProduct
    cost :: Int <- liftIO $ randomRIO (1, 100)
    _ <- runDB $ insertProductBy (toProduct apiProduct) theProductType theTime cost whareHouseId
    print $ apiProduct

postWharehouseNewRandomProduct toProduct theProductType generateRandomFakeProduct = do
    gotYesod <- getYesod
    let whareHouseId = appWharehouseLocation . appSettings $ gotYesod
    theTime <- getCurrentTime
    (apiProduct, cost) <- liftIO generateRandomFakeProduct
    _ <- runDB $ insertProduct (toProduct apiProduct) theProductType theTime  cost whareHouseId
    _ <- runDB deleteAllBooks'
    print $ apiProduct


-- SqlPersistT m ~ ReaderT SqlBackend m
-- type SqlPersistT = ReaderT SqlBackend
-- deleteAllBooks' :: MonadIO m => ReaderT SqlBackend m  ()
deleteAllBooks' :: DB ()
deleteAllBooks' = deleteWhere ([] :: [Filter Book])

deleteAllBooks :: DB ()
deleteAllBooks = deleteWhere ([] :: [Filter Book])


deleteAllFoods :: DB ()
deleteAllFoods = deleteWhere ([] :: [Filter Food])

deleteAllProducts :: DB ()
deleteAllProducts = deleteWhere ([] :: [Filter Product])

deleteTypeOfProduct'' :: forall a m. (PersistEntityBackend a ~ SqlBackend, MonadIO m, PersistEntity a) => SqlPersistT m ()
deleteTypeOfProduct'' = deleteWhere @_ @_ @a []

-- call this in src/Application.hs 
handleDeleteAllBook :: DB ()
handleDeleteAllBook = deleteTypeOfProduct'' @Book

data LocationsInventoryAPI = LocationsInventoryAPI {
    locationInventory :: Key StockLocation
    } 
    deriving stock Generic
    deriving anyclass FromJSON
    deriving Show

data ProductKeyAPI = ProductKeyAPI {
    productKeyAPI :: Key Product 
    }  
    deriving stock Generic
    deriving anyclass FromJSON
    deriving Show

-- probs delete this 
-- data ProductHistoryListAPI = ProductHistoryListAPI {
--     prodHists :: [Entity ProductHistory] 
--     } 
--     deriving stock Generic
--     deriving anyclass FromJSON
--     deriving anyclass ToJSON
--     deriving Show

getLocationsInventoryR :: Handler Value
getLocationsInventoryR = do
    LocationsInventoryAPI {..} <- requireCheckJsonBody  
    productsAtLocation <- runDB $ selectList [ProductStockLocationId ==. locationInventory] []
    returnJson productsAtLocation

-- transferAProdFromLocAtoB :: todo
-- place these args in json
develTransferAProdFromLocAtoB prodId locB = updateWhere [ProductId ==. prodId] [ProductStockLocationId =. locB]
        

data TransferProdLocationFromAToBJson = TransferProdLocationFromAToBJson {
-- so both these needs to be used with toSqKey 
        productId :: Key Product ,
        transferOrigin :: Key StockLocation , 
        transferDestination :: Key StockLocation
    }
    deriving stock Generic
    deriving anyclass FromJSON
    deriving Show

productAtLocation :: Key Product -> Key StockLocation -> DB (Maybe (Entity Product))
productAtLocation productId  transferOrigin = selectFirst [ProductId  ==. productId, ProductStockLocationId ==. transferOrigin] []

-- TRANSFER!
postTransferAProdFromLocAtoB_R :: Handler Value
postTransferAProdFromLocAtoB_R = do
    TransferProdLocationFromAToBJson {..} <- requireCheckJsonBody
    thetime <- getCurrentTime
    returnmessage :: Either Text Text <- runDB $ do 
        ensureProdLoc <- productAtLocation productId transferOrigin
        ensureDestinationExists <- selectFirst [StockLocationId ==. transferDestination] []
        nameOfOrigin <- selectFirst [StockLocationId ==. transferOrigin] []
        nameOfDestination <- selectFirst [StockLocationId ==. transferDestination] []
        case (ensureProdLoc, ensureDestinationExists,  nameOfOrigin, nameOfDestination) of 
            (Just prod, Just (destExist) , Just (Entity _ nameOrigin), Just (Entity _ nameDest)) -> do 
                updated <- updateWhere [ProductId ==. productId] [ProductStockLocationId =. transferDestination]
                _ <- addToProductHistory productId transferDestination thetime False
                pure $ Right ("The product was transferred from " <> ( stockLocationName nameOrigin) <> " to " <> (stockLocationName nameDest))
            (_,_,_,_) -> pure $ Left ("check the product location and origin")
    -- if this pattern happens again and again, then consider a custom ToJson Instance on a newtype wrapper for an Either,
    -- could just return the json, probs not that big of a deal
    -- or just pattern matching like as a one off task that isn't repeated much
    case returnmessage of
        Right msg -> pure $ object ["Right" .= msg]
        Left msg -> pure $ object ["Left" .= msg]

-- verifyAndUpdateLocation :: MonadUnliftIO m => TransferProdLocationFromAToBJson -> SqlPersistT m (Either Text Text)
verifyAndUpdateLocation :: TransferProdLocationFromAToBJson -> DB (Either Text Text)
verifyAndUpdateLocation (TransferProdLocationFromAToBJson {..}) = do
    ensureProdLoc <- productAtLocation productId transferOrigin
    ensureDestinationExists <- selectFirst [StockLocationId ==. transferDestination] []
    case validateProdAtLocationAndDestinationExists ensureProdLoc ensureDestinationExists of
        Vld.Failure errs -> do
            pure $ Left errs 
        Vld.Success _ -> do 
            updated <- updateWhere [ProductId ==. productId, ProductStockLocationId ==. transferOrigin] [ProductStockLocationId =. transferDestination]
            pure $ Right "updated"

verifyAndUpdateLocationWithRegularArgsAndAddToHistory :: Key Product -> Key StockLocation -> Key StockLocation -> UTCTime -> DB (Either Text Text)
verifyAndUpdateLocationWithRegularArgsAndAddToHistory productId transferOrigin transferDestination thetime = do 
    ensureProdLoc <- productAtLocation productId transferOrigin
    ensureDestinationExists <- selectFirst [StockLocationId ==. transferDestination] []
    case validateProdAtLocationAndDestinationExists ensureProdLoc ensureDestinationExists of
        Vld.Failure errs -> do
            pure $ Left errs 
        Vld.Success _ -> do 
            _ <- addToProductHistory productId transferDestination thetime False
            updated <- updateWhere [ProductId ==. productId, ProductStockLocationId ==. transferOrigin] [ProductStockLocationId =. transferDestination]
            pure $ Right "updated"

-- TRANSFER!
postTransferAProdFromLocAtoB_ValidationR :: Handler () 
postTransferAProdFromLocAtoB_ValidationR = do 
    thetime <- liftIO $ getCurrentTime
    TransferProdLocationFromAToBJson {..} <- requireCheckJsonBody
    res <- runDB $ verifyAndUpdateLocationWithRegularArgsAndAddToHistory productId transferOrigin transferDestination thetime
    case res of
        Left errs -> sendResponseStatus status400 (errs :: Text)
        Right _ ->   sendResponseStatus status201 ("The product was transferred" :: Text)

    
maybeToValidation :: Text -> Maybe a -> Vld.Validation Text a 
maybeToValidation e myb = case myb of
    Nothing -> Vld._Failure # (" " <> e <> " ")
    Just val -> Vld._Success # val 

validateProdAtLocationAndDestinationExists prodLoc dest = 
    maybeToValidation (pack "product isnt at origin") prodLoc <*
    maybeToValidation (pack "destination doesnt exist") dest 

-- batch transfer, transferring a list of products from one location to another (like an equivalent for a truckload delivery from wharehouse to store)
data TransferListProdLocationFromAToBJson = TransferListProdLocationFromAToBJson {
-- so both these needs to be used with toSqKey 
        productIds :: [Key Product] ,
        transferOriginForList :: Key StockLocation ,
        transferDestinationForList :: Key StockLocation
    }
    deriving stock Generic
    deriving anyclass FromJSON
    deriving Show
-- is there a better way to handle this batch update? it seems ok and I didn't see a custom function for it

-- TRANSFER!
postTransferListProdFromLocAtoBR :: Handler Value 
postTransferListProdFromLocAtoBR = do 
    transferList :: TransferListProdLocationFromAToBJson <- requireCheckJsonBody
    let TransferListProdLocationFromAToBJson {..} = transferList
    thetime <- liftIO getCurrentTime
    attemptedUpdates <- runDB $ do
        mapM (\prodId -> verifyAndUpdateLocationWithRegularArgsAndAddToHistory prodId transferOriginForList transferDestinationForList thetime) productIds
    return $ object ["attemptedUpdates" .= attemptedUpdates]

-- myTodo updateWhere returns () but maybe there should be a version that returns whether it updated or not
-- a function to do this is updateWhereCount 

data EsqA = EsqA {
        loc :: Int64
    } deriving (Show, Eq, Generic)

instance FromJSON EsqA
instance ToJSON EsqA

postLikeselectListEsq :: Handler Value
postLikeselectListEsq = do 
   EsqA {..} <- requireCheckJsonBody 
   prodsAtLocation <- runDB $ likeSelectList_Esq (toSqlKey loc)
   returnJson prodsAtLocation 



-- transfer prod function 
-- need a productId from the relevant transfer
-- for inspiration -> productAtLocation :: Key Product -> Key StockLocation -> DB (Maybe (Entity Product))
-- insertProduct :: (PersistEntity a, PersistEntityBackend a ~ SqlBackend) => (ProductId -> a) -> ProductTypes -> UTCTime -> Key StockLocation  ->  DB ()

-- addToProductHistory :: Key Product -> Key StockLocation -> UTCTime -> DB (Key ProductHistory)
addToProductHistory prodKey locKey time soldToCustomer = do 
    -- myTodo consider soldToCustomer, if it True then we dont want want to do any inserting and want to report that this is unexpected
    prodHistId <- insert $ ProductHistory prodKey locKey time soldToCustomer
    pure prodHistId


--   :: (PersistQueryRead backend, MonadIO m,
    --   BaseBackend backend ~ SqlBackend) =>
getProductsHistory :: Key Product -> DB [Entity ProductHistory]
getProductsHistory prodKey = selectList [ProductHistoryProduct ==. prodKey] []

-- myTodo how to type Handler as a specific type like 
-- Handler ProductHistory
data ProductHistoryAPI = ProductHistoryAPI [ProductHistory] 
    deriving stock Generic
    deriving anyclass FromJSON
    deriving anyclass ToJSON
    -- deriving ToTypedContent
    deriving Show 

getProductsHistoryHandler :: Handler Value
getProductsHistoryHandler = do 
    ProductKeyAPI {..} <- requireCheckJsonBody
    xs <- runDB $ getProductsHistory productKeyAPI
    -- return (ProductHistoryAPI $ map entityVal xs)
    returnJson xs






-- for customer to buy product from a store
    -- product must be at a store, not the wharehouse for example 
    -- customer must have enough money, and transfer the money from the customer to the store
    -- (would be kind of cool to emulate like the old way where the store has the cash and then its transferred to a central location like at end of day )
    -- product should be transferred from product history to customer history

-- myTodo refactor this to use probs MaybeT?
customerBuysFromStore :: Key Customer -> Key Store -> Key Product -> DB (Either String String)
customerBuysFromStore keyCust keyStore keyProd = do 
    mCustomer <- selectFirst [CustomerId ==. keyCust] []
    mStore <- selectFirst [StoreId ==. keyStore] []
    mProd <- selectFirst [ProductId ==. keyProd] []

    case (mCustomer, mStore, mProd) of
        (Just (Entity cId cust), Just (Entity sId store), Just (Entity pId prod)) -> do 
            -- if (balanceCustomer )
            
            
            pure (Right "it was transfered") 
        _ -> pure (Left "todo"    )    