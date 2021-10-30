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
toBook CreateBook {..} prodId = Book prodId title author genre pageCount cost


-- the API version of book
data CreateBook = CreateBook
    { title :: Text
    , author :: Text
    , genre :: Text
    , pageCount :: Int
    , cost :: Int
    }
    deriving stock (Show, Generic)
    deriving anyclass FromJSON


-- consider placing the random logic into monadRandom , more restricted
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
insertProduct :: (PersistEntity a, PersistEntityBackend a ~ SqlBackend) => (ProductId -> a) -> ProductTypes -> UTCTime -> Key StockLocation  ->  DB ()
insertProduct f productType now locationId = do
    prodId <- insert $ Product productType locationId now
    void $ insert (f prodId)

insertProductBy f productTypes now locationId = do
    prodId <- insert $ Product productTypes locationId now
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

postWharehouseAquiresBookR :: Handler Text 
postWharehouseAquiresBookR = do
    gotYesod <- getYesod
    let whareHouseId = appWharehouseLocation . appSettings $ gotYesod
    theTime <- getCurrentTime
    apiBook :: CreateBook <- requireCheckJsonBody
    _ <- runDB $ insertProduct (toBook apiBook) BookProduct theTime whareHouseId
    sendResponseStatus status201 ("BOOK stocked in store" :: Text)

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

postTransferAProdFromLocAtoB_R :: Handler Value
postTransferAProdFromLocAtoB_R = do
    TransferProdLocationFromAToBJson {..} <- requireCheckJsonBody
    returnmessage :: Either Text Text <- runDB $ do 
        ensureProdLoc <- productAtLocation productId transferOrigin
        ensureDestinationExists <- selectFirst [StockLocationId ==. transferDestination] []
        nameOfOrigin <- selectFirst [StockLocationId ==. transferOrigin] []
        nameOfDestination <- selectFirst [StockLocationId ==. transferDestination] []
        case (ensureProdLoc, ensureDestinationExists,  nameOfOrigin, nameOfDestination) of 
            (Just prod, Just (destExist) , Just (Entity _ nameOrigin), Just (Entity _ nameDest)) -> do 
                updated <- updateWhere [ProductId ==. productId] [ProductStockLocationId =. transferDestination]
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

verifyAndUpdateLocationWithRegularArgs :: Key Product -> Key StockLocation -> Key StockLocation -> UTCTime -> DB (Either Text Text)
verifyAndUpdateLocationWithRegularArgs productId transferOrigin transferDestination thetime = do 
    ensureProdLoc <- productAtLocation productId transferOrigin
    ensureDestinationExists <- selectFirst [StockLocationId ==. transferDestination] []
    case validateProdAtLocationAndDestinationExists ensureProdLoc ensureDestinationExists of
        Vld.Failure errs -> do
            pure $ Left errs 
        Vld.Success _ -> do 
            inserted <- insert $ ProductHistory productId transferDestination thetime False
            updated <- updateWhere [ProductId ==. productId, ProductStockLocationId ==. transferOrigin] [ProductStockLocationId =. transferDestination]
            pure $ Right "updated"

postTransferAProdFromLocAtoB_ValidationR :: Handler () 
postTransferAProdFromLocAtoB_ValidationR = do 
    thetime <- liftIO $ getCurrentTime
    TransferProdLocationFromAToBJson {..} <- requireCheckJsonBody
    res <- runDB $ verifyAndUpdateLocationWithRegularArgs productId transferOrigin transferDestination thetime
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
postTransferListProdFromLocAtoBR :: Handler Value 
postTransferListProdFromLocAtoBR = do 
    transferList :: TransferListProdLocationFromAToBJson <- requireCheckJsonBody
    let TransferListProdLocationFromAToBJson {..} = transferList
    thetime <- liftIO getCurrentTime
    attemptedUpdates <- runDB $ do
        mapM (\prodId -> verifyAndUpdateLocationWithRegularArgs prodId transferOriginForList transferDestinationForList thetime) productIds
    return $ object ["attemptedUpdates" .= attemptedUpdates]

-- myTodo updateWhere returns () but maybe there should be a version that returns whether it updated or not
-- the function to do this is updateWhereCount 

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



