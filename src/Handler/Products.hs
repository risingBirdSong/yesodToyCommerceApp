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
-- import Control.Monad.Fail









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
    deriving stock (Show, Generic)
    deriving anyclass FromJSON

-- instance FromJSON CreateBook
-- instance ToJSON CreateBook

-- newtype Sum = Sum { getSum :: Int }
--     deriving newtype FromJSON -- "1"

--     deriving stock Generic
--     deriving anyclass FromJSON -- "{ 'getSum': '1' }"

-- class FromJSON a where
--     parseJSON :: a -> Value
--     parseJSON = genericParseJSON

-- class IsSum a where
--     isSum :: Bool
--     isSum = True

-- (genRandom FK.author) (genRandom FK.genre) (randomRIO (50, 500) :: IO Int) (randomRIO (5, 50) :: IO Int)

-- TODO maybe try to pull the specific generateFakeBook and generateFakeFood into a generic helper function? 

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

-- insertProduct :: (PersistEntity a, PersistEntityBackend a ~ SqlBackend) => (ProductId -> a) -> UTCTime -> Int64  ->  DB ()
insertProduct :: (PersistEntity a, PersistEntityBackend a ~ SqlBackend) => (ProductId -> a) -> ProductTypes -> UTCTime -> Key StockLocation  ->  DB ()
insertProduct f productType now locationId = do
    prodId <- insert $ Product productType locationId now
    void $ insert (f prodId)

insertProductBy f productTypes now locationId = do
    prodId <- insert $ Product productTypes locationId now
    void $ insertBy (f prodId)

-- fakeHandlerR :: do
--     runDB $ do
--         makeMainWharehouse
--         makeAStore
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


-- was like this when using toSqlKey... so in API types the data were all INT64 instead of being example Key Product 
-- getLocationsInventoryR :: Handler Value
-- getLocationsInventoryR = do
--     locationId <- lookupGetParam "locationid"
--     case locationId of
--         Nothing -> sendResponseStatus status404 ("that location was not found" :: Text)
--         Just locId -> do
--             case decimal locId of
--                 Left fail -> sendResponseStatus status404 ("we couldnt parse that number" :: Text)
--                 Right (theint, _) -> do
--                     productsAtLocation <- runDB $ selectList [ProductStockLocationId ==. theint] []
--                     returnJson productsAtLocation

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
        

-- data TransferProdLocationFromAToBJson = TransferProdLocationFromAToBJson {
-- -- so both these needs to be used with toSqKey 
--         productId :: Key Product ,
--         transferOrigin :: Key LocationId , 
--         transferDestination :: Key LocationId
--     }
--  we might be able to derive stock for TransferProdLocationFromAToBJson with these more specific keys, but if not...
-- instance FromJSON TransferProdLocationFromAToBJson where
--   parseJSON = withObject "TransferProdLocationFromAToBJson" $ \v -> TransferProdLocationFromAToBJson
--      <$> (toSqlKey <$> o .: "productId")
--      <*> (toSqlKey <$> o .: "transferOrigin")
-- ...


data TransferProdLocationFromAToBJson = TransferProdLocationFromAToBJson {
-- so both these needs to be used with toSqKey 
        productId :: Key Product ,
        transferOrigin :: Key StockLocation , 
        transferDestination :: Key StockLocation
    }
    deriving stock Generic
    deriving anyclass FromJSON
    deriving Show

-- productAtLocation :: (MonadIO m, PersistQueryRead backend, BaseBackend backend ~ SqlBackend) =>
--      Int64 -> Int64 -> ReaderT backend m (Maybe (Entity Product))

-- productAtLocation :: Key Product -> Key TransferOrigin -> DB (Maybe (Entity Product))
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
    -- could just return the json, probs not that ig of a deal
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


postTransferAProdFromLocAtoB_ValidationR :: Handler () 
postTransferAProdFromLocAtoB_ValidationR = do 
    transferProdLocationFromAToBJson <- requireCheckJsonBody
    res <- runDB $ verifyAndUpdateLocation transferProdLocationFromAToBJson
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

validationExampeA = maybeToValidation (pack "error a") (Just 1) <*
                    maybeToValidation (pack "error b") (Just 2) <*
                    maybeToValidation (pack "error c") (Just 3) <*
                    maybeToValidation (pack "error d") (Just 4) 
                    -- Success 1


validationExampeB = maybeToValidation (pack " error a ") (Nothing) <*
                    maybeToValidation (pack " error b ") (Nothing) <*
                    maybeToValidation (pack " error c ") (Just 3)  <*
                    maybeToValidation (pack " error d ") (Nothing) 
                    -- Failure " error a  error b error d "

-- maybeToValidationA = 

-- ensureProdAtLocationAndDestination :: 
-- ensureProdAtLocationAndDestination productId transferOrigin = 
--     case productAtLocation productId transferOrigin of
--         Nothing -> _Failure # (pack "prod was not at location")
--         Just val -> _Success # (val)




postTransferAProdFromLocAtoB_MaybeTR :: Handler Value
postTransferAProdFromLocAtoB_MaybeTR = do
    TransferProdLocationFromAToBJson {..} <- requireCheckJsonBody
    mSuccess  <- runDB $ runMaybeT $ do 
        ensureProdLoc <- MaybeT $ productAtLocation productId transferOrigin
        ensureDestinationExists <- MaybeT $ selectFirst [StockLocationId ==. transferDestination] []
        -- pure updated
        pure (ensureProdLoc, ensureDestinationExists)
    case mSuccess of
        Nothing -> sendResponseStatus status400 ("check validation" :: Text)
        -- Just (ensureProdLoc, ensureDestinationExists , origin, destination) -> do
        Just _ -> do
            myb <- runDB $ runMaybeT $ do
                mybNameOfOrigin <- MaybeT $ selectFirst [StockLocationId ==. transferOrigin] []
                mybNameOfDestination <- MaybeT $ selectFirst [StockLocationId ==. transferDestination] []
                pure (mybNameOfOrigin, mybNameOfDestination)
            case myb of
                Just ((Entity _ nameOfOrigin), (Entity _ nameOfDestination)) -> runDB $ do 
                    updated <- updateWhere [ProductId ==. productId] [ProductStockLocationId =. transferDestination]
                    sendResponseStatus status201 ("transfered" :: Text)
                _ -> sendResponseStatus status400 ("something went wrong" :: Text)



-- postTransferAProdFromLocAtoB_MaybeR :: Handler Value
-- postTransferAProdFromLocAtoB_MaybeR = do
--     TransferProdLocationFromAToBJson {..} <- requireCheckJsonBody
--     mSuccess :: Either [String] a  <- runDB $ runValidateT $ do 
--         disputeNothing "prod was not found" $ productAtLocation productId transferOrigin
--         m a >>= (a -> Either l b)

        -- eStatus <- runValidateT $ do my validations
        -- case eStatus of
        --   Left err -> Left err
        --   Right _ -> Right <$> ... other sql stuff

    -- f :: IO String
    -- f = do
    --   putStrLn "Hey" :: IO ()
    --   pure "My String" :: IO String -- m a >>= (a -> m b) -- (>>) -- m a >>= (\() -> m b)

    -- newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
    -- newtype ValidateT e m a = ValidateT { getValidateT :: forall s. StateT (MonoMaybe s e) (ExceptT e m) a }
    -- nameOfOrigin <- ValidateT $ selectFirst [StockLocationId ==. (toSqlKey transferOrigin)] []


    --     ensureDestinationExists <- MaybeT $ selectFirst [StockLocationId ==. (toSqlKey transferDestination)] []
        -- nameOfOrigin <- pure $ selectFirst [StockLocationId ==. (toSqlKey transferOrigin)] []
        -- nameOfOrigin <- $ selectFirst [StockLocationId ==. (toSqlKey transferOrigin)] []
        -- nameOfOrigin <- refuteNothing $ selectFirst [StockLocationId ==. (toSqlKey transferOrigin)] []

    --     nameOfDestination <- MaybeT $ selectFirst [StockLocationId ==. (toSqlKey transferDestination)] []
    --     pure (ensureProdLoc, ensureDestinationExists, nameOfOrigin, nameOfDestination)
    -- case mSuccess of
    --     Nothing -> sendResponseStatus status400 ("something went wrong" :: Text)
    --     Just (_,_,origin, destination) ->  pure $ object ["status" .= ("success" :: Text), "origin" .= origin, "destination" .= destination]

-- disputeNothing :: (MonadValidate e m) => e -> Maybe a -> m ()
-- disputeNothing err mVal = case mVal of
--     Nothing -> dispute err -- refute :: e -> m a
--     Just _ -> pure ()

-- refuteNothing :: (MonadValidate e m) => e -> Maybe a -> m a
-- refuteNothing err = fromMaybe (refute err)


-- (<*>) :: m (a -> b) -> m a -> m b
-- (<*>) mf ma = do
    -- eval mf
    -- eval ma
    -- refute 1 *> refute 2
    -- f a else Left errs

-- disputeNothing returns a ValidateT
-- disputeNothing take in an error message and a maybe, and case on the maybe
-- Nothing is refuted
-- Just is pured 



 
    -- MaybeT 
    -- fusion in Haskell works because of purity, but in a runDB with sql were working with effects and therefore fusion is much more difficult not necc. desired
    -- just put the case of inside the runDB not outside 
    -- void $ runDB $ updateWhere [ProductId ==. (toSqlKey productId)] [ProductStockLocationId =. (toSqlKey transferLocation)]

-- batch transfer, transferring a list of products from one location to another (like an equivalent for a truckload delivery from wharehouse to store)
data TransferListProdLocationFromAToBJson = TransferListProdLocationFromAToBJson {
-- so both these needs to be used with toSqKey 
        productIds :: [Int64] ,
        transferOriginForList :: Int64 ,
        transferDestinationForList :: Int64
    }
    deriving stock Generic
    deriving anyclass FromJSON
    deriving Show
-- is there a better way to handle this batch update? it seems ok and I didn't see a custom function for it
postTransferListProdFromLocAtoBR :: Handler Value 
postTransferListProdFromLocAtoBR = do 
    transferList :: TransferListProdLocationFromAToBJson <- requireCheckJsonBody
    let TransferListProdLocationFromAToBJson {..} = transferList

    attemptedUpdates <- runDB $ do
        mapM (\prodId -> updateWhere [ProductId ==. (toSqlKey prodId)] [ProductStockLocationId =. (toSqlKey transferDestinationForList)]) productIds
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

