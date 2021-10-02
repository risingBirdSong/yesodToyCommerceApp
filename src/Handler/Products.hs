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

data ABook = ABook {aBook :: Entity Book}
    deriving (Generic)

instance FromJSON ABook
instance ToJSON ABook

-- getStoreAquiresBookR :: Handler ()
-- getStoreAquiresBookR = do 
--     liftIO $ print "hitting" 
--     pure ()

-- so how can we abstract these two functions? Where they're mostly similar. Can we pass in the type and Constructor in as arguments?

-- the API version of book
data CreateBook = CreateBook
    { author :: Text
    , genre :: Text
    , pageCount :: Int
    , cost :: Int
    }
    deriving stock Generic
    deriving anyclass FromJSON

-- also consider the reverse where we convert the BD representation to the API representation

toBook :: CreateBook -> ProductId -> Book
toBook CreateBook {..} prodId = Book prodId author genre pageCount cost

data Products = Books Book | Foods Food

insertProduct :: (PersistEntity a, PersistEntityBackend a ~ SqlBackend) => (ProductId -> a) -> UTCTime -> DB ()
insertProduct f now = do
    prodId <- insert $ Product defaultLocationId now
    void $ insert (f prodId)
 where
    defaultLocationId = toSqlKey (1 :: Int64)

-- newtype Sum = Sum { unSum :: Int }

-- coerce 2 :: Sum
-- coerce (Sum 2) :: Int

-- requireCheckJsonBody :: forall a m. (MonadHandler m, FromJSON a) => m ()

accessSettings :: Handler ()
accessSettings = do 
    gotYesod <- getYesod
    let thesettings = appSettings gotYesod
    print $ appWharehouseLocation thesettings

postStoreAquiresBookR :: Handler Value 
postStoreAquiresBookR = do
    -- :: type annotation
    -- @ type application
    let defaultLocationId = toSqlKey (1 :: Int64)
    theTime <- getCurrentTime
    apiBook <- requireCheckJsonBody
    _ <- runDB $ insertProduct (toBook apiBook) theTime
    _ <- runDB $ do
        prodId <- insert $ Product defaultLocationId theTime 
    --let theNewBook = Book aNewProduct  (partialBookWithLocationAuthor partialbook) (partialBookWithLocationGenre partialbook) (partialBookWithLocationPageCount partialbook) (partialBookWithLocationCost partialbook)
        insert $ toBook apiBook prodId
    print "book inserted"
    sendResponseStatus status201 ("BOOK stocked in store" :: Text)

postStoreAquiresFoodR :: Handler Value 
postStoreAquiresFoodR = do
    theTime <- getCurrentTime
    partialFood :: PartialFoodWithLocation <- requireCheckJsonBody 
    aNewProduct <- runDB $ insert $ Product (partialFoodWithLocationLocationId partialFood) theTime
    let theNewFood = Food aNewProduct (partialFoodWithLocationFoodType partialFood) (partialFoodWithLocationBrand partialFood) (partialFoodWithLocationName partialFood) (partialFoodWithLocationWeight partialFood) (partialFoodWithLocationDescription partialFood)
    insertedbook <- runDB $ insert $ theNewFood 
    print "food inserted"
    sendResponseStatus status201 ("FOOD stocked in store" :: Text)


