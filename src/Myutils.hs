{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}


module Myutils where

import GHC.Generics
import Data.Fixed 
import Database.Persist.TH
import Prelude
import Data.Aeson
import Data.Fixed





data Departments = 	MusicDept | BooksDept | FoodDept | ClothesDept | FurnitureDept deriving (Show, Read, Eq, Generic)
derivePersistField "Departments"

instance ToJSON Departments where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Departments

data OrderStatus = Processing | InTransit | Delivered | Cancelled deriving (Show, Read, Eq, Generic)
derivePersistField "OrderStatus"

instance ToJSON OrderStatus where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON OrderStatus

data FoodTypes =  Beverage | Snack | Dinner deriving (Show, Read, Eq, Generic)
derivePersistField "FoodTypes"

instance ToJSON FoodTypes where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON FoodTypes

