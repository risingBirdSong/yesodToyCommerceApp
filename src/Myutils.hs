{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}


module Myutils where

import GHC.Generics
import Data.Fixed 
import Database.Persist.TH
import Prelude
import Data.Aeson
import Data.Fixed





data Departments = 	MusicDept | BooksDept
	| FoodDept
	| FlothesDept
	| FurnitureDept deriving (Show, Read, Eq, Generic)
derivePersistField "Departments"

instance ToJSON Departments where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Departments


data FoodTypes =  Beverage | Snack | Dinner deriving (Show, Read, Eq, Generic)
derivePersistField "FoodTypes"

instance ToJSON FoodTypes where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON FoodTypes


newtype Dollar = Dollar {unDollar :: Centi} deriving (Show, Read, Eq, Generic)
derivePersistField "Dollar"

instance ToJSON Dollar where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Dollar

-- wait how to use like this?
-- newtype Grams p = Grams (Fixed p) deriving (Show, Read, Eq, Generic)
-- -- derivePersistField "Grams"

-- grams = Grams ( 1 :: Milli)
-- actually this prbably bad idea 
-- https://lexi-lambda.github.io/blog/2020/11/01/names-are-not-type-safety/

newtype Milligrams = Milligrams {unMilligrams :: Int} deriving (Show, Read, Eq, Generic)
derivePersistField "Milligrams"


instance ToJSON Milligrams where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Milligrams

newtype Grams = Grams {unGrams :: Int} deriving (Show, Read, Eq, Generic)
derivePersistField "Grams"

instance ToJSON Grams where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Grams
