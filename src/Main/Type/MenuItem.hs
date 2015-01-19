{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main.Type.MenuItem
    (MenuItem(..), MenuItems(..))
  where

import Data.Data (Data)
import Data.Function (($))
import Data.Monoid (Monoid(mempty, mappend))
import Data.String (String)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Text.Show (Show)

import Data.Aeson (FromJSON, ToJSON)


data MenuItem = MenuItem
    { id          :: String
    , name        :: String
    , description :: String
    , command     :: String
    }
  deriving (Data, Generic, Show, Typeable)

instance FromJSON MenuItem
instance ToJSON MenuItem

newtype MenuItems = MenuItems [MenuItem]
  deriving (Data, Generic, Show, Typeable)

instance FromJSON MenuItems
instance ToJSON MenuItems

instance Monoid MenuItems where
    mempty = MenuItems []
    MenuItems x `mappend` MenuItems y = MenuItems $ x `mappend` y
