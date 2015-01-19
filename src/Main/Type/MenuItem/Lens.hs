{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Lenses for types defined in Main.Type.MenuItem module.
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Lenses for types defined in "Main.Type.MenuItem" module.
module Main.Type.MenuItem.Lens
    ( menuItems
    , id
    , name
    , description
    , command
    )
  where

import Data.Functor ((<$>))
import Data.String (String)

import Control.Lens (Lens', lens)

import Main.Type.MenuItem (MenuItem, MenuItems(MenuItems))
import qualified Main.Type.MenuItem as MenuItem
    ( command
    , description
    , id
    , name
    )


id :: Lens' MenuItem String
id = MenuItem.id `lens` \s b -> s{MenuItem.id = b}

name :: Lens' MenuItem String
name = MenuItem.name `lens` \s b -> s{MenuItem.name = b}

description :: Lens' MenuItem String
description = MenuItem.description `lens` \s b -> s{MenuItem.description = b}

command :: Lens' MenuItem String
command = MenuItem.command `lens` \s b -> s{MenuItem.command = b}

menuItems :: Lens' MenuItems [MenuItem]
menuItems f (MenuItems as) = MenuItems <$> f as
