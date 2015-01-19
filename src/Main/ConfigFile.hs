{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Read and parse configuration files.
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Read and parse configuration files.
module Main.ConfigFile
    ( defaultConfigFileName
    , readConfigFile
    , readConfigFile'
    )
  where

import Control.Monad (Monad(return), (=<<))
import Data.Bool (otherwise)
import Data.Either (Either)
import Data.Eq (Eq((==)))
import Data.Function ((.))
import Data.Functor (Functor(fmap))
import Data.String (String)
import System.IO (IO, FilePath)

import Data.ByteString (readFile)

import Control.Lens ((^.))
import Data.Aeson (eitherDecodeStrict')

import Main.Type.Options (Options)
import Main.Type.Options.Lens (configFile)
import Main.Type.MenuItem (MenuItems)


-- | Default configuration file name.
--
-- @
-- 'defaultConfigFileName' = \"toolbox-menu.json\"
-- @
defaultConfigFileName :: FilePath
defaultConfigFileName = "toolbox-menu.json"

-- | Read configuration file either one provided with the installation or one
-- supplied by the user on command line.
readConfigFile
    :: (FilePath -> IO FilePath)
    -- ^ Get absoulete file path for relative path to a config file. This is
    -- where 'Paths.toolbox.getDataFileName' would be used.
    -> Options
    -> IO (Either String MenuItems)
readConfigFile f opts = readConfigFile' =<< case opts ^. configFile of
    fileName@(c : _)
      | c == '/'  -> return fileName
        -- Leave absolute path as it is.
      | otherwise -> f fileName
        -- Find config file using provided funcion in case of relative path.
    ""            -> f defaultConfigFileName
        -- Empty path means "use default configuration file."

-- | Read and parse configuration file (JSON).
readConfigFile' :: FilePath -> IO (Either String MenuItems)
readConfigFile' = fmap eitherDecodeStrict' . readFile
