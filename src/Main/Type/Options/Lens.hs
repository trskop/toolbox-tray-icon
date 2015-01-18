{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Lenses for Options data type.
-- Copyright:    (c) 2015 Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Lenses for 'Options' data type.
module Main.Type.Options.Lens (configFile)
  where

import Control.Lens (Lens', lens)
import System.IO (FilePath)

import Main.Type.Options (Options)
import qualified Main.Type.Options as Options (configFile)


-- | Configuration file path.
configFile :: Lens' Options FilePath
configFile = Options.configFile `lens` \s b -> s{Options.configFile = b}
