{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Options data type describing parsed options.
-- Copyright:    (c) 2015 Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- 'Options' data type describing parsed options.
module Main.Type.Options (Options(..))
  where

import System.IO (FilePath)
import Text.Show (Show)


data Options = Options {configFile :: FilePath}
  deriving (Show)
