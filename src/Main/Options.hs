{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Parsing of command line options
-- Copyright:    (c) 2015 Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Parsing of command line options and utility functions for
-- <http://hackage.haskell.org/package/optparse-applicative optparse-applicative>
-- package.
module Main.Options
    ( optionsParser
    , execParser
    )
  where

import Control.Applicative (Applicative((<*>)))
import Control.Monad.Trans.Identity (IdentityT(IdentityT, runIdentityT))
import Data.Either (Either(Left, Right))
import Data.Function ((.), ($))
import Data.Functor ((<$>))
import Data.List (null)
import Data.Maybe (Maybe)
import Data.Monoid (Monoid(mempty), (<>))
import Data.String (String)
import System.IO (IO, FilePath)

import Control.Lens (set)
import Data.Default.Class (Default(def))
import Data.Monoid.Endo (E, runEndo)
import Data.Monoid.Endo.Fold ((<&$>), foldEndo)
import Options.Applicative
    ( InfoMod
    , Mod
    , OptionFields
    , Parser
    , ReadM
    , eitherReader
    , execParserPure
    , handleParseResult
    , help
    , info
    , long
    , metavar
    , option
    , optional
    , prefs
    , short
    )

import Main.Type.Options (Options)
import Main.Type.Options.Lens (configFile, iconFile)


-- | Type alias for wrapped 'Parser'. It is used so to avoid orphan instances.
type Parser' = IdentityT Parser

parser' :: Parser a -> Parser' a
parser' = IdentityT

fromParser' :: Parser' a -> Parser a
fromParser' = runIdentityT

optionalOption :: ReadM a -> Mod OptionFields a -> Parser' (Maybe a)
optionalOption r = parser' . optional . option r

-- | Alternative to 'Options.Applicative.Extra.execParser' from
-- <http://hackage.haskell.org/package/optparse-applicative optparse-applicative>
-- package.
execParser :: Parser a -> InfoMod a -> [String] -> IO a
execParser parser infoMod =
    handleParseResult . execParserPure (prefs mempty) (info parser infoMod)

-- | Parser for command line options.
optionsParser :: Parser Options
optionsParser = fromParser' $ runEndo def <&$> foldEndo
    <*> configFileOption
    <*> iconFileOption

-- | Parse @--config=FILE@ option.
configFileOption :: Parser' (Maybe (E Options))
configFileOption = optionalOption (set configFile <$> parseFilePath)
    $ short 'c' <> long "config" <> metavar "FILE"
    <> help "Use specified FILE instead of default configuration file."

-- | Parse @--icon=FILE@ option.
iconFileOption :: Parser' (Maybe (E Options))
iconFileOption = optionalOption (set iconFile <$> parseFilePath)
    $ short 'i' <> long "icon" <> metavar "FILE"
    <> help "Instead of default icon use FILE."

-- | Parse file path argument to an option.
parseFilePath :: ReadM FilePath
parseFilePath = eitherReader $ \s ->
    if null s
        then Left "Option argument can not be empty file path."
        else Right s
