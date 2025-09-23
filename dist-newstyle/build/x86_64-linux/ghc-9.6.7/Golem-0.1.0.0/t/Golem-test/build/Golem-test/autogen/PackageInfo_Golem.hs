{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_Golem (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "Golem"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "A typesetting EDSL in Haskell that produces LaTeX"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
