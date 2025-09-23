{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Writer.Strict (Writer, runWriter, tell)
import Data.Text as Text (Text, words)
import Golem

newtype MInt = MInt Int

instance Semigroup MInt where
  (MInt a) <> (MInt b) = MInt (a + b)

instance Monoid MInt where
  mempty = MInt 0

-- First document
preamble :: NormalText'
preamble = do
  comment "This is a cool comment that is also within a do-block!"
  newP
  usepackage .> text "amsmath"
  usepackage .> text "graphicx"
  usepackage .> text "float"

content :: NormalText'
content =
  text "Lorem ipsum."
    # text "Let"
    # inline (math "x = 0")
    >< text "."
    # text "Look at this cool equation!"
    # text "I call it equation"
    # ref "coolio"
    >< text "."
    % equation eq
    % coolParagraphs
    % split [math "x = x", math "x < y < z"]
    % figure "H" "0.1" "apple.png"
 where
  eq = math "(x + 1) + 0 = x + (1 + 0) = x + 1" % label "coolio"
  coolParagraphs = do
    text "Wow! This is even more text. I could write about whatever here."
    newP
    text "This is a new paragraph following the previous one! Check out the stuff below!"

document :: Document
document =
  defaultArticle
    "First document"
    "Vincent Harbander"
    "September 2025"
    preamble
    content

-- Second document
preamble2 :: NormalTextT (Writer MInt) ()
preamble2 = usepackage .> text "graphicx"

logger :: Writer MInt a -> IO a
logger wr = do
  let (doc, MInt n) = runWriter wr
  writeFile "totalCharactersIn2.txt" $ show n
  return doc

mdoc :: Writer MInt Document
mdoc = defaultArticleT "Second document" "Other person" "January 1960" preamble2 mcont

text' :: Text -> NormalTextT (Writer MInt) ()
text' t = do
  lift . tell . MInt . length $ Text.words t
  text t

mcont :: NormalTextT (Writer MInt) ()
mcont = do
  text' "Hello, I need to count these words!"
  newP
  text' "It should be 12 words."

main :: IO ()
main = do
  compile "output.tex" document
  compileWith logger "output2.tex" mdoc
  stdoutWith logger mdoc
