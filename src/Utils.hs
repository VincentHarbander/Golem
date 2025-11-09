{-# LANGUAGE OverloadedStrings #-}

module Utils (usepackageM,
              usepackage,
              equationM,
              equation,
              equationM',
              equation',
              labelM,
              label,
              refM,
              ref,
              commentM,
              comment,
              stdoutWith,
              stdout,
              splitM,
              split,
              figureM,
              figure
              ) where

import Data.Text (Text)
import Data.Text.IO as TextIO
import Core


usepackageM :: (Monoid a, Applicative m) => NormalTextT m a
usepackageM = commandM "usepackage"

usepackage :: (Applicative m) => NormalTextT m ()
usepackage = usepackageM

equationM :: (Monoid a, Applicative m) => MathTextT m a -> NormalTextT m a
equationM = mscopeM "equation"

equation :: (Applicative m) => MathTextT m () -> NormalTextT m ()
equation = equationM

equationM' :: (Monoid a, Applicative m) => MathTextT m a -> NormalTextT m a
equationM' = mscopeM "equation*"

equation' :: (Applicative m) => MathTextT m () -> NormalTextT m ()
equation' = equationM'

labelM :: (Monoid a, Monad m, TextPack t m) => Text -> t m a
labelM t = cast $ commandM "label" .> textM t

label :: (Monad m, TextPack t m) => Text -> t m ()
label = labelM

refM :: (Monoid a, Monad m) => Text -> NormalTextT m a
refM t = commandM "ref" .> textM t

ref :: (Monad m) => Text -> NormalTextT m ()
ref = refM

commentM :: (Monoid a, Applicative m, TextPack t m) => Text -> t m a
commentM t = fromTextM $ "% " <> t

comment :: (Applicative m, TextPack t m) => Text -> t m ()
comment = commentM


stdoutWith :: (Monad m) => (m Document -> IO Document) -> m Document -> IO ()
stdoutWith runner mdoc = do
  Document t <- runner mdoc
  TextIO.putStrLn t

stdout :: Document -> IO ()
stdout (Document t) = TextIO.putStrLn t

splitM :: (Monoid a, Monad m) => [MathTextT m a] -> NormalTextT m a
splitM = scopeM "align" . mscopeM "split" . helper
 where
  helper [] = pure mempty
  helper [x] = x
  helper (x : y : xs) = do
    a <- x
    fromText " \\\\\n"
    b <- helper $ y : xs
    return $ a <> b

split :: (Monad m) => [MathTextT m ()] -> NormalTextT m ()
split = splitM

figureM :: (Monoid a, Monad m) => Text -> Text -> Text -> NormalTextT m a
figureM arg scale filename =
  scopeWithM arg "figure" $
    command "centering"
      >> (commandM "includegraphics" ?> textM ("scale=" <> scale) .> textM filename)

figure :: (Monad m) => Text -> Text -> Text -> NormalTextT m ()
figure = figureM
