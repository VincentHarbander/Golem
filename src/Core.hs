{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Core (
  NormalTextT,
  NormalText,
  NormalText',
  MathTextT,
  MathText,
  MathText',
  Document,
  unDocument,
  fromTextM,
  fromText,
  textM,
  text,
  mathM,
  math,
  fromMathM,
  fromMath,
  inlineM,
  inline,
  scopeWithM,
  scopeWith,
  scopeM,
  scope,
  mscopeWithM,
  mscopeWith,
  mscopeM,
  mscope,
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
  putBetween,
  (.>),
  (?>),
  (#),
  (><),
  (&>),
  (<&),
  newL,
  newP,
  bsM,
  bs,
  defaultArticle,
  defaultArticleT,
  commandM,
  command,
  mcommandM,
  mcommand,
  usepackageM,
  usepackage,
  cast,
  GolemText (..),
  compile,
  compileWith,
  stdout,
  stdoutWith,
  splitM,
  split,
  figureM,
  figure,
) where

import Control.Applicative (liftA2)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Writer.Strict (WriterT (..), runWriterT)
import Data.Bifunctor (second)
import Data.Text (Text, unpack)
import Data.Text.IO as TextIO

type NormalText' = NormalText ()
type NormalText a = NormalTextT Identity a
newtype NormalTextT m a = NormalTextT {unNormalTextT :: WriterT Text m a}
type MathText' = NormalText ()
type MathText a = MathTextT Identity a
newtype MathTextT m a = MathTextT {unMathTextT :: WriterT Text m a}
newtype Document = Document {unDocument :: Text}

instance (Functor m) => Functor (NormalTextT m) where
  fmap f (NormalTextT w) = NormalTextT $ fmap f w

instance (Monad m) => Applicative (NormalTextT m) where
  pure = NormalTextT . pure
  (NormalTextT mf) <*> (NormalTextT ma) = NormalTextT $ mf <*> ma

instance (Monad m) => Monad (NormalTextT m) where
  return = pure
  (NormalTextT ma) >>= f = NormalTextT $ do
    a <- ma
    let NormalTextT mb = f a
    mb

instance (Functor m) => Functor (MathTextT m) where
  fmap f (MathTextT w) = MathTextT $ fmap f w

instance (Monad m) => Applicative (MathTextT m) where
  pure = MathTextT . pure
  (MathTextT mf) <*> (MathTextT ma) = MathTextT $ mf <*> ma

instance (Monad m) => Monad (MathTextT m) where
  return = pure
  (MathTextT ma) >>= f = MathTextT $ do
    a <- ma
    let MathTextT mb = f a
    mb

instance MonadTrans NormalTextT where
  lift = fromTextPack . fmap (,"")

instance MonadTrans MathTextT where
  lift = fromTextPack . fmap (,"")

instance (Show a) => Show (NormalTextT Identity a) where
  show = show . second unpack . runIdentity . toTextPack

instance (Show a) => Show (MathTextT Identity a) where
  show = show . second unpack . runIdentity . toTextPack

instance (Semigroup a, Applicative m) => Semigroup (NormalTextT m a) where
  (NormalTextT w) <> (NormalTextT w') = NormalTextT $ liftA2 (<>) w w'

instance (Monoid a, Monad m) => Monoid (NormalTextT m a) where
  mempty = pure mempty

instance (Semigroup a, Applicative m) => Semigroup (MathTextT m a) where
  (MathTextT w) <> (MathTextT w') = MathTextT $ liftA2 (<>) w w'

instance (Monoid a, Monad m) => Monoid (MathTextT m a) where
  mempty = pure mempty

-- Text with data
class TextPack t m where
  fromTextPack :: m (a, Text) -> t m a
  toTextPack :: t m a -> m (a, Text)

instance TextPack NormalTextT m where
  fromTextPack = NormalTextT . WriterT
  toTextPack = runTextT

instance TextPack MathTextT m where
  fromTextPack = MathTextT . WriterT
  toTextPack = runTextT

cast :: (TextPack t m, TextPack t' m) => t m a -> t' m a
cast = fromTextPack . toTextPack

fromTextM :: (Monoid a, Applicative m, TextPack t m) => Text -> t m a
fromTextM = fromTextPack . pure . (mempty,)

fromText :: (Applicative m, TextPack t m) => Text -> t m ()
fromText = fromTextM

-- Pass regular argument
infixl 4 .>
(.>) :: (Monoid a, Applicative m, TextPack t m, TextPack t' m, Semigroup (t m a)) => t m a -> t' m a -> t m a
t .> x = t <> fromTextM "{" <> cast x <> fromTextM "}"

-- Pass optional argument
infixl 4 ?>
(?>) :: (Monoid a, Applicative m, TextPack t m, TextPack t' m, Semigroup (t m a)) => t m a -> t' m a -> t m a
t ?> x = t <> fromTextM "[" <> cast x <> fromTextM "]"

-- Separate two texts with something
putBetween :: (Monoid a, Applicative m, Applicative (t m), TextPack t m, Semigroup (t m a)) => Text -> t m a -> t m a -> t m a
putBetween phrase a b = a <> fromTextM phrase <> b

-- Space between
infixl 3 #
(#) :: (Monoid a, Applicative m, Applicative (t m), TextPack t m, Semigroup (t m a)) => t m a -> t m a -> t m a
(#) = putBetween " "

-- Nothing between
infixl 3 ><
(><) :: (Monoid a, Applicative m, Applicative (t m), TextPack t m, Semigroup (t m a)) => t m a -> t m a -> t m a
(><) = putBetween ""

infix 4 &>
(&>) :: (GolemText t, Monad m) => t m () -> t m a -> t m a
twmbtm &> twma = fromWriterT $ toWriterT twmbtm >> toWriterT twma

infix 5 <&
(<&) :: (GolemText t, Monad m) => t m a -> t m () -> t m a
twma <& twmbtm = fromWriterT $ do
  a <- toWriterT twma
  toWriterT twmbtm
  return a

bsM :: (Monoid a, Applicative m, TextPack t m) => t m a
bsM = fromTextM "\\"

bs :: (Applicative m, TextPack t m) => t m ()
bs = bsM

commandM :: (Monoid a, Applicative m) => Text -> NormalTextT m a
commandM t = fromTextM $ "\\" <> t

command :: (Applicative m) => Text -> NormalTextT m ()
command = commandM

mcommandM :: (Monoid a, Applicative m) => Text -> MathTextT m a
mcommandM t = cast $ commandM t

mcommand :: (Applicative m) => Text -> MathTextT m ()
mcommand = mcommandM

-- New line
newL :: (Applicative m, TextPack t m) => t m ()
newL = fromText "\n"

-- Double new line in NormalTextT, but new line in MathTextT
newP :: (Applicative m, Applicative (t m), TextPack t m, Semigroup (t m ()), GolemText t) => t m ()
newP = pure () % pure ()

infixl 3 %
class GolemText t where
  (%) :: (Monoid a, Applicative m, Applicative (t m), TextPack t m, Semigroup (t m a)) => t m a -> t m a -> t m a
  fromWriterT :: WriterT Text m a -> t m a
  toWriterT :: t m a -> WriterT Text m a
  runTextT :: t m a -> m (a, Text)

instance GolemText NormalTextT where
  (%) = putBetween "\n\n"
  fromWriterT = NormalTextT
  toWriterT = unNormalTextT
  runTextT = runWriterT . unNormalTextT

instance GolemText MathTextT where
  (%) = putBetween "\n"
  fromWriterT = MathTextT
  toWriterT = unMathTextT
  runTextT = runWriterT . unMathTextT

textM :: (Monoid a, Monad m) => Text -> NormalTextT m a
textM = fromTextM

text :: (Monad m) => Text -> NormalTextT m ()
text = textM

mathM :: (Monoid a, Monad m) => Text -> MathTextT m a
mathM = fromTextM

math :: (Monad m) => Text -> MathTextT m ()
math = mathM

fromMathM :: (Monoid a, Applicative m) => MathTextT m a -> NormalTextT m a
fromMathM t = commandM "mathrm" .> t

fromMath :: (Applicative m) => MathTextT m () -> NormalTextT m ()
fromMath = fromMathM

-- Inline math
inlineM :: (Monad m) => MathTextT m a -> NormalTextT m a
inlineM t = fromTextM "$" &> cast t <& fromTextM "$"

inline :: (Monad m) => MathTextT m () -> NormalTextT m ()
inline = inlineM

scopeWithM :: (Monoid a, Applicative m) => Text -> Text -> NormalTextT m a -> NormalTextT m a
scopeWithM arg scopeName t =
  fromTextM "\\begin{"
    <> fromTextM scopeName
    <> fromTextM "}"
    <> fromTextM "["
    <> fromTextM arg
    <> fromTextM "]\n"
    <> t
    <> fromTextM "\n\\end{"
    <> fromTextM scopeName
    <> fromTextM "}"

scopeWith :: (Applicative m) => Text -> Text -> NormalTextT m () -> NormalTextT m ()
scopeWith = scopeWithM

scopeM :: (Monoid a, Applicative m) => Text -> NormalTextT m a -> NormalTextT m a
scopeM scopeName t =
  fromTextM "\\begin{"
    <> fromTextM scopeName
    <> fromTextM "}\n"
    <> t
    <> fromTextM "\n\\end{"
    <> fromTextM scopeName
    <> fromTextM "}"

scope :: (Applicative m) => Text -> NormalTextT m () -> NormalTextT m ()
scope = scopeM

mscopeWithM :: (Monoid a, Applicative m) => Text -> Text -> MathTextT m a -> MathTextT m a
mscopeWithM arg scopeName t = cast $ scopeWithM arg scopeName (cast t)

mscopeWith :: (Applicative m) => Text -> Text -> MathTextT m () -> MathTextT m ()
mscopeWith = mscopeWithM

mscopeM :: (Monoid a, Applicative m) => Text -> MathTextT m a -> NormalTextT m a
mscopeM scopeName t = scopeM scopeName (cast t)

mscope :: (Applicative m) => Text -> MathTextT m () -> NormalTextT m ()
mscope = mscopeM

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

usepackageM :: (Monoid a, Applicative m) => NormalTextT m a
usepackageM = commandM "usepackage"

usepackage :: (Applicative m) => NormalTextT m ()
usepackage = usepackageM

defaultArticleT ::
  (Monoid a, Monad m) =>
  Text ->
  Text ->
  Text ->
  NormalTextT m a ->
  NormalTextT m a ->
  m Document
defaultArticleT title author date preamble content =
  fmap (Document . snd) . toTextPack . runWriterT . unNormalTextT $
    lift $
      commandM "documentclass"
        .> textM "article"
        % preamble
        % commandM "title"
        .> textM title
        % commandM "author"
        .> textM author
        % commandM "date"
        .> textM date
        % scopeM "document" (commandM "maketitle" % content)

defaultArticle ::
  (Monoid a) =>
  Text ->
  Text ->
  Text ->
  NormalText a ->
  NormalText a ->
  Document
defaultArticle title author date preamble content = runIdentity $ defaultArticleT title author date preamble content

compileWith :: (Monad m) => (m Document -> IO Document) -> FilePath -> m Document -> IO ()
compileWith runner filepath mdoc = do
  Document t <- runner mdoc
  TextIO.writeFile filepath t

compile :: FilePath -> Document -> IO ()
compile filepath (Document t) = TextIO.writeFile filepath t

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
