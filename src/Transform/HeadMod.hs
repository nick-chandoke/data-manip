{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Defines 'HeadModT', which wraps Lucid's @Html a@ and an automorphism on \<head\>. Allows defining html fragments in the same place as the resources that it requires; never worry about \<head\> again! More generally, useful for importing a bunch of dependencies non-redundantly and without conflict. Example:
--
-- @
-- -- | An \<h2\> tag as per usual Lucid, plus the side effect of adding an /author/ meta tag in \<head\>
-- authorTag :: (Ord h, Monad m) => Text -> HeadModT h m ()
-- authorTag name
--     = 'putHead' ('Element' "meta" Nothing [name_ "author", content_ name])
--    \<+ h2_ ("Article written by " \<\> name)
-- 
-- testDoc :: Html ()
-- testDoc = 'toDoc' toHtml head body
--   where
--     -- head is a Set; using -XOverloadedLists
--     head = [ Element "title" (Just ("Sample Page" :: T.Text)) [] ]
--     body = authorTag "A.K. Yearling" <+ div_ "Ahuizotl's up to his old tricks again!"
-- @
--
-- >>> renderText testDoc
-- <!doctype html>
-- <html>
--     <head>
--         <meta content="A.K. Yearling" name="author"></meta>
--         <title>Sample Page</title>
--     </head>
--     <body>
--         <h2>Article written by A.K. Yearling</h2>
--         <div>Ahuizotl&#39;s up to his old tricks again!</div>
--     </body>
-- </html>
--
-- @(*>)@, @(+>)@, and friends are associative. I still haven't found a way to elegantly compose @(HeadModT a)@'s and @(Html a)@'s. Also, these examples are short and use @(<+)@; we could have used @do@ blocks and 'liftHtml' instead.
--
-- === Nota Bene
--
-- * this library uses /strict/ 'StateT'!
-- * throughout the library you'll see the symbol @conv@; this refers to either @toHtml@ or @toHtmlRaw@.
module Transform.HeadMod
( -- * HeadModT
  Element(..)
, Head
, HeadModT(..)
, HeadMod
, toDoc
, putHead
, mergeHead
, liftHtml
, (<+)
, (+>)
  -- * Utilities
, liftRender
) where

-- base
import Control.Applicative
import Control.Monad ((<=<))
import Data.Foldable
import Data.Functor.Identity
import RIO

-- transformers
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict

-- lucid
import Lucid
import Lucid.Base

-- containers
import qualified Data.Set as S

-- miscellaneous packages
import qualified Data.Text as T -- text

deriving instance Ord Attribute

-- | Abstract representation of an Html element that belongs in <head>. Usually I expect you'll use @Element Text@; however, I'm leaving it polymorphic so that you can use any @ToHtml a => Element a@, as that's the most general form of element that works with @toDoc@.
--
-- @Text@ should work fine considering that, as far as I know, at least, there are no children of <head> that have any more children than merely a single text node.
--
-- The purpose of @Element@ is to be put in @Head@, which is a set; you'll be using 'Set.fromList' (explicitly, or implicitly via @-XOverloadedLists@) to input that set, which means that the argument type to @Element@ must be ordered. Thus, even though it isn't present in the definition of @Element@, you'll practically need to use @Ord a => Element a@ rather than mere @Element a@. For example, the following would not compile wihout the explicit type:
--
-- @
-- thing :: (Monad m, Ord h) => 'HeadModT' h m ()
-- thing = liftHtml (div_ mempty)
--       *> 'mergeHead' ([ Element "link"   Nothing [href_ "some.css", rel_ "stylesheet"]
--                      , Element "script" Nothing [src_ "some.js"]
--                      ] :: Ord h => Set (Element h))
-- @
--
-- Well, that, /and/ both 'mergeHead' and 'putHead' require @Ord@ constraints as well! Thus there's a need for @Ord h@ in @thing@'s type signature.
data Element a = Element
    { name :: T.Text
    , mcontent :: Maybe a
    , attrs :: [Attribute]
    } deriving (Show, Eq, Ord)

instance ToHtml a => ToHtml (Element a) where
    toHtml (Element {name, mcontent, attrs}) =
        with (makeElement name (maybe mempty toHtml mcontent)) attrs
    toHtmlRaw (Element {name, mcontent, attrs}) =
        with (makeElement name (maybe mempty toHtmlRaw mcontent)) attrs

-- | We see the <head> element not as @Html ()@, but as @Set (Html ())@ because
--
-- 1. \<head\> is an almost flat hierarchy, and
-- 2. we want 'HeadModT''s to be able to modify the elements in \<head\>, which would not be possible by storing it as @Html ()@, since @Html ()@, despite being a semigroup, has no way of accessing its elements separately (/i.e./ it supports reduction but not generation.)
--
-- One such need is not adding redundant elements; if I want to add three Google charts, I shouldn't add the CSS & JS three times! This is the reason for using @StateT@, too.
type Head a = S.Set (Element a)

newtype HeadModT h m a = HeadModT
    { runHeadModT :: StateT (Head h) (HtmlT m) a }
    deriving (Functor, Applicative, Monad)

instance (Semigroup a, Monad m) => Semigroup (HeadModT h m a) where
    HeadModT a <> HeadModT b = HeadModT $ liftA2 (<>) a b

instance (Monoid a, Monad m) => Monoid (HeadModT h m a) where
    mempty = HeadModT . lift . pure $ mempty

instance Monad m => IsString (HeadModT h m ()) where
    fromString = HeadModT . lift . fromString

type HeadMod h = HeadModT h Identity

-- | Put an element into \<head\>
putHead :: (Monad m, Ord h) => Element h -> HeadModT h m ()
putHead = HeadModT . modify . S.insert

-- | Set union a @Head h@ with the @HeadModT@'s @Head h@ state
mergeHead :: (Monad m, Ord h) => Head h -> HeadModT h m ()
mergeHead s = HeadModT $ modify (<> s)

-- | Lift 'HtmlT' into 'HeadModT'
liftHtml :: Monad m => HtmlT m a -> HeadModT h m a
liftHtml = HeadModT . lift

-- | Lift LHS into @HeadModT@, then sequence
(+>) :: Monad m => HtmlT m a -> HeadModT h m b -> HeadModT h m b
a +> b = liftHtml a *> b

-- | Lift RHS into @HeadModT@, then sequence
(<+) :: Monad m => HeadModT h m b -> HtmlT m a -> HeadModT h m b
(<+) = flip (+>)

-- | The main way to use @HeadModT@; converts HeadModT into \<head\> and \<body\> and puts them under 'doctypehtml_'.
toDoc :: (ToHtml h, Monad m)
      => (Element h -> HtmlT m ()) -- ^ @conv@. You may think of it as @h -> HtmlT m ()@; @Element h@ is the morphism's domain because of a silly typechecking technicality
      -> Head h -- ^ the initial \<head\> before the head automorphisms are evaluated
      -> HeadModT h m a -- ^ the markup to set as child of the \<body\> element (do not wrap your HeadModT in a \<body\> tag!)
      -> HtmlT m ()
toDoc conv h0 hmt = do
    head <- lift $ evalHtmlT html_head -- eval the HtmlT m (Head h) to get m (Head h), then rewrap into HtmlT; this way we extract the (Head h) without writing the Html contents here.
    doctypehtml_ $ do {head_ $ foldMap conv head; body_ html}
    where
        html_head = execStateT (runHeadModT hmt) h0 -- :: HtmlT m (Head h)
        html = const () <$> html_head -- :: HtmlT m ()

-- | Turn a monadic variable into Html, so that it renders (rather than @lift@, which lifts in such a way that does not render)
--
-- Example:
--
-- >>> renderTextT $ liftRender (fromJust <$> lookupEnv "USER") <> liftRender (pure " ") <> liftRender (fromJust <$> lookupEnv "TERM")
-- "nic screen" :: IO Text
--
-- Given the @IsString@ instance for @HtmlT m ()@, the above example can be better expressed with ```-XOverloadedStrings```:
--
-- @renderTextT $ liftRender (fromJust \<$\> lookupEnv \"USER\") \<\> " " \<\> liftRender (fromJust \<$\> lookupEnv \"TERM\")@
liftRender :: (Monad m, ToHtml a) => m a -> HtmlT m ()
liftRender = toHtml <=< lift -- originally I'd written: liftRender = HtmlT . fmap ((,()) . const . putStringUtf8) :: Functor m => m String -> HtmlT m ()
