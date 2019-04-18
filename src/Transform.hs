-- | Utilities for applying string endomorphisms (or /transforms/) within parts of text documents. Two cases are supported: 1) text to text; and 2) text to @StreamingBody@ (see <http://hackage.haskell.org/package/wai-3.2.2/docs/Network-Wai.html#v:responseStream the wai package>. See <https://nicholaschandoke.me/articles/how-to-host-content How to Host Content>.
--
-- === Example
--
-- @
-- let input :: Text
--     input = "just text [hi|this input will be ignored|] and [upper|this input will be uppercase'd|]"

--     macroMap :: Applicative m =\> Map Text (Text -\> Transform m Utf8Builder)
--     macroMap = [ ("hi", \\_ send _ -\> send "hi")
--                , ("upper", \\arg send flush -\> send (display $ T.toUpper arg) *\> flush)
--                ]

--     onMissingMacro :: Monad m =\> Text -\> Text -\> Transform m Utf8Builder
--     onMissingMacro macroName remainder = \\send flush -\> send ("invalid macro: " \<\> display macroName) *\> flush *\> buildTransform onMissingMacro macroMap remainder send flush

--     transform :: Monad m =\> Transform m Utf8Builder
--     transform = buildTransform onMissingMacro macroMap input
-- @
-- >>> utf8BuilderToText . runIdentity . runTransform $ transform
-- "just text hi and THIS INPUT WILL BE UPPERCASE'D"
--
-- If we change the "hi" macro in input to "ho", which is not in the macro map, then we get the result "just text invalid macro: ho and THIS INPUT WILL BE UPPERCASE'D"
--
-- @runTransform@ sets @flush@ to a no-op, so flush does nothing in this example. That being said, if @transform@ were used as a @StreamingBody@ in a server setting, then flush would matter!
--
-- === Efficiency Note
--
-- As I mention in the blog post, there're no downsides to using @Transform@ compared to @(Utf8)Builder@. This assumes that you're using flush judiciously! If you're unsure, then it's probably better to use it less often than more often. (Maybe it'd be nice to have a monad that auto-flushes when buffer reaches a given size?)
module Transform where

import RIO
import RIO.Text (Text)
import qualified RIO.Text as T
import RIO.Map (Map)
import qualified RIO.Map as M
import Control.Monad.Trans.Writer.Strict
import Data.Attoparsec.Text

-- | Parses an input string and returns an output string in an Applicative. The first argument is a /send/ function, and the second a /flush/ one. A generalization of @StreamingBody@ which allows writing a streaming server route at the same time as a text transformer. This lessens work needed to keep dynamically and statically-served web routes consistent.
--
-- For responses that may fail/need validation, use @Either@. For example, giving an HTTP 500 Internal Server Error page (using some tongue-in-cheek objects of the @req@ package and lens):
--
-- @
-- someTrans :: (Status, Transform IO Utf8Builder)
-- someTrans = Transform $ \send flush -> ((status500,) ||| (ok200,)) <=< runExceptT $ do
--     md <- handle (throwM . to500Resp) . liftIO $ readFile "thing.md"
--     pure . display $ commonmarkToHtml [] md
--   where
--     to500Resp :: IOException -> Utf8Builder
--     to500Resp e = "Could not read markdown source file: " <> display e
--
-- case someTrans responseStream toStreamingBody
-- @
--
-- The last line of the monad should be @flush@ or @pure ()@.
newtype Transform f s = Transform { getTrans :: (s -> f ()) -> f () -> f () } deriving Functor -- alternatively I could return an s, then pass that to s merely inside. Really all I need is an s; I can supply the flush and 

-- | Create a @Transform@ that sequences two transforms. Essentially a wrapper around @*>@
instance Applicative f => Semigroup (Transform f t) where
    Transform f <> Transform g = Transform $ \send flush -> f send flush *> g send flush

-- | Run a @Transform@ on text. You can convert the output to Text via 'utf8BuilderToText'.
runTransform :: (Monad m, w ~ Utf8Builder) => Transform (WriterT w m) w -> m w
runTransform (Transform t) = fmap snd . runWriterT $ t tell (pure ())

-- | Output text to an IO 'Handle', as you would a @StreamingBody@.
runTransformHandle :: MonadIO m => Handle -> Transform m Utf8Builder -> m ()
runTransformHandle h (Transform t) = t (hPutBuilder h . getUtf8Builder) (hFlush h)

toStreamingBody :: Transform IO Utf8Builder -> (Builder -> IO ()) -> IO () -> IO ()
toStreamingBody = getTrans . fmap getUtf8Builder

-- | Create a transform that merely sends a value and *does not flush it*. This would be the implementation of @pure@ if @Transform@ were an @Applicative@, which it isn't because @liftA2@ cannot be defined (though I haven't proven this, and I'd like to identify exactly /why/ it cannot be done.)
pureTrans :: Applicative f => s -> Transform f s
pureTrans x = Transform $ \send _ -> send x

-- | Computes a @Transform@ completely or until short-circuiting. If failure, the returned @Transform@ is already complete and includes a call to flush.
withError :: ExceptT e IO Utf8Builder -> 

-- | Create a Transform from macros in a body of text. The macro format is QuasiQuotation-like: @[macroName|input|]@. This will pass @input@ to the @macroName@ macro, then replace the whole quasi-quote block with its output.
--
-- * Preface the first '[' with a backslash to leave as a literal.
--
-- *Note*: @buildTransform@ does not flush after performing macro substitutions; that is the responlibility of each macro itself. Only macros that give a signaficant amount of output should flush.
buildTransform :: forall m trans
                . (Monad m, trans ~ Transform m Utf8Builder)
               => (Text -> Text -> trans) -- ^ how to handle macros not being found. Arguments are macro name, then text following macro clause
               -> Map Text (Text -> trans) -- ^ macro identifiers and their corresponding functions to apply to their inputs
               -> Text -- ^ input text
               -> trans
buildTransform handleNoMacro !m = go
  where
    go :: Text -> trans
    go !s = Transform $ \send flush -> case parse parser s of
        Partial _ -> -- the input string ended in the middle of a macro clause; TODO: send warning about it
            send (display s)
        Done remainder (!before, mm) -> do
            send (display before)
            case mm of
                Nothing -> getTrans (go remainder) send flush
                Just (!macro, arg) -> case M.lookup macro m of
                    Nothing -> getTrans (handleNoMacro macro remainder) send flush
                    Just f -> getTrans (f arg) send flush *> getTrans (go remainder) send flush
        _ -> error "impossible parse fail in buildTransform"
      where
        parser :: Parser (Text, Maybe (Text, Text))
        parser = escape '[' $ do
            name <- takeTill (=='|')
            next
            input <- manyTill' anyChar "|]" -- surprised there's no provided more efficient Text-specific version
            pure (name, T.pack input)
{-
-- | Create links to footnotes for of the form:
--
-- @
-- Ordinary document like any other(1). Just *some(2) stuff*.
--
-- ---
--
-- (1) well, like any other, PLUS FOOTNOTES! :D
-- (2) not "any" stuff: *some* stuff!
-- @
--
-- Indented for markdown documents, but works on plaintext, too.
--
-- === Teachnical Details
--
-- * footnotes and their references both be in parenthesis
-- * footnotes must be each one line of plain text
-- * the footnotes section is defined as the section after the document's final "---"
-- * the footnotes do not need to start at 1. Any parenthesis-with-number ocurring before the footnotes section is looked-up in the footnotes section; thus having out of order or non-consecutive footnotes is fine.
-- * never should a footnote occur more than once in either the body or footnote lookup section. You'll get odd results otherwise (but it won't break anything.)
footnotes :: Text -> Transform m s
footnotes s = \send flush -> case parse parser s of
    Fail before context errMsg ->
    Partial k ->
    Done remainder (before, mns) -> do
        send (display before)
        flush
        case mns of
            Nothing ->
            Just ns ->
  where
    parser = escape '(' $ do
        n <- floor <$> scientific
        char ')' -}

-- | Enable a parser with escaping by leading backslash, and also returns the string prior to that which should be parsed. Returning the leading text enables /transforming/ text rather than just parsing /from out/ of it.
--
-- The returned parser is about (the text up to the backslash or specified leading charater, @Nothing@ if escaped; else @Just@ the result of the given parser).
--
-- Note that if the given parser fails, then the whole parser fails; in this case the leading text is in the first argument of the @Fail@ constructor.
escape :: Char -- ^ leading character
       -> Parser a -- ^ input parser to transform
       -> Parser (Text, Maybe a)
escape leading p = do
    before <- takeTill (\c -> c == '\\' || c == leading)
    someChar <- anyChar
    if | someChar == '\\' -> next *> pure (before, Nothing)
       | someChar == leading -> (before,) . Just <$> p
       | True -> error "Impossible character encountered in escape"

-- | Skip a character
next = skip (const True)
