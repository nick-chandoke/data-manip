-- TODO: update examples
-- | Transforming streams that output purely, to file handle, or HTTP streaming response. See accompanying blog post, [How to Host Content](https://nicholaschandoke.me/articles/how-to-host-content). The goal is to be able to write a web route that can be written to a file or streamed.
--
-- === Design
--
-- * __Q__: Why output streams rather than Objects? __A__: it enables short-circuiting and flushing early, so that we can stream data as it becomes available, for macros that take time or memory to compute.
-- * __Q__: why @StreamingBody@ instead of @Conduit@? __A__: Because wai uses it. It used to use @Conduit@. They're basically isomorphic. The only obvious difference is that, though @Conduit@ isn't strictly related to @ResourceT@, they often go together, but afaik there're no @ResourceT@ functions for the  @Transform@ pattern. Still, at least one can wrap a @Transform@ in @bracket@.
--
-- If you're using only macros that execute quickly and have little output, then you don't benefit from streaming. That being said, you don't /lose/ anything, either. In the interest of making a unified API, I've created 'pureTrans' for that purpose. That way you can use streams without using streams ;p
--
-- This module uses @MonadChronicle@, which generalizes @ExceptT@ and 'ChronicleT'. @ChronicleT@ was considered in anticipation for possible future proofreading utilities that should support accumulating errors at authoring time, and shorting errors at execution time. Polymorphism is retained with recognition that many routes will fail without accumulating errors, and thus are more appropriately (or easily) written in terms of @ExceptT@. Remember that we have 'liftExcept' and 'memento' to convert between the two.
--
-- === Efficiency Note
--
-- As I mention in the blog post, there're no downsides to using @Transform@ compared to @(Utf8)Builder@. This assumes that you're using flush judiciously! If you're unsure, then it's probably better to use it less often than more often. (Maybe it'd be nice to have a monad transformer that auto-flushes when buffer reaches a given size?)
--
-- == Examples
--
-- The types that I give are monomorphic examples of actually polymorphic signatures.
--
-- === Pure
--
-- @
-- runTransformPure (send id   *> pure 3) "hello!" :: WriterT Text IO Int
-- runTransformPure (send lift *> pure 3) "hello!" :: ChronicleT [Text] (Writer Text) Int
-- @
--
-- === I/O
--
-- @
-- 'toStreamingBodyUtf8'
-- @
--
-- Check-out 'macros' for more examples.
module Transform
( -- * Basic Types
  Transform(..)
, strmap
, transLift
, NatTrans
, mmap
-- * Basic Arrows
, send
, flush
, sendAndFlush
, withInput
-- * Execution
, runTransformHandle
, toStreamingBodyUtf8
, toStreamingBody
, runTransformPure
-- * Parse Arrow
, macros
-- * Error Handling
, handleExcept
, liftExcept
-- * Specific Transforms
--, footnotes
) where

import Control.Arrow
import Control.Category
import Control.Monad.Trans.Chronicle
import Control.Monad.Trans.Chronicle
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer.Strict
import Data.Attoparsec.Text
import Data.Bitraversable
import Data.Semigroup (Last(..))
import Data.These
import Data.These
import NicLib.NStdLib (maybeC)
import RIO hiding ((.), id)
import RIO.Map (Map)
import RIO.Text (Text)
import qualified RIO.Map as M
import qualified RIO.Text as T

-- | Natural transformtion. Unrelated to @Transform@.
type NatTrans n m = forall a. n a -> m a

-- | "Monad map." I couldn't think of a better name for a general 2-morphism.
--
-- ex. @mmap runChronicleT :: Transform n (ChronicleT c d) s i a -> Transform n d s i (These c a)@
mmap :: (c a -> d b) -> Transform n c s i a -> Transform n d s i b
mmap n t = Transform $ \s fl i -> n (runTrans t s fl i)

-- Nearly isomorphic to ReaderT (s -> n (), n ()) m a: Transform encapsulates its input, so we can map over it, whereas ReaderT does not.
-- | Category equipped with two functions: one to send/write a string, and one to flush the buffer that it's been written to. The send & flush monad differs from the codomain to accomodate monad transformers for which @lift@ is a monomorphism. It seems that any natural transformation from @n@ to @m@ should be monic. Using two monads is necessary to flush to IO (/e.g./ the case of @StreamingBody@) while still collecting errors or having other effects usually employed by monad transformers.
newtype Transform n m s a b = Transform { runTrans :: (s -> n ()) -> n () -> a -> m b } deriving Functor -- m b rather than b because calling send or flush mandates returning in a monad

instance Monad m => Category (Transform n m s) where
    id = Transform $ \_ _ -> pure
    Transform g . Transform f = Transform $ \s fl i -> f s fl i >>= g s fl

instance Monad m => Arrow (Transform n m s) where
    arr m = Transform $ \_ _ -> pure . m
    first  (Transform t) = Transform $ \s fl (b, d) -> (,d) <$> t s fl b
    second (Transform t) = Transform $ \s fl (b, d) -> (b,) <$> t s fl d
    Transform g *** Transform f = Transform $ \s fl (b, b') -> liftA2 (,) (g s fl b) (f s fl b')
    Transform g &&& Transform f = Transform $ \s fl b -> liftA2 (,) (g s fl b) (f s fl b)

instance Monad m => ArrowChoice (Transform n m s) where
    left (Transform t) = Transform $ \s fl -> bitraverse (t s fl) pure
    right (Transform t) = Transform $ \s fl -> bitraverse pure (t s fl)
    Transform g +++ Transform f = Transform $ \s fl -> bitraverse (g s fl) (f s fl)
    Transform g ||| Transform f = Transform $ \s fl -> g s fl ||| f s fl

instance Monad m => ArrowApply (Transform n m s) where
    app = Transform $ \s fl (Transform t, a) -> t s fl a

instance Monad m => Applicative (Transform n m s i) where
    pure = arr . const
    liftA2 f x y = uncurry f <$> (x &&& y) -- g and f must have same input, since the applicative category is parameterized over input type

instance Monad m => Monad (Transform n m s ()) where
    Transform t >>= k = Transform $ \s fl i -> t s fl i >>= \a -> (runTrans $ k a) s fl ()

-- Transform is not a Bifunctor because its input begets its output; thus we cannot generally map over the input and maintain transitivity. Only endomorphisms would be acceptable, but that's not unconstrained parametric polymorphism, so GHC doesn't allow it. However, you can map over the input of the string sending function.

-- | @Transform@ can't be a @MonadTrans@ because it's of the wrong kind. So here's its implementation of @lift@
transLift :: m b -> Transform n m s a b
transLift m = Transform $ \_ _ _ -> m

send :: NatTrans n m -> Transform n m s s ()
send n = Transform $ \s _ -> n . s

flush :: NatTrans n m -> Transform n m s a ()
flush n = Transform $ \_ fl _ -> n fl

sendAndFlush :: Applicative m => NatTrans n m -> Transform n m s s ()
sendAndFlush n = Transform $ \s fl i -> (n . s) i *> n fl

-- | Override any input passed to a transform, preserving send and flush
withInput :: a -> Transform n m s a b -> Transform n m s i b
withInput x (Transform t) = Transform $ \s fl _ -> t s fl x

-- | Imagining @Transform@ as a record type, @strmap f t = "t {send = send t . f}"@
strmap :: (s -> s') -> Transform n f s a b -> Transform n f s' a b
strmap f (Transform t) = Transform $ \s fl -> t (s . f) fl

-- | Output text to an IO 'Handle', as you would a @StreamingBody@.
runTransformHandle :: MonadIO n => Handle -> Transform n m Utf8Builder a b -> a -> m b
runTransformHandle h (Transform t) = t (hPutBuilder h . getUtf8Builder) (hFlush h)

toStreamingBodyUtf8 :: Transform IO IO Utf8Builder a () -> a -> (Builder -> IO ()) -> IO () -> IO ()
toStreamingBodyUtf8 = toStreamingBody . strmap getUtf8Builder

-- having n ~ m ~ IO is fine; we still benefit from having both types because before converting to IO, m will be e.g. ExceptT e IO
-- | Pass to @responseStream@ in the @wai@ package
toStreamingBody :: Transform IO IO Builder a () -> a -> (Builder -> IO ()) -> IO () -> IO ()
toStreamingBody t i = \s f -> (runTrans t) s f i

-- Writer is fine if its strict, right?
-- | Uses a @WriterT@ to accumulate stream (@tell@ as send, @pure ()@ as flush).
runTransformPure :: (Monad n, Monoid s) => Transform (WriterT s n) m s a b -> a -> m b
runTransformPure t = runTrans t tell (pure ())

-- | Lifts a call to @handle@ to a @ChronicleT@. See example in 'withError' documentation.
handleExcept :: (MonadUnliftIO m, Exception e, Semigroup l) => (e -> m l) -> m r -> ChronicleT l m r
handleExcept h r = ChronicleT $ handle (fmap This . h) (fmap pure r)

-- I wonder why this doesn't come already included in the @these@ package.
-- | @ExceptT@ → @ChronicleT@ monomorphism
--
-- * though @Last@ is not stictly needed to construct a @ChronicleT@, @ChronicleT@ is defined as an @Applicative@ only when parameterized over a @Semigroup@.
-- * monic because it allows the terminal object of the monad to be a product rather than a mere coproduct. See 'memento' for its epic dual.
liftExcept :: Functor m => NatTrans (ExceptT e m) (ChronicleT (Last e) m)
liftExcept = ChronicleT . fmap (either This That) . runExceptT . withExceptT Last

-- | Create a Transform from macros in a body of text. The macro format is QuasiQuotation-like: @[macroName|input|]@. This will pass @input@ to the @macroName@ macro, then replace the whole quasi-quote block with its output.
--
-- * Preface the first '[' with a backslash to leave as a literal.
--
-- *Note*: @buildTransform@ does not flush after performing macro substitutions; that is the responlibility of each macro itself. Only macros that give a signaficant amount of output should flush.
--
-- === Example
--
-- @
-- type OurTransformer n = Transform n (ChronicleT [Text] n) Text Text ()
--
-- let nat :: (MonadTrans t, Monad m) => m a -> t m a
--     nat = lift
--
--     send' :: (MonadTrans t, Monad n) => Transform n (t n) s s ()
--     send' = send nat
--
--     input :: Text
--     input = "just text [hi|this input will be ignored|] and [upper|this input will be uppercase'd|]"
--
--     macroMap :: Monad n => Map Text (OurTransformer n)
--     macroMap = [ ("hi", withInput "hi" send')
--                , ("upper", T.toUpper ^>> send')
--                ]
--
--     onMissingMacro :: Monad n => Text -> OurTransformer n
--     onMissingMacro macroName = transLift (dictate [macroName]) *> macros nat onMissingMacro macroMap
--
--     transform :: Monad n => OurTransformer n
--     transform = macros nat onMissingMacro macroMap
-- @
--
-- >>> runWriter . runChronicleT $ runTransformPure transform input
-- (That (),"just text hi and THIS INPUT WILL BE UPPERCASE'D")
--
-- If we change the "hi" macro in input to "ho", which is not in the macro map, then we get
--
-- @(These ["ho"] (),"just text  and THIS INPUT WILL BE UPPERCASE'D")@
--
-- Note that @onMissingMacro@ is recursively defined /and/ calls @macros@. This is an important pattern for @macros@, since it enables consuming the text after the invalid macro. If I'd defined onMissingMacro as @transLift . dictate . (:[])@, then, because /ho/ isn't a valid macro, I'd get @(These ["ho"] (),"just text ")@.
--
-- Also instead, I could run @transform@ as a HTTP response stream, here discarding effects collected from @ChronicleT@, and creating a WAI @Response@:
--
-- @responseStream ok200 [] (toStreamingBodyUtf8 . mmap (void . runChronicleT) . strmap display) transform input@
--
-- I generally prefer @ChronicleT@ over @ExceptT@ or @WriterT@, by the way, since it can do either. I'll assume, however, that @ChronicleT@ acts non-strictly; this it should be used for logging exceptional behavior, since such logging is expected to be little.
macros :: forall n m trans
        . (Monad m, trans ~ Transform n m Text Text ())
       => NatTrans n m
       -> (Text -> trans) -- ^ how to handle macros not being found, parameterized by the transform name that wasn't present in the map. The transform will run over the remaining input.
       -> Map Text trans -- ^ map of transforms by name
       -> trans
macros n handleNoMacro !m = go
  where
    go :: trans
    go = proc str -> do
        parsed <- arr (parse parser) -< str
        let y = case parsed of
                Partial _ -> send n
                Done remainder (!before, mm) ->
                    withInput before (send n)
                    *> withInput remainder (maybeC mm go $ \(!macro, arg) -> maybeC (M.lookup macro m) (handleNoMacro macro) $ \f -> withInput arg f *> go)
                _ -> error "impossible parse fail in macros"
        y -<< str
      where
        parser :: Parser (Text, Maybe (Text, Text))
        parser = escape '[' $ do
            name <- takeTill (=='|')
            next
            input <- manyTill' anyChar "|]" -- surprised there's no provided more efficient Text-specific version
            pure (name, T.pack input)

-- consider proofreading extension that, if a transform detects a syntax error, suggests a transform that'd fix it; all fixes are then applied at the end of the proofreading session, or incrementally
{- | Create links to footnotes for of the form:
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
footnotes :: Transform n m s Text ()
footnotes = \s fl -> case parse parser str of
    Fail before context errMsg ->
    Partial k ->
    Done remainder (before, mns) -> do
        s (display before)
        fl
        case mns of
            Nothing -> -- 続く: write footnotes, and apply it using comonads/functors, which are easy to define assuming that I'm hard-coding a structure (which I can afford to do for now.)
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
       | otherwise -> error "Impossible character encountered in escape"

-- | Skip a character
next :: Parser ()
next = skip (const True)
