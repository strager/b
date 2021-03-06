{-# LANGUAGE GADTs #-}

module B.Oracle.InMemory.Pure
  ( State
  , getState
  , putState

  , mkOracle

  , empty
  , get
  , put
  , recheck
  , addDependency
  ) where

import Control.Applicative hiding (empty)
import Control.Monad
import Data.Typeable

import qualified Data.Binary as Binary
import qualified Data.Either as Either
import qualified Data.Maybe as Maybe

import B.Oracle (Oracle)
import B.Oracle.Binary
import B.Question
import B.Util.Binary (Sized(..), getCounted, putCounted)

import qualified B.Oracle as Oracle

data State m = State
  { questionAnswers :: [QuestionAnswer m]
  , dependencies :: [Dependency m]
  }

instance Show (State m) where
  show (State qas deps)
    = "questionAnswers =\n" ++ showBulleted qas
    ++ "dependencies =\n" ++ showBulleted deps
    where
    showBulleted :: (Show a) => [a] -> String
    showBulleted = unlines . map ((" - " ++) . show)

getState
  :: (Fingerprint -> Maybe (AQuestion m{-undefined-}))
  -> Binary.Get (State m)
getState lookupQuestion = State
  <$> getCounted (getQuestionAnswer lookupQuestion)
  <*> getCounted (getDependency lookupQuestion)

putState :: State m -> Binary.Put
putState s = do
  putCounted putQuestionAnswer (questionAnswers s)
  putCounted putDependency (dependencies s)

mkOracle
  :: (Monad m)
  => m (State m)  -- ^ 'get'/'read'/'ask'
  -> ((State m -> m (State m)) -> m ())  -- ^ 'modify'
  -> Oracle m
mkOracle ask modify = Oracle.Oracle
  { Oracle.get = \ q -> liftM (get q) ask
  , Oracle.put = (modifyPure .) . put
  , Oracle.recheck = modify . recheck
  , Oracle.recheckAll = modify recheckAll
  , Oracle.addDependency = (modifyPure .) . addDependency
  }
  where
  modifyPure f = modify (return . f)

data QuestionAnswer m where
  QuestionAnswer
    :: (Question q, m ~ AnswerMonad q)
    => q -> Answer q -> QuestionAnswer m

instance Show (QuestionAnswer m) where
  show (QuestionAnswer q a) = show q ++ " := " ++ show a

instance Eq (QuestionAnswer m) where
  QuestionAnswer questionA answerA == QuestionAnswer questionB answerB
    = cast questionA == Just questionB
    && cast answerA == Just answerB

getQuestionAnswer
  :: (Fingerprint -> Maybe (AQuestion m{-undefined-}))
  -> Binary.Get (QuestionAnswer m)
getQuestionAnswer lookupQuestion = do
  fingerprint <- getTypeFingerprint
  case lookupQuestion fingerprint of
    Just (AQuestion q) -> uncurry QuestionAnswer
      <$> getQuestionAnswerPair q
    Nothing -> fail "No type matching fingerprint"

  where
  getQuestionAnswerPair
    :: (Question q) => q -> Binary.Get (q, Answer q)
  getQuestionAnswerPair _ = do
    Sized q <- Binary.get
    Sized a <- Binary.get
    return (q, a)

putQuestionAnswer :: QuestionAnswer m -> Binary.Put
putQuestionAnswer (QuestionAnswer q a) = do
  putTypeOf q
  Binary.put (Sized q)
  Binary.put (Sized a)

data Dependency m where
  Dependency
    :: ( Question from
       , Question to
       , m ~ AnswerMonad from
       , m ~ AnswerMonad to
       )
    => from -> to -> Dependency m

instance Show (Dependency m) where
  show (Dependency from to) = show from ++ " => " ++ show to

instance Eq (Dependency m) where
  Dependency fromA toA == Dependency fromB toB
    = cast fromA == Just fromB
    && cast toA == Just toB

getDependency
  :: (Fingerprint -> Maybe (AQuestion m{-undefined-}))
  -> Binary.Get (Dependency m)
getDependency lookupQuestion = do
  fromFingerprint <- getTypeFingerprint
  toFingerprint <- getTypeFingerprint
  case (lookupQuestion fromFingerprint, lookupQuestion toFingerprint) of
    (Just (AQuestion from), Just (AQuestion to))
      -> uncurry Dependency <$> getDependencyPair from to
    _ -> fail "No type matching fingerprint"

  where
  getDependencyPair
    :: (Question from, Question to)
    => from
    -> to
    -> Binary.Get (from, to)
  getDependencyPair _ _ = do
    Sized from <- Binary.get
    Sized to <- Binary.get
    return (from, to)

putDependency :: Dependency m -> Binary.Put
putDependency (Dependency from to) = do
  putTypeOf from
  putTypeOf to
  Binary.put (Sized from)
  Binary.put (Sized to)

empty :: State m
empty = State [] []

get
  :: (Question q, m ~ AnswerMonad q)
  => q -> State m -> Maybe (Answer q)
get q s = findJust getAnswer (questionAnswers s)
  where
  getAnswer (QuestionAnswer q' a)
    | cast q == Just q' = cast a
  getAnswer _ = Nothing

put
  :: (Question q, m ~ AnswerMonad q)
  => q -> Answer q -> State m -> State m
put q a s = s
  { questionAnswers = QuestionAnswer q a
    `setInsert` questionAnswers s
  }

-- TODO check answer (as in recheckAll).
recheck
  :: (Question q, m ~ AnswerMonad q)
  => q -> State m -> m (State m)
recheck q = return . dirty q

dirty
  :: (Question q, m ~ AnswerMonad q)
  => q -> State m -> State m
dirty q state = foldr dirty' state' dependants
  where
  state' = State questionAnswers' dependencies'
  questionAnswers' = filter isClean $ questionAnswers state
  isClean (QuestionAnswer q' _) = Just q /= cast q'
  (dependencies', dependants) = dropDependants q $ dependencies state

dirty' :: AQuestion m -> State m -> State m
dirty' (AQuestion q) = dirty q

recheckAll :: (Monad m) => State m -> m (State m)
recheckAll state = do
  badQAs <- filterM
    (\ (QuestionAnswer q a) -> liftM not $ testAnswer q a)
    $ questionAnswers state
  let badQs = map questionAnswerQuestion badQAs
  return $ foldr dirty' state badQs

questionAnswerQuestion :: QuestionAnswer m -> AQuestion m
questionAnswerQuestion (QuestionAnswer q _) = AQuestion q

addDependency
  :: ( Question from
     , Question to
     , m ~ AnswerMonad from
     , m ~ AnswerMonad to
     )
  => from -> to -> State m -> State m
addDependency from to s = s
  { dependencies = Dependency from to
    `setInsert` dependencies s
  }

setInsert :: (Eq a) => a -> [a] -> [a]
setInsert x xs = x : filter (/= x) xs

findJust :: (a -> Maybe b) -> [a] -> Maybe b
findJust f = Maybe.listToMaybe . Maybe.mapMaybe f

dropDependants
  :: (Question q, m ~ AnswerMonad q)
  => q -> [Dependency m] -> ([Dependency m], [AQuestion m])
dropDependants q = Either.partitionEithers . map f
  where
  f (Dependency from to)
    | cast q == Just to
    = Right $ AQuestion from
  f dep = Left dep
