{-# LANGUAGE GADTs #-}

module B.Oracle.InMemory.Pure
  ( State
  , mkOracle

  , empty
  , get
  , put
  , dirty
  , addDependency
  ) where

import Control.Monad
import Data.Typeable

import qualified Data.Either as Either
import qualified Data.Maybe as Maybe

import B.Oracle (Oracle)
import B.Question

import qualified B.Oracle as Oracle

data State m = State
  { questionAnswers :: [QuestionAnswer m]
  , dependencies :: [Dependency m]
  }

mkOracle
  :: (Monad m)
  => m (State m)  -- ^ 'get'/'read'/'ask'
  -> ((State m -> State m) -> m ())  -- ^ 'modify'
  -> Oracle m
mkOracle ask modify = Oracle.Oracle
  { Oracle.get = \ q -> liftM (get q) ask
  , Oracle.put = (modify .) . put
  , Oracle.dirty = modify . dirty
  , Oracle.addDependency = (modify .) . addDependency
  }

data QuestionAnswer m where
  QuestionAnswer
    :: (Question m q)
    => q -> Answer q -> QuestionAnswer m

data Dependency m where
  Dependency
    :: (Question m from, Question m to)
    => from -> to -> Dependency m

-- | Poor man's lens.
mapQuestionAnswers
  :: ([QuestionAnswer m] -> [QuestionAnswer m])
  -> State m -> State m
mapQuestionAnswers f s = s
  { questionAnswers = f (questionAnswers s) }

-- | Poor man's lens.
mapDependencies
  :: ([Dependency m] -> [Dependency m])
  -> State m -> State m
mapDependencies f s = s
  { dependencies = f (dependencies s) }

empty :: State m
empty = State [] []

get :: (Question m q) => q -> State m -> Maybe (Answer q)
get q = findJust f . questionAnswers
  where
  f (QuestionAnswer q' a)
    | cast q == Just q' = cast a
  f _ = Nothing

put :: (Question m q) => q -> Answer q -> State m -> State m
put q a = mapQuestionAnswers (QuestionAnswer q a :)

dirty :: (Question m q) => q -> State m -> State m
dirty q state = foldr dirty' state' dependants
  where
  state' = State questionAnswers' dependencies'
  questionAnswers' = filter isClean $ questionAnswers state
  isClean (QuestionAnswer q' _) = Just q /= cast q'
  (dependencies', dependants) = dropDependants q $ dependencies state

dirty' :: AQuestion m -> State m -> State m
dirty' (AQuestion q) = dirty q

addDependency
  :: (Question m from, Question m to)
  => from -> to -> State m -> State m
addDependency from to = mapDependencies (Dependency from to :)

findJust :: (a -> Maybe b) -> [a] -> Maybe b
findJust f = Maybe.listToMaybe . Maybe.mapMaybe f

dropDependants
  :: (Question m q)
  => q -> [Dependency m] -> ([Dependency m], [AQuestion m])
dropDependants q = Either.partitionEithers . map f
  where
  f (Dependency from to)
    | cast q == Just to
    = Right $ AQuestion from
  f dep = Left dep
