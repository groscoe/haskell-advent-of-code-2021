{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module State where

newtype State s a = State {runState :: s -> (a, s)}
  deriving (Functor)

instance Applicative (State s) where
  pure :: a -> State s a
  pure x = State (x,)

  (<*>) :: State s (a -> b) -> State s a -> State s b
  State mf <*> State mx = State $ \s ->
    let (f, s') = mf s
        (x, s'') = mx s'
     in (f x, s'')

instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  State mx >>= f = State $ \s ->
    let (x, s') = mx s
     in runState (f x) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put newState = State $ const ((), newState)

evalState :: State s a -> s -> a
evalState s = fst . runState s

execState :: State s a -> s -> s
execState s = snd . runState s
