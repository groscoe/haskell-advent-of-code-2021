module Stack where

type Stack a = [a]

stackToList :: Stack a -> [a]
stackToList = id

emptyStack :: Stack a
emptyStack = []

push :: a -> Stack a -> Stack a
push x xs = x : xs

pop :: Stack a -> (a, Stack a)
pop xs = (head xs, tail xs)

top :: Stack a -> a
top = head

remove :: Stack a -> Stack a
remove = tail
