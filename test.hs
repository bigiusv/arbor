{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- Simple Forth

module SimpleForth where

t1 = begin push 1 push 2 add end

-- The simplest implemenation: Danvy's Functional unparsing
{-
begin :: (() -> a) -> a
begin k = k ()
push :: st -> Int -> ((Int,st) -> a) -> a
push st x k = k ((x::Int),st)
add :: (Int, (Int, st)) -> ((Int,st) -> a) -> a
add (n1,(n2,st)) k = k (n1+n2,st)
end :: (a,st) -> a
end (top,st) = top
-}

      
-- Uncomment the following to get a hint why t2 is too slow
{-
t11 = (begin `asTypeOf` _b) (push `asTypeOf` _p1) 1
      (push `asTypeOf` _p2) 2 (add `asTypeOf` _add)
      (end `asTypeOf` _end)
{-
 Found hole â€˜_bâ€™
      with type: (()
                  -> Int
                  -> ((Int, ())
                      -> Int
                      -> ((Int, (Int, ())) -> ((Int, ()) -> Int) -> Int)
                      -> ((Int, ()) -> Int)
                      -> Int)
                  -> Int
                  -> ((Int, (Int, ())) -> ((Int, ()) -> Int) -> Int)
                  -> ((Int, ()) -> Int)
                  -> Int)
                 -> Int
                 -> ((Int, ())
                     -> Int
                     -> ((Int, (Int, ())) -> ((Int, ()) -> Int) -> Int)
                     -> ((Int, ()) -> Int)
                     -> Int)
                 -> Int
                 -> ((Int, (Int, ())) -> ((Int, ()) -> Int) -> Int)
                 -> ((Int, ()) -> Int)
                 -> Int

Found hole â€˜_p1â€™
      with type: ()
                 -> Int
                 -> ((Int, ())
                     -> Int
                     -> ((Int, (Int, ())) -> ((Int, ()) -> Int) -> Int)
                     -> ((Int, ()) -> Int)
                     -> Int)
                 -> Int
                 -> ((Int, (Int, ())) -> ((Int, ()) -> Int) -> Int)
                 -> ((Int, ()) -> Int)
                 -> Int

The type of _begin
      with type: (t1 -> Int -> ((Int, t1) -> t0) -> t0)
                 -> a0
                 -> (t3 -> Int -> ((Int, t3) -> t2) -> t2)
                 -> a1
                 -> ((t5, (t5, t6)) -> ((t5, t6) -> t4) -> t4)
                 -> ((t7, t8) -> t7)
                 -> t
-}

-- Faster implementation:
-- Found hole â€˜_bâ€™
--   with type: Push -> Int -> Push -> Int -> Add -> End -> t
-- Found hole â€˜_p1â€™ with type: Push

t12 = _begin push 1 push 2 push 3 add add end
-- 18 type vars

t13 = _begin push 1 push 2 push 3 push 4 add add add end
-- 24 type vars
-}


-- Uncomment the following only if you are prepared to wait
{-
t2 = begin push 1 push 2 push 3 push 4 push 5 push 6 push 7 push 8 push 9
           add
           add
           add
           add
           add
           add
           add
           add
           end
-}
-- ghc -c 1.6 secs
-- ghc -c -O2 1.8 secs

{-
t21 = begin push 1 push 2 push 3 push 4 push 5 push 6 push 7 push 8 push 9
           push 10
           add
           add
           add
           add
           add
           add
           add
           add
           add
           end
-}
-- ghc -c 5.6 secs
-- ghc -c -O2 6.4 secs

{-
t22 = begin push 1 push 2 push 3 push 4 push 5 push 6 push 7 push 8 push 9
           push 10
           push 11
           add
           add
           add
           add
           add
           add
           add
           add
           add
           add
           end
-- ghc -c 19.2 secs
-- Super-exponential!
-- With the faster implementation
-- ghc -c 0.23 secs
-- ghc -c -O2 0.26 secs
-}

{-
t23 = begin push 1 push 2 push 3 push 4 push 5 push 6 push 7 push 8 push 9
           push 10
           push 11
           push 12
           add
           add
           add
           add
           add
           add
           add
           add
           add
           add
           add
           end
-- In the simple approach, GHC is Out of memory!
-}


-- A more complex but faster way

-- Start with the 'stack' and then continue
class Forth stack r where
  build :: stack -> r

data End = End
end = End

instance (a ~ stack) => Forth stack (End -> a) where
  build stack _ = stack

data Add = Add
add = Add

-- Start with (Int, (Int, stack)), see Add and continue with (Int,stack)
instance Forth (Int,stack) r => Forth (Int,(Int,stack)) (Add -> r) where
  build (n1,(n2,stack)) _ = build (n1+n2,stack)

data Push = Push
push = Push

instance (a ~ Int, Forth (Int,stack) r) => Forth stack (Push -> a -> r) where
  build stack _ n = build (n,stack)


begin = build ()

-- All of the following typecheck instantaneously, even on my slow
-- laptop

tt3 = begin
      push 1
      push 2 add
      push 3 add
      push 4 add
      push 5 add
      push 6 add
      push 7 add
      push 8 add
      push 9 add
      end
-- (45,())


data Slash = Slash deriving (Eq,Show)
(//) :: Slash
(//) = Slash

type Segment = String
data pose = Empty
          | Segment Segment
          | Concat pose pose
          | Multiple [pose]
          deriving (Eq,Show)

class C a where
    make :: pose -> a

instance C x => C (Slash -> Segment -> x) where
    make Empty Slash s = make (Segment s)
    make g Slash s = make (Concat g (Segment s))
instance C x => C (Slash -> [pose] -> x) where
    make g Slash gs = make (Concat g (Multiple gs))
instance (x ~ pose) => C (End -> x) where
    make g _ = g
-- instance (x ~ pose) => C (Slash -> x) where
    -- make g _ = g

goto = make Empty (//)
main = goto "1" (//) [goto "2" (//) [goto "2" End, goto "a" End] End, goto "a" End] End

class D a where
    prep :: [Segment] -> a

instance D x => D (Slash -> Segment -> x) where
    prep g Slash s = prep (g ++ [s])
instance D x => D (Slash -> [Segment] -> x) where
    prep g Slash ss = prep (g ++ ss)
instance (x ~ [Segment]) => D (End -> x) where
    prep g _ = g
    
start = prep [] (//)
-- main = start "1" (//) ["2","a"] (//) ["3","4"] End
-- main = start "a" (//) "b" (//) "c" End



-- class C a where
    -- f :: String -> a

-- instance C x => C (Char -> x) where
    -- f a x = f (a ++ [x])

-- instance C x => C (Bool -> x) where
    -- f a x = f (a ++ show x)

-- instance C x => C (String -> x) where
    -- f a x = f (a ++ x)
    
-- instance (x ~ String) => C (End -> x) where
    -- f a _ = a

-- -- goto = f ""
-- main = putStrLn $ f "Hello, " True " world" '!' End
