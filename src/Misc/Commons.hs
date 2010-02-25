{-# LANGUAGE TypeSynonymInstances #-}

module Misc.Commons where

import Control.Applicative 
import Control.Concurrent.STM
import Control.Monad (liftM, liftM2)
import Data.Char
import Data.Time.Clock
import Data.Time.LocalTime
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Data.List (partition, tails, inits, isPrefixOf)
import qualified Data.Map as Map

-------[ common utilities ]----------------------------------------------------

infixl 8 .$
(.$) :: (a -> b) -> a -> b
(.$) a b = a b

dup :: t -> (t, t)
dup a = (a, a)

branch :: (t -> t1) -> (t -> t2) -> t -> (t1, t2)
branch f g a = (f a, g a)

swap :: (t, t1) -> (t1, t)
swap (a, b) = (b, a)

bool :: t -> t -> Bool -> t
bool a b c = if c then a else b

-- Test eithers.

left :: Either a b -> Bool
left  (Left  _) = True
left  _         = False

right :: Either a b -> Bool
right (Right _) = True
right _         = False

partitionEither :: [Either a b] -> ([a], [b])
partitionEither xs =
  let (a, b) = partition left xs in
    (map (\(Left  x) -> x) a,
     map (\(Right x) -> x) b)

safeRead :: Read a => String -> Maybe a
safeRead s = case reads s of
  [(x, "")] -> Just x
  _         -> Nothing

intRead :: String -> Maybe Int
intRead = safeRead

-- Conversions.

boolM :: Bool -> Maybe ()
boolM = bool (Just ()) Nothing

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

listToMaybe :: [a] -> Maybe [a]
listToMaybe [] = Nothing
listToMaybe xs = Just xs

-------[ list utilities ]------------------------------------------------------

safeList :: ([a] -> b) -> [a] -> Maybe b
safeList _ [] = Nothing
safeList f xs = Just $ f xs

safeHead :: [a] -> Maybe a
safeHead = safeList head

safeLast :: [a] -> Maybe a
safeLast = safeList last

withReverse :: ([a1] -> [a]) -> [a1] -> [a]
withReverse f = reverse . f . reverse

trimWith :: (a -> Bool) -> [a] -> [a]
trimWith f = withReverse (dropWhile f) . dropWhile f

snoc :: [a] -> a -> [a]
snoc a b = a ++ [b]

split :: Eq a => a -> [a] -> [[a]]
split c = splitWith (==c)

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith f xs =
  case span (not . f) (dropWhile f xs) of
   ([], []) -> []
   (a,  []) -> [a]
   (a,  b)  -> a : splitWith f b

splitsWith :: Eq a => [a] -> [a] -> Maybe ([a], [a])
splitsWith n h = 
    fmap (fmap (drop (length n)))
  . safeHead
  . filter (isPrefixOf n . snd)
  . uncurry zip
  . fmap2 inits tails
  $ dup h

-------[ map utilities ]-------------------------------------------------------

(<->) :: Ord k => k -> a -> Map.Map k a -> Map.Map k a
(<->) a b c = Map.insert a b c

(<--) :: Ord k => k -> a -> Map.Map k a
(<--) a b = Map.insert a b Map.empty

-------[ text manipulation ]---------------------------------------------------

normalCase :: String -> String
normalCase []     = []
normalCase (x:xs) = toUpper x : map toLower xs

upperCase :: String -> String
upperCase = map toUpper

lowerCase :: String -> String
lowerCase = map toLower

-- Trim all heading and trailing whitespace.

trim :: String -> String
trim = trimWith (`elem` " \t\n\r")

-- ShowS functions.

ss :: String -> String -> String
ss = showString

intersperseS :: (String -> String) -> [String -> String] -> String -> String
intersperseS _ []  = id
intersperseS _ [s] = s
intersperseS c xs  = foldl1 (\a -> ((a.c).)) xs

concatS :: [String -> String] -> String -> String
concatS = foldl (.) id

concatMapS :: (a -> String -> String) -> [a] -> String -> String
concatMapS f = concatS . map f

unlinesS :: [String -> String] -> String -> String
unlinesS = concatMapS (. ss "\n")

eolS :: String -> String
eolS = ss "\n"

-------[ second arity functor ]------------------------------------------------

class Functor2 f where
  fmap2 :: (a -> c) -> (b -> d) -> f a b -> f c d

instance Functor2 (,) where
  fmap2 f g (a, b) = (f a, g b)

instance Functor2 Either where
  fmap2 f _ (Left  a) = Left  (f a)
  fmap2 _ g (Right b) = Right (g b)

-------[ monadic expressions ]-------------------------------------------------

ifM :: Monad m => m Bool -> (a -> a) -> a -> m a
ifM t f g = t >>= (\b -> return $ if b then f g else g)

-- Thread a value through monadic expression.

constM :: Monad m => b -> m a -> m b
constM c = (>> return c)

-------[ parsec extensions ]---------------------------------------------------

-- Helper function to quickly apply a parser.
(@@) :: GenParser Char () a -> String -> Maybe a
(@@) p b = either (const Nothing) Just $ parse (p <* eof) "" b

(@!) :: GenParser Char () a -> String -> Either ParseError a
(@!) p b = parse (p <* eof) "" b

-- Apply a parser for a minimum of `n' times.

pMin :: Int -> GenParser tok st a -> GenParser tok st [a]
pMin n p = (++) <$> count n p <*> many p

-- Apply a parser for a maximum of `n' times.

pMax :: Int -> GenParser tok st a -> GenParser tok st [a]
pMax n p | n <= 0    = return []
         | otherwise = option [] $ liftM2 (:) p (pMax (n-1) p)

-- Apply a parser for a minimum of `n' and a maximum of 'm' times.

pRange :: Int -> Int -> GenParser tok st a -> GenParser tok st [a]
pRange n m p = liftM2 (++) (count n p) (pMax (m-n) p)

-- Option parser with maybe result.
pMaybe :: GenParser tok st a -> GenParser tok st (Maybe a)
pMaybe = option Nothing . liftM Just

-------[ time utils ]----------------------------------------------------------

later :: Integer -> IO LocalTime
later howlong = do
  zone <- getCurrentTimeZone
  time <- liftM (addUTCTime $ fromInteger howlong) getCurrentTime
  return $ utcToLocalTime zone time

now :: IO LocalTime
now = later 0

-------[ concurrency utils ]---------------------------------------------------

atomModTVar :: (a -> a) -> TVar a -> IO a
atomModTVar f v = atomically $ do
  t <- readTVar v
  writeTVar v (f t)
  return (f t)

atomReadTVar :: TVar a -> IO a
atomReadTVar = atomically . readTVar

atomWithTVar :: (a -> b) -> TVar a -> IO b
atomWithTVar f v = atomically
  $ liftM f (readTVar v)
  
