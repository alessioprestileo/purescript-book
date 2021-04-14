module Test.MySolutions where

import Prelude

import Control.MonadZero (guard)
import Data.Array (catMaybes, cons, filter, find, foldl, head, last, length, tail, (..), (:))
import Data.Array.NonEmpty (elemLastIndex)
import Data.Foldable (maximum, minimum)
import Data.Int (quot, rem)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Path (Path(..), filename, isDirectory, ls, size)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Test.Examples (factors)

infix 4 filter as <$?>

isEven :: Int -> Boolean
isEven n = case n of
  0 -> true
  1 -> false
  _ -> isEven $ n - 2

oneIfEven :: Int -> Int
oneIfEven n = if isEven n then 1 else 0

countEven :: Array Int -> Int
countEven ints = safelyCountEven ints 0
  where
  safelyCountEven :: Array Int -> Int -> Int
  safelyCountEven [] count = count
  safelyCountEven ints' count = safelyCountEven (safeTail ints') updatedCount
    where
    safeTail :: forall a. Array a -> Array a
    safeTail xs = fromMaybe [] $ tail xs
    updatedCount = add count increment
    increment = oneIfEven $ safeHead ints'
    safeHead :: Array Int -> Int
    safeHead xs = fromMaybe (-1) $ head xs

squared :: Array Number -> Array Number
squared = map (\n -> n * n)

keepNonNegative :: Array Number -> Array Number
keepNonNegative = filter (_ >= 0.0)

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite xs = (_ >= 0.0) <$?> xs

isPrime :: Int -> Boolean
isPrime n = n > 1 && length (factors n) == 1

cartesianProduct :: ∀ a. Array a -> Array a -> Array (Array a)
cartesianProduct xs ys = do
  i <- xs
  j <- ys
  pure [i, j]

triples :: Int -> Array (Array Int)
triples n = do
  i <- 1 .. n
  j <- i .. n
  k <- j .. n
  guard $ i * i + j * j == k * k
  pure [i, j, k]

factorize :: Int -> Array Int
factorize 0 = []
factorize n = factorize' 2 n []
  where
  factorize' :: Int -> Int -> Array Int -> Array Int
  factorize' _ 1 result = result
  factorize' divisor dividend result =
    let
      remainder = rem dividend divisor
    in
      if remainder == 0 then
        factorize' divisor (quot dividend divisor) (cons divisor result)
      else
        factorize' (divisor + 1) dividend result

allTrue ::  Array Boolean -> Boolean
allTrue = foldl (\acc bool -> acc && bool) true

fibTailRec :: Int -> Int
fibTailRec n = fib' n 0 0 1
  where
  fib' :: Int -> Int -> Int -> Int -> Int
  fib' limit count n1 n2 =
    if limit == count then
      n1 + n2
    else
      fib' limit (count + 1) (n1 + n2) n1

reverse :: forall a. Array a -> Array a
reverse = foldl (\acc x -> [x] <> acc) []

allPaths :: Path -> Array Path
allPaths path = path : do
  child <- ls path
  allPaths child

onlyFiles :: Path -> Array Path
onlyFiles path = filter (not isDirectory) (allPaths path)

whereIs :: Path -> String -> Maybe Path
whereIs dir fName = head $ whereIs' $ allPaths dir
  where
  whereIs' :: Array Path -> Array Path
  whereIs' paths = do
    path <- paths
    child <- ls path
    guard $ eq fName $ fromMaybe "" $ last $ split (Pattern "/") $ filename child
    pure path

largestSmallest :: Path -> Array Path
largestSmallest dir = if not (isDirectory dir) then 
  [] else
  let
    files = onlyFiles dir
    allSizes = catMaybes $ map size files
    maxSize = maximum allSizes
    minSize = minimum allSizes
    findFileBySize :: Array Path -> Int -> Maybe Path
    findFileBySize filesList targetSize = find (\file -> size file == (Just targetSize)) filesList
  in
  case minSize, maxSize of
    Nothing, Nothing -> []
    Nothing, Just maxS -> 
      case (findFileBySize files maxS) of
      Nothing -> []
      Just file -> [file]
    Just minS, Nothing -> 
      case (findFileBySize files minS) of
      Nothing -> []
      Just file -> [file]
    Just minS, Just maxS  -> 
      case (findFileBySize files minS), (findFileBySize files maxS) of
      Nothing, Nothing -> []
      Just minFile, Nothing -> [minFile]
      Nothing, Just maxFile -> [maxFile]
      Just minFile, Just maxFile -> if size minFile == size maxFile
      then [minFile]
      else [minFile, maxFile]