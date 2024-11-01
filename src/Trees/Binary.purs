module Trees.Binary where

import Prelude

import Data.Array (foldr, replicate, reverse)
import Data.Foldable (maximum)
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromMaybe)
import Data.String (length)
import Effect (Effect)
import Utils (println)


data Tree a
  = EmptyTree
  | Node a (Tree a) (Tree a)

derive instance genericTree :: Generic (Tree a) _

insertNode :: forall a. Ord a => a -> Tree a -> Tree a
insertNode x EmptyTree = Node x EmptyTree EmptyTree

insertNode x (Node a left right)
  | x == a = Node a left right
  | x < a = Node a (insertNode x left) right
  | x > a = Node a left (insertNode x right)
  | otherwise = Node a left right

isTreeNode :: forall a. Ord a => a -> Tree a -> Boolean
isTreeNode _ EmptyTree = false

isTreeNode x (Node a left right)
  | x == a = true
  | x < a = isTreeNode x left
  | x > a = isTreeNode x right
  | otherwise = false

instance showTree :: (Show a) => Show (Tree a) where
  show EmptyTree = ""
  show tree = show' tree 0 $ widestElem tree + 1

show' :: forall a. (Show a) => Tree a -> Int -> Int -> String
show' EmptyTree _ _ = ""
show' (Node root left right) depth width = offset_r <> "\n" <> offset_c <> offset_l
  where
  offset_c = (foldr (<>) "" $ (replicate depth " ")) <> show root
  offset_l = show' left   (depth + width) width
  offset_r = show' right  (depth + width) width

widestElem :: forall a. (Show a) => Tree a -> Int
widestElem EmptyTree = 0

widestElem (Node root left right) = fromMaybe 0 (maximum [ l, r, c])
  where
  l = widestElem left
  r = widestElem right
  c = length $ show root

makeTree :: forall a. (Ord a) => Array a -> Tree a
makeTree = (foldr insertNode EmptyTree) <<< reverse

test :: Effect Unit
test = do
  let
    nums = [6, 4, 3, 5, 7, 1, 9] :: Array Int
    t = makeTree nums
  println t
