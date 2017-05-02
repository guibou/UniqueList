{-
This test file is funny.

I'm using quickcheck, that's property based testing

So the library will generate sample for me (based on type inference) and I wrote properties.

For exemple, a property :

property \variables -> condition ==> the property to check
property $ \x y -> x > 0 && y > 0 ==> x + y > x && x + y > y

triple equal sign just means equal, but the library override it to display nice errors.

There is lot of ugly operators, as you like, but most of them are here to enhance readability.

That's a point of Haskell, you can use custom operators to create a custom DSL for your needs

There is also a few type annotation to help the type inference generate types for me.
-}
import Test.Hspec
import Test.QuickCheck

import qualified Data.List as List

import UniqueList
import Data.Monoid ((<>))

main :: IO ()
main = hspec $ do
  describe "empty" $ do
    it "is of size 0" $ do
      property $ \() -> size empty === 0

  describe "singleton" $ do
    it "is of size 1" $ do
      property $ \x -> size (singleton (x :: Int)) === 1

  describe "fromList / toList" $ do
    it "size is coherent with nub" $ do
      property $ \l -> size (fromList (l :: [Int])) === length (List.nub l)

    it "toList . fromList ~ nub" $ do
      property $ \l -> toList (fromList (l :: [Int])) === List.nub l

  describe "snoc" $ do
    it "increase the size if not present" $ do
      property $ \l x -> x `List.notElem` (l :: [Int]) ==> size (snoc x (fromList l)) === (size (fromList l) + 1)

    it "does not change the size if present" $ do
      property $ \l x -> x `List.elem` (l :: [Int]) ==> size (snoc x (fromList l)) === (size (fromList l))

    it "appears in toList" $ do
      property $ \l x -> x `List.elem` toList (snoc x (fromList (l :: [Int])))

  describe "member" $ do
    it "all items of a list are member" $ do
      property $ \l -> all (\x -> x `List.elem` (toList (fromList l))) (l :: [Int])

    it "an item not in list in not member" $ do
      property $ \l x -> x `List.notElem` l ==> not (member x (fromList (l :: [Int])))

  describe "replace" $ do
    it "replace ~ snoc if not member" $ do
      property $ \l x -> x `List.notElem` (l :: [Int]) ==> toList (replace x (fromList l)) === toList (snoc x (fromList l))

    it "replace does not change size if present" $ do
      property $ \l x -> x `List.elem` (l :: [Int]) ==> size (replace x (fromList l)) === (size (fromList l))

    it "replace on singleton changes nothing" $ do
      property $ \x -> toList (replace x (singleton (x :: Int))) == [x]

    it "replace on longer list changes the position" $ do
      property $ \l -> let nl = List.nub l
                       in length nl > 1 ==> toList (replace (head nl) (fromList nl)) === tail nl ++ [head (nl :: [Int])]

  describe "Instances" $ do
    it "fold" $ do
      property $ \l -> sum (fromList (l :: [Int])) == sum (List.nub l)

    it "fold the right direction -- last" $ do
      property $ \l -> not (null l) ==> last (foldMap (\x -> [x]) (fromList (l :: [Int]))) === last (List.nub l)

    it "fold the right direction -- first" $ do
      property $ \l -> not (null l) ==> head (foldMap (\x -> [x]) (fromList (l :: [Int]))) === head (List.nub l)

    it "monoid (concat)" $ do
      property $ \l1 l2 -> List.nub ((l1 :: [Int]) ++ l2) === toList (fromList l1 <> fromList l2)

  describe "idx" $ do
    it "idx ~ list.!!" $ do
      property $ \l -> let
        nl = List.nub (l :: [Int])
        in all (\i -> idx i (fromList nl) == Just (nl List.!! i)) [0 .. length nl - 1 ]
