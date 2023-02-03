module MultiSet (MSet, empty, add, occs, elems, subeq, union, mapMSet) where

-- The sort function is imported hers so it can be used in the implementation of the Eq

import Data.Foldable
import Data.List (sort)
import Data.Map qualified as M

{-
This defines a new type called MSet which is a type of set that contains elements of type a.
The type is defined as a list of pairs, where each pair consists of an element of type a
and an integer representing the multiplicity of that element in the set.
-}
data MSet a = MS [(a, Int)]
  deriving (Show)

{--
This defines an instance of the Eq typeclass for the MSet type.
This allows us to use the equality operators == and /= with values of type MSet a.
It defines it using the subeq
--}

instance (Eq a, Ord a) => Eq (MSet a) where
  (MS xs) == (MS ys) = subeq (MS xs) (MS ys) && subeq (MS ys) (MS xs)
  (MS xs) /= (MS ys) = not (subeq (MS xs) (MS ys) && subeq (MS ys) (MS xs))

{--
This Foldable instance for MSet defines how the foldr function should work on a MSet.
foldr takes a binary function f, a value z, and a Foldable data structure,
and applies f to each element of the data structure, using z as the initial accumulator value.
It then returns the final accumulator value.
--}
-- instance Foldable MSet where
--   foldr f z (MS xs) = foldr (\(x, n) acc -> f x (f x acc)) z xs

instance Foldable MSet where
  foldr f z (MS xs) = foldr (\(x, n) acc -> foldr f acc (replicate n x)) z xs

{--
The mapMSet function takes two arguments: a function f of type a -> b and a multiset (MS xs) of type MSet a.
It returns a new multiset of type MSet b, obtained by applying f to each element x in xs and pairing the result with the corresponding multiplicity n.
The resulting list of pairs is then used to construct the new multiset MS [(f x, n) | (x, n) <- xs].
--}
mapMSet :: (a -> b) -> MSet a -> MSet b
mapMSet f (MS xs) = MS [(f x, n) | (x, n) <- xs]

-- mapMSet :: (Monad m, Ord b) => (a -> m b) -> M.Map k a -> m (M.Map k b)
-- mapMSet f m = mapM (\(k, v) -> do { x <- f v; return (k, x)}) (M.toList m) >>= return . M.fromList

-- mapM :: (Monad m, Traversable t) => (a -> m b) -> t a -> m (t b)
-- mapM f xs = sequenceA (fmap f xs)

{--
Why it is not possible to define an instance of Functor for MSet by providing mapMSet as the implementation of fmap
-------------------------------------------------------------------------------------------------------------------
mapMSet cannot be used to define a Functor instance for MSet is that mapMSet does not satisfy the functor laws, which are necessary for a type to be considered a valid instance of Functor.
In particular, the first functor law states that fmap id = id, meaning that applying the fmap function to the identity function should result in the identity function.
This is not true for mapMSet, as mapMSet id would result in a modified MSet, while id would result in the same MSet.
The second functor law states that fmap (f . g) = fmap f . fmap g, meaning that composing two functions and mapping them onto a functor should be the same as first mapping one function and then the other.
This is also not true for mapMSet, as mapMSet (f . g) would not necessarily be the same as mapMSet f . mapMSet g.
Therefore, mapMSet cannot be used to define a Functor instance for MSet, as it does not satisfy the functor laws.
--}

-- Implementation of the empty constructor
-- It returns a an MSet value with no elements
empty :: MSet a
empty = MS []

{--
Implementation of the add function/ operation

The add function takes an MSet value and an element of any type a,
and returns a new MSet value with the element added to it.
The element is added by increasing its multiplicity by 1 if it is already present in the MSet,
or by inserting it with a multiplicity of 1 if it is not present

The implementation of the add function uses a helper function add'
that takes a list of element-multiplicity pairs and an element v as arguments,
and returns a new list with the element v added to it.
If the element v is already present in the list, its multiplicity is increased by 1.
If the element v is not present in the list, it is inserted with a multiplicity of 1.

The add' function is defined using pattern matching on the input list.
If the input list is empty, add' returns a singleton list containing the element v with a multiplicity of 1.
If the input list is not empty, add' checks if the first element in the list is equal to v using the == operator.
If it is, the element v is replaced with a new element-multiplicity pair where the multiplicity is increased by 1.
If the first element in the list is not equal to v, add' recursively calls itself with the tail of the list and the element v,
and then conses the first element of the list onto the resulting list.

Finally, the add function wraps the result of calling add'
with the input list and element v in an MSet constructor, MS, to produce the final MSet value.
--}

add :: Eq a => MSet a -> a -> MSet a
add (MS xs) v = MS (add' xs v)
  where
    add' [] v = [(v, 1)]
    add' ((w, n) : xs) v
      | v == w = (w, n + 1) : xs
      | otherwise = (w, n) : (add' xs v)

{--
Implementation of the occs function or operation

occs takes an MSet a and an element v of type a and returns the number of occurrences of v in the multiset.
The function first pattern matches on the underlying list of pairs representing the multiset and binds it to xs.
It then defines a helper function occs' that takes this list and the element v.
If the list is empty, occs' returns 0. Otherwise, it checks the first pair in the list.
If the element of the pair is equal to v, it returns the multiplicity of the element.
Otherwise, it recursively calls occs' on the rest of the list.
--}

occs :: Eq a => MSet a -> a -> Int
occs (MS xs) v = occs' xs v
  where
    occs' [] v = 0
    occs' ((w, n) : xs) v
      | v == w = n
      | otherwise = occs' xs v

{--
Implementation of the elems function or operation
-------------------------------------------------------
elems takes an MSet a and returns a list of all the elements in the multiset.
It does this by pattern matching on the underlying list of pairs representing the multiset and binding it to xs.
It then uses list comprehension to create a new list
by taking the first element of each pair and ignoring the second element (the multiplicity).
--}

elems :: MSet a -> [(a, Int)]
elems (MS xs) = xs

{--
Implementation of the subeq function or operation
------------------------------------------------------
subeq is a function that takes two MSets and returns a Bool indicating whether the first MSet is a subset of the second.
It does this by pattern matching on the underlying lists of pairs representing the multisets and binding them to xs and ys, respectively.
It then defines a local function subeq' which takes two lists and recursively checks whether the first list is a subset of the second.

If the first list is empty, then it returns True, because the empty set is a subset of every set.
If the first list is not empty, it checks whether the multiplicity of the first element in the first list is less than or equal to the multiplicity of the same element in the second list.
If it is, then it recursively checks the rest of the first list against the second list.
If it is not, then it returns False, because the element is not present in the second list with the required multiplicity.
The overall subeq function then returns the result of subeq' applied to the two lists.

--}
subeq :: Eq a => MSet a -> MSet a -> Bool
subeq (MS xs) (MS ys) = subeq' xs ys
  where
    subeq' [] ys = True
    subeq' ((x, n) : xs) ys = (occs (MS ys) x) >= n && subeq' xs ys

{--
Implementation of the union function or operation
---------------------------------------------------------
The union function is a function that takes two MSets as input and returns a new MSet that contains all the elements from
both input MSets, with the corresponding multiplicities being the sum of the multiplicities in the input MSets.
The Eq a constraint indicates that the elements in the MSets must belong to a type that is an instance of the Eq class,
which defines equality and inequality for values of that type.

The union function works by first defining a helper function union' that takes two lists of pairs representing the elements
and multiplicities of the input MSets, and returns a list of pairs representing the elements and multiplicities of the output MSet.
If either input list is empty, union' simply returns the other list.
Otherwise, union' takes the head element of the first list and looks up its multiplicity in the second MSet using the occs function.
It then adds this element to the output MSet with a multiplicity that is the sum of its multiplicity in the first MSet and its multiplicity in the second MSet, obtained using occs.
It then recursively calls union' on the tail of the first list and the MSet obtained by adding the head element of the first list to the second MSet.
--}

union :: (Eq a, Ord a) => MSet a -> MSet a -> MSet a
union xs ys = foldr add xs (toList ys)
  where
    add v (MS zs) = MS $ subEq v zs
    subEq v [] = [(v, 1)]
    subEq v ((w, n) : zs)
      | v == w = (w, n + 1) : zs
      | otherwise = (w, n) : (subEq v zs)
