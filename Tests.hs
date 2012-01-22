{-# LANGUAGE DatatypeContexts
           , FlexibleInstances
           , KindSignatures
           , MultiParamTypeClasses
           , TemplateHaskell
           , TypeFamilies
           , UndecidableInstances
  #-}

{- output of "runhaskell -ddump-splices Tests.hs"

Tests.hs:1:14:
    Warning: -XDatatypeContexts is deprecated: It was widely considered a misfeature, and has been removed from the Haskell language.
Tests.hs:1:1: Splicing declarations
    mkNewTypes ['Yarn, 'Occasionally, 'ShowNum, 'Kinda, 'CartesianList]
  ======>
    Tests.hs:31:3-75
    instance Newtype Yarn String where
        { unpack (Yarn x) = x
          pack = Yarn }
    instance Newtype (Occasionally a_a2fx) (Maybe a_a2fx) where
        { unpack (Occasionally x) = x
          pack = Occasionally }
    instance (Num a_a2fv, Show b_a2fw) =>
             Newtype (ShowNum a_a2fv b_a2fw) (a_a2fv -> b_a2fw) where
        { unpack (ShowNum x) = x
          pack = ShowNum }
    instance Newtype (Kinda (a_a2ft :: * -> *) b_a2fu) b_a2fu where
        { unpack (Kinda x) = x
          pack = Kinda }
    instance Newtype (CartesianList a_a2fs) [[a_a2fs]] where
        { unpack (CartesianList x) = x
          pack = CartesianList }
Tests.hs:1:1: Splicing declarations
    mkNewTypes ['Familial1, 'Familial2, 'Familial3]
  ======>
    Tests.hs:39:3-52
    instance f_a2w8 ~ (V a_a2un) =>
             Newtype (Familial1 a_a2un) f_a2w8 where
        { unpack (Familial1 x) = x
          pack = Familial1 }
    instance f_a2w9 ~ (V a_a2um) =>
             Newtype (Familial2 a_a2um) (f_a2w9, f_a2w9) where
        { unpack (Familial2 x) = x
          pack = Familial2 }
    instance (f_a2wa ~ (V a_a2ul), f_a2wb ~ (V (V a_a2ul))) =>
             Newtype (Familial3 a_a2ul) (f_a2wa, f_a2wb) where
        { unpack (Familial3 x) = x
          pack = Familial3 }

"ball"
Just "ice"
"42"
5
[[[4,1,0],[4,2,0],[5,1,0],[5,2,0],[6,1,0],[6,2,0]]]
-}

import Control.Newtype
import Control.Newtype.TH
import Data.Monoid
import Data.Foldable (fold)

newtype Yarn = Yarn String

newtype Occasionally a = Occasionally (Maybe a)

-- Test that 'DatatypeContexts' are used as class constraints, despite this
-- extension being deprecated.
newtype (Num a, Show b) => ShowNum a b = ShowNum { pump :: a -> b }

newtype Kinda (a :: * -> *) (b :: *) = Kinda b

newtype CartesianList a = CartesianList [[a]] deriving Show

instance Monoid (CartesianList a) where
  mempty = pack [[]]
  a `mappend` b = pack [x ++ y | x <- unpack a, y <- unpack b]

$(mkNewTypes [''Yarn, ''Occasionally, ''ShowNum , ''Kinda, ''CartesianList])

type family V a :: *
type instance V [a] = V a
newtype Familial1 a = Familial1 (V a)
newtype Familial2 a = Familial2 (V a, V a)
newtype Familial3 a = Familial3 (V a, V (V a))

$(mkNewTypes [''Familial1, ''Familial2, ''Familial3])

pun :: (Newtype a b, Show b) => a -> IO ()
pun = print . unpack

klift :: Int -> Kinda Maybe Int
klift = Kinda

single x = [x]

main :: IO ()
main = do
  -- Let's see if we can use them. Going to assume that pack works..
  pun $ Yarn "ball"
  pun . Occasionally $ Just "ice"
  print $ unpack (ShowNum show) 42
  pun . klift $ 5
  print $ underF CartesianList (\xs -> [fold xs])
    [[[4],[5],[6]], [[1],[2]], [[0]]]
