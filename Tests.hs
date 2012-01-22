{-# LANGUAGE DatatypeContexts
           , FlexibleInstances
           , KindSignatures
           , MultiParamTypeClasses
           , TemplateHaskell
           , TypeFamilies
           , UndecidableInstances
  #-}

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

type family V a :: *
type instance V [a] = V a

newtype Familial a = Familial (V a)

$(mkNewTypes [''Yarn, ''Occasionally, ''ShowNum , ''Kinda, ''CartesianList, ''Familial])

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
