{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses,
KindSignatures #-}

import Control.Newtype
import Control.Newtype.TH
import Data.Monoid
import Data.Foldable (fold)

newtype Yarn = Yarn [Char]

newtype Occasionally a = Occasionally (Maybe a)

newtype (Num a, Show b) => Endocrine a b = Endocrine { pump :: a -> b }

newtype Kinda (a :: * -> *) (b :: *) = Kinda b

newtype CartesianList a = CartesianList [[a]] deriving Show

instance Monoid (CartesianList a) where
  mempty = pack [[]]
  a `mappend` b = pack [x ++ y | x <- unpack a, y <- unpack b]

$(mkNewTypes [''Yarn, ''Occasionally, ''Endocrine, ''Kinda, ''CartesianList])

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
  print $ unpack (Endocrine show) 42
  pun . klift $ 5
  print $ underF CartesianList (\xs -> [fold xs])
    ([[[4],[5],[6]], [[1],[2]], [[0]]])
