{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Newtype.TH
-- Copyright   :  Michael Sloan 2011
--
-- Maintainer  :  Michael Sloan (mgsloan@gmail.com)
-- Portability :  unportable
--
-- This module provides a template Haskell based mechanism for deriving
-- instances of the Newtype class, defined in Control.Newtype.  Example usage:
-- 
-- > newtype CartesianList a = CartesianList [a]
-- > $(mkNewTypes [''CartesianList])
-- >
-- > instance Monoid (CartesianList a) where
-- >   mempty = pack [[]]
-- >   a `mappend` b = pack [x ++ y | x <- unpack a, y <- unpack b]
-- 
-- > *Main> print $ underF CartesianList (\xs -> [fold xs]) ([[[4],[5],[6]], [[1],[2]], [[0]]])
-- > [[[4,1,0],[4,2,0],[5,1,0],[5,2,0],[6,1,0],[6,2,0]]]

module Control.Newtype.TH (mkNewTypes) where

import Language.Haskell.TH
import Language.Haskell.Meta.Utils (conName, conTypes)

import Control.Newtype (Newtype(pack, unpack))

-- | Derive instances of Newtype, specified as a list of references to newtypes.
mkNewTypes :: [Name] -> Q [Dec]
mkNewTypes = mapM mkInst
  where
    mkInst name = fmap (mkInstH name) $ reify name
    mkInstH name (TyConI (NewtypeD context _ vs con _)) =
      -- Construct the instance declaration
      -- "instance Newtype (<newtype> a ...) (<field type> a ...) where"
      InstanceD context
        (foldl1 AppT [ConT ''Newtype, bndrsToType (ConT name) vs, head $ conTypes con])
        (defs (conName con))
    mkInstH name _ = error $ show name ++ " is not a Newtype"
    defs cnam =
      [ FunD 'unpack [Clause [ConP cnam [VarP xnam]] (NormalB $ VarE xnam) []]
      , FunD 'pack   [Clause [] (NormalB (ConE cnam)) []]
      ]
    xnam = mkName "x"

-- Given a root type and a list of type variables, converts for use as
-- parameters to the newtype's type in the instance head.
bndrsToType :: Type -> [TyVarBndr] -> Type
bndrsToType = foldl (\x y -> AppT x $ bndrToType y)

-- This converts a type variable binding to a type.  Preserving kind
-- signatures is probably unnecessary, but we might as well.
bndrToType :: TyVarBndr -> Type
bndrToType (PlainTV x) = VarT x
bndrToType (KindedTV x k) = SigT (VarT x) k
