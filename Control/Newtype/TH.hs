{-# LANGUAGE TemplateHaskell, TupleSections #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Newtype.TH
-- Copyright   :  Michael Sloan 2011
--
-- Maintainer  :  Michael Sloan (mgsloan@gmail.com)
-- Portability :  unportable
--
-- This module provides a template Haskell based mechanism for deriving
-- instances of the @Newtype@ class, defined in @ Control.Newtype @ in the
-- newtype package.  Example usage:
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
--
-----------------------------------------------------------------------------

module Control.Newtype.TH ( mkNewTypes ) where

import Control.Newtype ( Newtype(pack, unpack) )

import Control.Applicative   ((<$>))
import Control.Arrow         ((&&&), (***))
import Data.Function         ( on )
import Data.List             ( groupBy, sortBy, find, nub )
import Data.Maybe            ( catMaybes )
import Data.Ord              ( comparing )
import Data.Generics         ( Data(gmapQ) )
import Data.Generics.Schemes ( everywhere' )
import Data.Generics.Aliases ( extT, extQ )

import Language.Haskell.TH
import Language.Haskell.Meta.Utils (conName, conTypes)

-- | Derive instances of @Newtype@, specified as a list of references to newtypes.
mkNewTypes :: [Name] -> Q [Dec]
mkNewTypes = mapM mkInst
  where
    mkInst name = rewriteFamilies =<< mkInstH name <$> reify name
    mkInstH name (TyConI (NewtypeD context _ vs con _)) =
      -- Construct the instance declaration
      -- "instance Newtype (<newtype> a ...) (<field type> a ...) where"
      InstanceD context
        (foldl1 AppT [ConT ''Newtype, bndrsToType (ConT name) vs, head $ conTypes con])
        (defs (conName con))
    mkInstH name _ = error $ show name ++ " is not a Newtype"
    defs cname =
      [ FunD 'unpack [Clause [ConP cname [VarP xname]] (NormalB $ VarE xname) []]
      , FunD 'pack   [Clause [] (NormalB (ConE cname)) []]
      ]
    xname = mkName "x"

-- Given a root type and a list of type variables, converts for use as
-- parameters to the newtype's type in the instance head.
bndrsToType :: Type -> [TyVarBndr] -> Type
bndrsToType = foldl (\x y -> AppT x $ bndrToType y)

-- This converts a type variable binding to a type.  Preserving kind
-- signatures is probably unnecessary, but we might as well.
bndrToType :: TyVarBndr -> Type
bndrToType (PlainTV x) = VarT x
bndrToType (KindedTV x k) = SigT (VarT x) k

-- This rewrites type family instances to equality constraints.
rewriteFamilies :: Dec -> Q Dec
rewriteFamilies (InstanceD preds ity ds) = do
  -- Infos of every type constructor that's applied to something else.
  infos <- mapM (\(n, t) -> (n, t, ) <$> reify n) $ apps ity
  -- Each one that's determined to be a family constraint.
  fams <- mapM (\(ns, t) -> (ns, t, ) . VarT <$> newName "f")
  -- Merge all of the identical applications of the family constructor.
        . map (nub . map snd &&& (snd . fst . head))
        . groupBy ((==) `on` fst)
        . sortBy (comparing ((id *** show) . fst)) 
        . catMaybes $ map process infos
  -- Build resulting instance
  -- TODO: consider substituting into other predicates too?
  return $ InstanceD (preds' fams) (ity' fams) ds
 where
  process (n, t, TyConI (FamilyD _ n' _ _)) = Just ((n', t), n)
  process _ = Nothing

  preds' fams = map (\((n:_),  t, v) -> EqualP v (AppT (ConT n) t)) fams ++ preds

  ity' :: [([Name], Type, Type)] -> Type
  ity' fams = everywhere' (id `extT` handleType) ity
   where
    handleType :: Type -> Type
    handleType app@(AppT (ConT n) r)
      = case find (\(ns, t, _) -> n `elem` ns && t == r) fams of
          Just (_, _, v) -> v
          Nothing -> app
    handleType t = t

  apps :: Type -> [(Name, Type)]
  apps = handleType
   where
    handleType :: Type -> [(Name, Type)]
    handleType (AppT (ConT v) r) = (v, r) : handleType r
    handleType t = generic t
    generic :: Data a => a -> [(Name, Type)]
    generic = concat . gmapQ (const [] `extQ` handleType)

rewriteFamilies d = return d