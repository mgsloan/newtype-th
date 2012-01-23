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
import Control.Arrow         ((&&&))
import Data.Function         ( on )
import Data.List             ( groupBy, sortBy, find, nub )
import Data.Maybe            ( catMaybes )
import Data.Ord              ( comparing )
import Data.Generics         ( Data(gmapQ) )
import Data.Generics.Schemes ( everywhere' )
import Data.Generics.Aliases ( extT, extQ )

import Language.Haskell.TH
import Language.Haskell.Meta.Utils (conName, conTypes)

-- | Derive instances of @Newtype@, specified as a list of references
--   to newtypes.
mkNewTypes :: [Name] -> Q [Dec]
mkNewTypes = mapM (\n -> rewriteFamilies =<< mkInst <$> reify n)
 where
  mkInst (TyConI (NewtypeD a b c  d  _)) = mkInstFor a b c d
  mkInst (TyConI (DataD    a b c [d] _)) = mkInstFor a b c d
  mkInst x
    = error $ show x
    ++ " is not a Newtype or single-field single-constructor datatype."
  
--Construct the instance declaration
-- "instance Newtype (<newtype> a ...) (<field type> a ...) where"
  mkInstFor context name bnds con
    = InstanceD context
    ( foldl1 AppT [ ConT ''Newtype
                  , bndrsToType (ConT name) bnds
                  , head $ conTypes con
                  ] )
    [ FunD 'pack   [Clause []                  (NormalB $ ConE cn) []]
    , FunD 'unpack [Clause [ConP cn [VarP xn]] (NormalB $ VarE xn) []]
    ]
   where
    cn = conName con
    xn = mkName "x"

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
  -- Every unique family constraint found, each with a new name.
  fams <- mapM (\(ns, t) -> (ns, t, ) . VarT <$> newName "f")
        . mergeApps . catMaybes $ map justFamily infos
  -- Build resulting instance.
  return $ InstanceD (preds' fams) (ity' fams) ds
 where
-- Selects for just family declarations, and yields the name used to
-- refer to it, along with the cannonical reified name and the passed
-- type.
  justFamily :: (Name, Type, Info) -> Maybe (Name, (Name, Type))
#if __GLASGOW_HASKELL__ >= 704
  justFamily (n, t, FamilyI (FamilyD _ n' _ _) _) = Just (n, (n', t))
#else
  justFamily (n, t, TyConI (FamilyD _ n' _ _)) = Just (n, (n', t))
#endif
  justFamily _ = Nothing

-- Merges all of the identical applications of the family constructor.
  mergeApps :: [(Name, (Name, Type))] -> [([Name], Type)]
  mergeApps = map (nub . map fst &&& (snd . snd . head))
            . groupBy ((==) `on` snd) . sortBy (comparing snd) 

  preds' = (preds ++)
         . map (\((n:_),  t, v) -> EqualP v (AppT (ConT n) t))

  ity' :: [([Name], Type, Type)] -> Type
  ity' fams = everywhere' (id `extT` handleType) ity
   where
    handleType :: Type -> Type
    handleType app@(AppT (ConT n) r)
      = case find (\(ns, t, _) -> n `elem` ns && t == r) fams of
          Just (_, _, v) -> v
          Nothing -> app
    handleType t = t

  -- Enumerates all of the found instances of an application of a
  -- type constructor.
  apps :: Type -> [(Name, Type)]
  apps = handleType
   where
    handleType :: Type -> [(Name, Type)]
    handleType (AppT (ConT v) r) = (v, r) : handleType r
    handleType (AppT (SigT t _) r) = handleType (AppT t r)
--TODO: any conceivable reason to special-case (AppT (ForallT ...)) ?
--    handleType (AppT (SigT t) r) = 
    handleType t = generic t
    generic :: Data a => a -> [(Name, Type)]
    generic = concat . gmapQ (const [] `extQ` handleType)

rewriteFamilies d = return d