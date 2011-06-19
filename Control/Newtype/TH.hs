{-# LANGUAGE TemplateHaskell #-}

module Control.Newtype.TH (mkNewTypes) where

import Control.Monad (mapM, liftM)

import Language.Haskell.TH
import Language.Haskell.Meta.Utils (conName, conTypes)

mkNewTypes :: [Name] -> Q [Dec]
mkNewTypes = liftM concat . mapM (\nt -> do
  i <- reify nt
  return $ 
    case i of
      TyConI (NewtypeD ct n vs c _) ->
        [makeInstance ct n vs c]
      _ -> []
  )

makeInstance :: Cxt -> Name -> [TyVarBndr] -> Con -> Dec
makeInstance context name vs con = InstanceD context
    (AppT (AppT (ConT $ mkName "Control.Newtype.Newtype")
            $ bndrsToType (ConT name) vs)
      . head $ conTypes con)
    [ FunD (mkName "pack")
        [Clause [] (NormalB $ (ConE cnam)) []]
    , FunD (mkName "unpack")
        [Clause [ConP cnam [VarP xnam]] (NormalB $ VarE xnam) []]
    ]
  where xnam = mkName "x"
        cnam = conName con

bndrToType :: TyVarBndr -> Type
bndrToType (PlainTV x) = VarT x
bndrToType (KindedTV x k) = SigT (VarT x) k

bndrsToType :: Type -> [TyVarBndr] -> Type
bndrsToType = foldl (\x y -> AppT x $ bndrToType y)
