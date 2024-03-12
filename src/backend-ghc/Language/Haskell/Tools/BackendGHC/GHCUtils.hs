{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances, LiberalTypeSynonyms, DataKinds#-}

-- | Utility functions defined on the GHC AST representation.
module Language.Haskell.Tools.BackendGHC.GHCUtils where

-- import Data.Generics.Uniplate.Data ()
-- import Data.List

-- import Bag (Bag, bagToList, unionManyBags)
-- import BasicTypes (SourceText(..))
-- import ConLike (ConLike(..))
-- import Data.Maybe (Maybe(..), listToMaybe)
-- import GHC
-- import Id (Id, mkVanillaGlobal)
-- import OccName (OccName)
-- import Outputable (Outputable(..), showSDocUnsafe)
-- import PatSyn (patSynSig)
-- import RdrName (RdrName, rdrNameOcc, nameRdrName)
-- import SrcLoc
-- import Type (TyThing(..))

-- class (OutputableBndrId name) => GHCName name where
--   rdrName :: IdP name -> RdrName
--   getFromNameUsing :: Applicative f => (Name -> Ghc (f Id)) -> Name -> Ghc (f (IdP name))
--   getBindsAndSigs :: HsValBinds name -> ([LSig name], LHsBinds name)
--   nameFromId :: Id -> IdP name
--   fieldOccToId :: RdrName -> XCFieldOcc name -> IdP name
--   nameIfThereIs :: IdP name -> Maybe Name

-- instance GHCName GhcPs where
--   rdrName = id
--   getFromNameUsing _ n = return $ pure (nameRdrName n)
--   getBindsAndSigs (ValBinds _ binds sigs) = (sigs, binds)
--   getBindsAndSigs _ = error "ValBindsOut: ValBindsOut in parsed source"
--   nameFromId = nameRdrName . getName
--   fieldOccToId rdr _ = rdr
--   nameIfThereIs _ = Nothing

-- occName :: forall n . GHCName n => IdP n -> OccName
-- occName = rdrNameOcc . rdrName @n

-- instance GHCName GhcRn where
--   rdrName = nameRdrName
--   getFromNameUsing f n = fmap (nameFromId @GhcRn) <$> f n
--   getBindsAndSigs (XValBindsLR (NValBinds bindGroups sigs)) = (sigs, unionManyBags (map snd bindGroups))
--   getBindsAndSigs _ = error "getBindsAndSigs: ValBindsIn in renamed source"
--   nameFromId = getName
--   fieldOccToId _ name = name
--   nameIfThereIs name = Just name

-- getFieldOccName :: forall n . GHCName n => Located (FieldOcc n) -> Located (IdP n)
-- getFieldOccName (L l (FieldOcc name (L _ rdr))) = L l (fieldOccToId @n rdr name)

-- getFieldOccName' :: forall n . GHCName n => FieldOcc n -> IdP n
-- getFieldOccName' (FieldOcc name (L _ rdr)) = fieldOccToId @n rdr name

-- -- | Loading ids for top-level ghc names
-- getTopLevelId :: GHC.Name -> Ghc (Maybe GHC.Id)
-- getTopLevelId name =
--     lookupName name >>= \case
--       Just (AnId id) -> return (Just id)
--       Just (AConLike (RealDataCon dc)) -> return $ Just $ mkVanillaGlobal name (dataConUserType dc)
--       Just (AConLike (PatSynCon ps)) -> return $ Just $ mkVanillaGlobal name (createPatSynType ps)
--       Just (ATyCon tc) -> return $ Just $ mkVanillaGlobal name (tyConKind tc)
--       _ -> return Nothing
--   where createPatSynType patSyn = case patSynSig patSyn of (_, _, _, _, args, res) -> mkVisFunTys args res

-- hsGetNames' :: HsHasName a => a -> [GHC.Name]
-- hsGetNames' = map fst . hsGetNames Nothing

-- -- | Get names from the GHC AST
-- class HsHasName a where
--   hsGetNames :: Maybe GHC.Name -> a -> [(GHC.Name, Maybe GHC.Name)]

-- instance HsHasName RdrName where
--   hsGetNames _ _ = []

-- instance HsHasName Name where
--   hsGetNames p n = [(n, p)]

-- instance HsHasName Id where
--   hsGetNames p n = [(getName n, p)]

-- instance HsHasName e => HsHasName [e] where
--   hsGetNames p es = concatMap (hsGetNames p) es

-- instance HsHasName e => HsHasName (Located e) where
--   hsGetNames p (L _ e) = hsGetNames p e

-- instance HsHasName (IdP (GhcPass n)) => HsHasName (HsLocalBinds (GhcPass n)) where
--   hsGetNames p (HsValBinds _ bnds) = hsGetNames p bnds
--   hsGetNames _ _ = []

-- instance (GHCName n, HsHasName (IdP n)) => HsHasName (HsDecl n) where
--   hsGetNames p (TyClD _ tycl) = hsGetNames p tycl
--   hsGetNames p (ValD _ vald) = hsGetNames p vald
--   hsGetNames p (ForD _ ford) = hsGetNames p ford
--   hsGetNames p (InstD _ inst) = hsGetNames p inst
--   hsGetNames _ _ = []

-- instance (GHCName n, HsHasName (IdP n)) => HsHasName (InstDecl n) where
--   hsGetNames p (ClsInstD _ clsInst) = hsGetNames p (cid_datafam_insts clsInst)
--   hsGetNames p (DataFamInstD _ dataFamInst) = hsGetNames p dataFamInst
--   hsGetNames _ _ = []

-- instance (GHCName n, HsHasName (IdP n), HsHasName r) => HsHasName (FamEqn n p r) where
--   hsGetNames p (FamEqn _ id _ _ _ rhs) = hsGetNames p id ++ hsGetNames p rhs

-- instance (GHCName n, HsHasName (IdP n)) => HsHasName (DataFamInstDecl n) where
--   hsGetNames p dfid = hsGetNames p (hsib_body $ dfid_eqn dfid)

-- instance (GHCName n, HsHasName (IdP n)) => HsHasName (TyClGroup n) where
--   hsGetNames p (TyClGroup _ tycls _ _) = hsGetNames p tycls

-- instance (GHCName n, HsHasName (IdP n)) => HsHasName (TyClDecl n) where
--   hsGetNames p (FamDecl _ fd) = hsGetNames p fd
--   hsGetNames p (SynDecl {tcdLName = name}) = hsGetNames p name
--   hsGetNames p (DataDecl {tcdLName = name, tcdDataDefn = datadef})
--     = let n = hsGetNames p name in n ++ hsGetNames (listToMaybe (map fst n)) datadef
--   hsGetNames p (ClassDecl {tcdLName = name, tcdSigs = sigs, tcdATs = typeAssocs})
--     = let n = hsGetNames p name in n ++ hsGetNames (listToMaybe (map fst n)) sigs
--                                      ++ hsGetNames (listToMaybe (map fst n)) typeAssocs

-- instance (GHCName n, HsHasName (IdP n)) => HsHasName (FamilyDecl n) where
--  hsGetNames p (FamilyDecl { fdLName = name }) = hsGetNames p name

-- instance (GHCName n, HsHasName (IdP n)) => HsHasName (HsDataDefn n) where
--   hsGetNames p (HsDataDefn {dd_cons = ctors}) = hsGetNames p ctors

-- instance (GHCName n, HsHasName (IdP n)) => HsHasName (ConDecl n) where
--   hsGetNames p (ConDeclGADT {con_names = names, con_res_ty = (L _ (HsFunTy _ (L _ (HsRecTy _ flds)) _))})
--     = hsGetNames p names ++ hsGetNames p flds
--   hsGetNames p (ConDeclGADT {con_names = names, con_res_ty = (L _ (HsRecTy _ flds))})
--     = hsGetNames p names ++ hsGetNames p flds
--   hsGetNames p (ConDeclGADT {con_names = names}) = hsGetNames p names
--   hsGetNames p (ConDeclH98 {con_name = name, con_args = details})
--     = hsGetNames p name ++ hsGetNames p details

-- instance (GHCName n, HsHasName (IdP n)) => HsHasName (HsConDeclDetails n) where
--   hsGetNames p (RecCon rec) = hsGetNames p rec
--   hsGetNames _ _ = []

-- instance (GHCName n, HsHasName (IdP n)) => HsHasName (ConDeclField n) where
--   hsGetNames p (ConDeclField _ name _ _) = hsGetNames p name

-- instance forall n . (GHCName n, HsHasName (IdP n)) => HsHasName (FieldOcc n) where
--   hsGetNames p fl = case nameIfThereIs @n (getFieldOccName' fl) of Just n -> [(n, p)]
--                                                                    _      -> []

-- instance (GHCName n, HsHasName (IdP n)) => HsHasName (Sig n) where
--   hsGetNames p (TypeSig _ n _) = hsGetNames p n
--   hsGetNames p (ClassOpSig _ _ n _) = hsGetNames p n
--   hsGetNames p (PatSynSig _ n _) = hsGetNames p n
--   hsGetNames _ _ = []

-- instance HsHasName (IdP n) => HsHasName (ForeignDecl n) where
--   hsGetNames p (ForeignImport _ n _ _) = hsGetNames p n
--   hsGetNames _ _ = []

-- instance forall n . HsHasName (IdP (GhcPass n)) => HsHasName (HsValBinds (GhcPass n)) where
--   hsGetNames p (ValBinds _ bnds _) = hsGetNames p bnds
--   hsGetNames p (XValBindsLR (NValBinds bnds _ :: NHsValBindsLR (GhcPass n))) = hsGetNames p $ map snd bnds

-- instance HsHasName n => HsHasName (Bag n) where
--   hsGetNames p = hsGetNames p . bagToList

-- instance HsHasName (IdP n) => HsHasName (HsBind n) where
--   hsGetNames p (FunBind {fun_id = lname}) = hsGetNames p lname
--   hsGetNames p (PatBind {pat_lhs = pat}) = hsGetNames p pat
--   hsGetNames p (VarBind {var_id = id}) = hsGetNames p id
--   hsGetNames p (PatSynBind _ (PSB {psb_id = id})) = hsGetNames p id
--   hsGetNames _ _ = error "hsGetNames: called on compiler-generated binding"

-- instance HsHasName (IdP n) => HsHasName (ParStmtBlock l n) where
--   hsGetNames p (ParStmtBlock _ _ binds _) = hsGetNames p binds

-- --instance HsHasName n => HsHasName (LHsTyVarBndrs n) where
-- --  hsGetNames (HsQTvs kvs tvs) = hsGetNames kvs ++ hsGetNames tvs

-- instance HsHasName (IdP n) => HsHasName (HsTyVarBndr n) where
--   hsGetNames p (UserTyVar _ n) = hsGetNames p n
--   hsGetNames p (KindedTyVar _ n _) = hsGetNames p n
--   hsGetNames _ _ = []

-- instance HsHasName (IdP n) => HsHasName (Match n b) where
--   hsGetNames p (Match _ _ pats _) = concatMap (hsGetNames p) pats

-- instance HsHasName (IdP (GhcPass n)) => HsHasName (StmtLR (GhcPass n) (GhcPass n) b) where
--   hsGetNames p (LetStmt _ binds) = hsGetNames p binds
--   hsGetNames p (BindStmt _ pat _ _ _) = hsGetNames p pat
--   hsGetNames p (RecStmt {recS_rec_ids = ids}) = hsGetNames p ids
--   hsGetNames _ _ = []

-- instance HsHasName (IdP n) => HsHasName (Pat n) where
--   hsGetNames x (VarPat _ id) = hsGetNames x id
--   hsGetNames x (LazyPat _ p) = hsGetNames x p
--   hsGetNames x (AsPat _ lname p) = hsGetNames x lname ++ hsGetNames x p
--   hsGetNames x (ParPat _ p) = hsGetNames x p
--   hsGetNames x (BangPat _ p) = hsGetNames x p
--   hsGetNames x (ListPat _ pats) = concatMap (hsGetNames x) pats
--   hsGetNames x (TuplePat _ pats _) = concatMap (hsGetNames x) pats
--   hsGetNames x (ConPatIn _ details) = concatMap (hsGetNames x) (hsConPatArgs details)
--   hsGetNames x (ConPatOut {pat_args = details}) = concatMap (hsGetNames x) (hsConPatArgs details)
--   hsGetNames x (ViewPat _ _ p) = hsGetNames x p
--   hsGetNames x (NPlusKPat _ lname _ _ _ _) = hsGetNames x lname
--   hsGetNames x (SigPat _ p _) = hsGetNames x p
--   hsGetNames _ _ = []

-- instance (GHCName (GhcPass n), HsHasName (IdP (GhcPass n))) => HsHasName (HsGroup (GhcPass n)) where
--   hsGetNames p g@(HsGroup _ vals _ clds _ _ _ foreigns _ _ _ _)
--     = hsGetNames p vals ++ hsGetNames p clds ++ hsGetNames p (hsGroupInstDecls g) ++ hsGetNames p foreigns

-- -- instance (GHCName n, HsHasName (IdP n)) => HsHasName (DefaultDecl n) where
-- --   hsGetNames p (DefaultDecl _ ts) = hsGetNames p ts
-- --   hsGetNames _ _ = []

-- -- | Get the original form of a name
-- rdrNameStr :: RdrName -> String
-- rdrNameStr name = showSDocUnsafe $ ppr name


-- class FromGHCName n where
--   fromGHCName :: GHC.Name -> n

-- instance FromGHCName RdrName where
--   fromGHCName = rdrName @GhcRn

-- instance FromGHCName GHC.Name where
--   fromGHCName = id



-- {-
-- -- | Tries to simplify the type that has HsAppsTy before renaming. Does not always provide the correct form.
-- -- Treats each operator as if they are of equivalent precedence and always left-associative.
-- cleanHsType :: forall n . (OutputableBndrId n {-, SourceTextX n -}) => HsType n -> HsType n
-- -- for some reason * is considered infix
-- cleanHsType (HsAppsTy apps) = unLoc $ guessType apps
--   where guessType :: OutputableBndrId n => [LHsAppType n] -> LHsType n
--         guessType (L l (HsAppInfix n) : rest) -- must be a prefix actually
--           = guessType' (L l (HsTyVar NotPromoted n)) rest
--         guessType (L _ (HsAppPrefix t) : rest) = guessType' t rest
--         guessType [] = error $ "guessType: empty" ++ showSDocUnsafe (ppr apps)

--         guessType' :: LHsType n -> [LHsAppType n] -> LHsType n
--         guessType' fun (L _ (HsAppPrefix t) : rest) = guessType' (hsAppTy fun t) rest
--         guessType' fun (L l (HsAppInfix n) : rest)
--           -- TODO: find a better check
--           | showSDocUnsafe (ppr n) == "*" = guessType' (hsAppTy fun (L l (HsTyVar NotPromoted n))) rest
--         guessType' left (L _ (HsAppInfix n) : right) = hsOpTy left n (guessType right)
--         guessType' t [] = t

--         hsAppTy :: LHsType n -> LHsType n -> LHsType n
--         hsAppTy t1 t2 = L (getLoc t1 `combineSrcSpans` getLoc t2) $ HsAppTy t1 t2

--         hsOpTy :: LHsType n -> Located (IdP n) -> LHsType n -> LHsType n
--         hsOpTy t1 n t2 = L (getLoc t1 `combineSrcSpans` getLoc t2) $ HsOpTy t1 n t2
-- cleanHsType t = t
-- -}

-- mergeFixityDefs :: [Located (FixitySig n)] -> [Located (FixitySig n)]
-- mergeFixityDefs (s@(L l _) : rest)
--   = let (same, different) = partition ((== l) . getLoc) rest
--      in foldl mergeWith s (map unLoc same) : mergeFixityDefs different
--   where mergeWith (L l (FixitySig x names fixity)) (FixitySig _ otherNames _) = L l (FixitySig x (names ++ otherNames) fixity)
-- mergeFixityDefs [] = []

-- getGroupRange :: HsGroup (GhcPass n) -> SrcSpan
-- getGroupRange (HsGroup {..})
--   = foldr combineSrcSpans noSrcSpan locs
--   where locs = [getHsValRange hs_valds] ++ map getLoc hs_splcds ++ map getLoc (concatMap group_tyclds hs_tyclds)
--                  ++ map getLoc (concatMap group_roles hs_tyclds)
--                  ++ map getLoc hs_derivds ++ map getLoc hs_fixds ++ map getLoc hs_defds
--                  ++ map getLoc hs_fords ++ map getLoc hs_warnds ++ map getLoc hs_annds ++ map getLoc hs_ruleds
--                  ++ map getLoc hs_docs

-- getHsValRange :: HsValBinds (GhcPass n) -> SrcSpan
-- getHsValRange (ValBinds _ vals sig) = foldr combineSrcSpans noSrcSpan $ map getLoc (bagToList vals) ++ map getLoc sig
-- getHsValRange ((XValBindsLR (NValBinds vals sig))) = foldr combineSrcSpans noSrcSpan $ concatMap (map getLoc . bagToList . snd) vals ++ map getLoc sig

-- fromSrcText :: SourceText -> String
-- fromSrcText (SourceText s) = s
-- fromSrcText NoSourceText = ""

import Data.Generics.Uniplate.Data ()
import Data.List

import Bag (Bag, bagToList, unionManyBags)
import BasicTypes (SourceText(..))
import ConLike (ConLike(..))
import Data.Maybe (Maybe(..), listToMaybe)
import GHC
import Id (Id, mkVanillaGlobal)
import OccName (OccName)
import Outputable (Outputable(..), showSDocUnsafe)
import PatSyn (patSynSig)
import RdrName (RdrName, rdrNameOcc, nameRdrName)
import SrcLoc
import Type (TyThing(..))
import TyCoRep as GHC


class (OutputableBndrId name) => GHCName name where
  rdrName :: IdP (GhcPass name) -> RdrName
  getFromNameUsing :: Applicative f => (Name -> Ghc (f Id)) -> Name -> Ghc (f (IdP (GhcPass name)))
  getBindsAndSigs :: HsValBinds (GhcPass name) -> ([LSig (GhcPass name)], LHsBinds (GhcPass name))
  nameFromId :: Id -> IdP (GhcPass name)
  fieldOccToId :: RdrName -> XCFieldOcc (GhcPass name) -> IdP (GhcPass name)
  nameIfThereIs :: IdP (GhcPass name) -> Maybe Name

instance GHCName Parsed where
  rdrName = id
  getFromNameUsing _ n = return $ pure (nameRdrName n)
  getBindsAndSigs (ValBinds _ binds sigs) = (sigs, binds)
  getBindsAndSigs _ = error "ValBindsOut: ValBindsOut in parsed source"
  nameFromId = nameRdrName . getName
  fieldOccToId rdr _ = rdr
  nameIfThereIs _ = Nothing

occName :: forall n . GHCName n => IdP (GhcPass n) -> OccName
occName = rdrNameOcc . rdrName @n

instance GHCName Renamed where
  rdrName = nameRdrName
  getFromNameUsing f n = fmap (nameFromId @Renamed) <$> f n
  getBindsAndSigs (XValBindsLR (NValBinds bindGroups sigs)) = (sigs, unionManyBags (map snd bindGroups))
  getBindsAndSigs _ = error "getBindsAndSigs: ValBindsIn in renamed source"
  nameFromId = getName
  fieldOccToId _ name = name
  nameIfThereIs name = Just name

getFieldOccName :: forall n . GHCName n => Located (FieldOcc (GhcPass n)) -> Located (IdP (GhcPass n))
getFieldOccName (L l (FieldOcc name (L _ rdr))) = L l (fieldOccToId @n rdr name)

getFieldOccName' :: forall n . GHCName n => FieldOcc (GhcPass n) -> IdP (GhcPass n)
getFieldOccName' (FieldOcc name (L _ rdr)) = fieldOccToId @n rdr name

-- | Loading ids for top-level ghc names
getTopLevelId :: GHC.Name -> Ghc (Maybe GHC.Id)
getTopLevelId name =
    lookupName name >>= \case
      Just (AnId id) -> return (Just id)
      Just (AConLike (RealDataCon dc)) -> return $ Just $ mkVanillaGlobal name (dataConUserType dc)
      Just (AConLike (PatSynCon ps)) -> return $ Just $ mkVanillaGlobal name (createPatSynType ps)
      Just (ATyCon tc) -> return $ Just $ mkVanillaGlobal name (tyConKind tc)
      _ -> return Nothing
  where createPatSynType patSyn = case patSynSig patSyn of (_, _, _, _, args, res) -> mkVisFunTys args res

hsGetNames' :: HsHasName a => a -> [GHC.Name]
hsGetNames' = map fst . hsGetNames Nothing

-- | Get names from the GHC AST
class HsHasName a where
  hsGetNames :: Maybe GHC.Name -> a -> [(GHC.Name, Maybe GHC.Name)]

instance HsHasName RdrName where
  hsGetNames _ _ = []

instance HsHasName Name where
  hsGetNames p n = [(n, p)]

instance HsHasName Id where
  hsGetNames p n = [(getName n, p)]

instance HsHasName e => HsHasName [e] where
  hsGetNames p es = concatMap (hsGetNames p) es

instance HsHasName e => HsHasName (Located e) where
  hsGetNames p (L _ e) = hsGetNames p e

instance HsHasName (IdP (GhcPass n)) => HsHasName (HsLocalBinds (GhcPass n)) where
  hsGetNames p (HsValBinds _ bnds) = hsGetNames p bnds
  hsGetNames _ _ = []

instance (GHCName n, HsHasName (IdP (GhcPass n))) => HsHasName (HsDecl (GhcPass n)) where
  hsGetNames p (TyClD _ tycl) = hsGetNames p tycl
  hsGetNames p (ValD _ vald) = hsGetNames p vald
  hsGetNames p (ForD _ ford) = hsGetNames p ford
  hsGetNames p (InstD _ inst) = hsGetNames p inst
  hsGetNames _ _ = []

instance (GHCName n, HsHasName (IdP (GhcPass n))) => HsHasName (InstDecl (GhcPass n)) where
  hsGetNames p (ClsInstD _ clsInst) = hsGetNames p (cid_datafam_insts clsInst)
  hsGetNames p (DataFamInstD _ dataFamInst) = hsGetNames p dataFamInst
  hsGetNames _ _ = []

instance (GHCName n, HsHasName (IdP (GhcPass n)), HsHasName p) => HsHasName (FamEqn (GhcPass n) p ) where
  hsGetNames p (FamEqn _ id _ _ _ rhs) = hsGetNames p id ++ hsGetNames p rhs

instance (GHCName n, HsHasName (IdP (GhcPass n))) => HsHasName (DataFamInstDecl (GhcPass n)) where
  hsGetNames p dfid = hsGetNames p (hsib_body $ dfid_eqn dfid)

instance (GHCName n, HsHasName (IdP (GhcPass n))) => HsHasName (TyClGroup (GhcPass n)) where
  hsGetNames p (TyClGroup _ tycls _ _ _) = hsGetNames p tycls

instance (GHCName n, HsHasName (IdP (GhcPass n))) => HsHasName (TyClDecl (GhcPass n)) where
  hsGetNames p (FamDecl _ fd) = hsGetNames p fd
  hsGetNames p (SynDecl {tcdLName = name}) = hsGetNames p name
  hsGetNames p (DataDecl {tcdLName = name, tcdDataDefn = datadef})
    = let n = hsGetNames p name in n ++ hsGetNames (listToMaybe (map fst n)) datadef
  hsGetNames p (ClassDecl {tcdLName = name, tcdSigs = sigs, tcdATs = typeAssocs})
    = let n = hsGetNames p name in n ++ hsGetNames (listToMaybe (map fst n)) sigs
                                     ++ hsGetNames (listToMaybe (map fst n)) typeAssocs

instance (GHCName n, HsHasName (IdP (GhcPass n))) => HsHasName (FamilyDecl (GhcPass n)) where
 hsGetNames p (FamilyDecl { fdLName = name }) = hsGetNames p name

instance (GHCName n, HsHasName (IdP (GhcPass n))) => HsHasName (HsDataDefn (GhcPass n)) where
  hsGetNames p (HsDataDefn {dd_cons = ctors}) = hsGetNames p ctors

instance (GHCName n, HsHasName (IdP (GhcPass n))) => HsHasName (ConDecl (GhcPass n)) where
  hsGetNames p (ConDeclGADT {con_names = names, con_res_ty = (L _ (HsFunTy _ (L _ (HsRecTy _ flds)) _))})
    = hsGetNames p names ++ hsGetNames p flds
  hsGetNames p (ConDeclGADT {con_names = names, con_res_ty = (L _ (HsRecTy _ flds))})
    = hsGetNames p names ++ hsGetNames p flds
  hsGetNames p (ConDeclGADT {con_names = names}) = hsGetNames p names
  hsGetNames p (ConDeclH98 {con_name = name, con_args = details})
    = hsGetNames p name ++ hsGetNames p details

instance (GHCName n, HsHasName (IdP (GhcPass n))) => HsHasName (HsConDeclDetails (GhcPass n)) where
  hsGetNames p (RecCon rec) = hsGetNames p rec
  hsGetNames _ _ = []

instance (GHCName n, HsHasName (IdP (GhcPass n))) => HsHasName (ConDeclField (GhcPass n)) where
  hsGetNames p (ConDeclField _ name _ _) = hsGetNames p name

instance forall n . (GHCName n, HsHasName (IdP (GhcPass n))) => HsHasName (FieldOcc (GhcPass n)) where
  hsGetNames p fl = case nameIfThereIs @n (getFieldOccName' fl) of Just n -> [(n, p)]
                                                                   _      -> []

instance (GHCName n, HsHasName (IdP (GhcPass n))) => HsHasName (Sig (GhcPass n)) where
  hsGetNames p (TypeSig _ n _) = hsGetNames p n
  hsGetNames p (ClassOpSig _ _ n _) = hsGetNames p n
  hsGetNames p (PatSynSig _ n _) = hsGetNames p n
  hsGetNames _ _ = []

instance HsHasName (IdP n) => HsHasName (ForeignDecl n) where
  hsGetNames p (ForeignImport _ n _ _) = hsGetNames p n
  hsGetNames _ _ = []

instance forall n . HsHasName (IdP (GhcPass n)) => HsHasName (HsValBinds (GhcPass n)) where
  hsGetNames p (ValBinds _ bnds _) = hsGetNames p bnds
  hsGetNames p (XValBindsLR (NValBinds bnds _ :: NHsValBindsLR (GhcPass n))) = hsGetNames p $ map snd bnds

instance HsHasName n => HsHasName (Bag n) where
  hsGetNames p = hsGetNames p . bagToList

instance HsHasName (IdP n) => HsHasName (HsBind n) where
  hsGetNames p (FunBind {fun_id = lname}) = hsGetNames p lname
  -- hsGetNames p (PatBind {pat_lhs = pat}) = hsGetNames p pat
  hsGetNames p (VarBind {var_id = id}) = hsGetNames p id
  hsGetNames p (PatSynBind _ (PSB {psb_id = id})) = hsGetNames p id
  hsGetNames _ _ = error "hsGetNames: called on compiler-generated binding"

instance HsHasName (IdP n) => HsHasName (ParStmtBlock l n) where
  hsGetNames p (ParStmtBlock _ _ binds _) = hsGetNames p binds

--instance HsHasName n => HsHasName (LHsTyVarBndrs n) where
--  hsGetNames (HsQTvs kvs tvs) = hsGetNames kvs ++ hsGetNames tvs

instance HsHasName (IdP n) => HsHasName (HsTyVarBndr n) where
  hsGetNames p (UserTyVar _ n) = hsGetNames p n
  hsGetNames p (KindedTyVar _ n _) = hsGetNames p n
  hsGetNames _ _ = []

instance HsHasName (IdP (GhcPass n)) => HsHasName (Match (GhcPass n) b) where
  hsGetNames p (Match _ _ pats _) = concatMap (\x -> hsGetNames p $ unLoc x) pats

instance HsHasName (IdP (GhcPass n)) => HsHasName (StmtLR (GhcPass n) (GhcPass n) b) where
  hsGetNames p (LetStmt _ binds) = hsGetNames p binds
  hsGetNames p (BindStmt _ pat _ _ _) = hsGetNames p (unLoc pat)
  hsGetNames p (RecStmt {recS_rec_ids = ids}) = hsGetNames p ids
  hsGetNames _ _ = []

instance HsHasName (IdP (GhcPass n)) => HsHasName (Pat (GhcPass n)) where
  hsGetNames x (VarPat _ id) = hsGetNames x id
  hsGetNames x (LazyPat _ p) = hsGetNames x (unLoc p)
  hsGetNames x (AsPat _ lname p) = hsGetNames x lname ++ hsGetNames x p
  hsGetNames x (ParPat _ p) = hsGetNames x p
  hsGetNames x (BangPat _ p) = hsGetNames x p
  hsGetNames x (ListPat _ pats) = concatMap (hsGetNames x) pats
  hsGetNames x (TuplePat _ pats _) = concatMap (hsGetNames x) pats
  hsGetNames x (ConPatIn _ details) = concatMap (hsGetNames x) (hsConPatArgs details)
  hsGetNames x (ConPatOut {pat_args = details}) = concatMap (hsGetNames x) (hsConPatArgs details)
  hsGetNames x (ViewPat _ _ p) = hsGetNames x p
  hsGetNames x (NPlusKPat _ lname _ _ _ _) = hsGetNames x lname
  hsGetNames x (SigPat _ p _) = hsGetNames x p
  hsGetNames _ _ = []

instance (GHCName n, HsHasName (IdP (GhcPass n))) => HsHasName (HsGroup (GhcPass n)) where
  hsGetNames p g@(HsGroup _ vals _ clds _ _ _ foreigns _ _ _ _)
    = hsGetNames p vals ++ hsGetNames p clds ++ hsGetNames p (hsGroupInstDecls g) ++ hsGetNames p foreigns

-- instance (GHCName n, HsHasName (IdP n)) => HsHasName (DefaultDecl n) where
--   hsGetNames p (DefaultDecl _ ts) = hsGetNames p ts
--   hsGetNames _ _ = []

-- | Get the original form of a name
rdrNameStr :: RdrName -> String
rdrNameStr name = showSDocUnsafe $ ppr name


class FromGHCName n where
  fromGHCName :: GHC.Name -> n

instance FromGHCName RdrName where
  fromGHCName = rdrName @Renamed

instance FromGHCName GHC.Name where
  fromGHCName = id



{-
-- | Tries to simplify the type that has HsAppsTy before renaming. Does not always provide the correct form.
-- Treats each operator as if they are of equivalent precedence and always left-associative.
cleanHsType :: forall n . (OutputableBndrId n {-, SourceTextX n -}) => HsType n -> HsType n
-- for some reason * is considered infix
cleanHsType (HsAppsTy apps) = unLoc $ guessType apps
  where guessType :: OutputableBndrId n => [LHsAppType n] -> LHsType n
        guessType (L l (HsAppInfix n) : rest) -- must be a prefix actually
          = guessType' (L l (HsTyVar NotPromoted n)) rest
        guessType (L _ (HsAppPrefix t) : rest) = guessType' t rest
        guessType [] = error $ "guessType: empty" ++ showSDocUnsafe (ppr apps)

        guessType' :: LHsType n -> [LHsAppType n] -> LHsType n
        guessType' fun (L _ (HsAppPrefix t) : rest) = guessType' (hsAppTy fun t) rest
        guessType' fun (L l (HsAppInfix n) : rest)
          -- TODO: find a better check
          | showSDocUnsafe (ppr n) == "*" = guessType' (hsAppTy fun (L l (HsTyVar NotPromoted n))) rest
        guessType' left (L _ (HsAppInfix n) : right) = hsOpTy left n (guessType right)
        guessType' t [] = t

        hsAppTy :: LHsType n -> LHsType n -> LHsType n
        hsAppTy t1 t2 = L (getLoc t1 `combineSrcSpans` getLoc t2) $ HsAppTy t1 t2

        hsOpTy :: LHsType n -> Located (IdP n) -> LHsType n -> LHsType n
        hsOpTy t1 n t2 = L (getLoc t1 `combineSrcSpans` getLoc t2) $ HsOpTy t1 n t2
cleanHsType t = t
-}

mergeFixityDefs :: [Located (FixitySig n)] -> [Located (FixitySig n)]
mergeFixityDefs (s@(L l _) : rest)
  = let (same, different) = partition ((== l) . getLoc) rest
     in foldl mergeWith s (map unLoc same) : mergeFixityDefs different
  where mergeWith (L l (FixitySig x names fixity)) (FixitySig _ otherNames _) = L l (FixitySig x (names ++ otherNames) fixity)
mergeFixityDefs [] = []

getGroupRange :: HsGroup (GhcPass n) -> SrcSpan
getGroupRange (HsGroup {..})
  = foldr combineSrcSpans noSrcSpan locs
  where locs = [getHsValRange hs_valds] ++ map getLoc hs_splcds ++ map getLoc (concatMap group_tyclds hs_tyclds)
                 ++ map getLoc (concatMap group_roles hs_tyclds)
                 ++ map getLoc hs_derivds ++ map getLoc hs_fixds ++ map getLoc hs_defds
                 ++ map getLoc hs_fords ++ map getLoc hs_warnds ++ map getLoc hs_annds ++ map getLoc hs_ruleds
                 ++ map getLoc hs_docs

getHsValRange :: HsValBinds (GhcPass n) -> SrcSpan
getHsValRange (ValBinds _ vals sig) = foldr combineSrcSpans noSrcSpan $ map getLoc (bagToList vals) ++ map getLoc sig
getHsValRange ((XValBindsLR (NValBinds vals sig))) = foldr combineSrcSpans noSrcSpan $ concatMap (map getLoc . bagToList . snd) vals ++ map getLoc sig

fromSrcText :: SourceText -> String
fromSrcText (SourceText s) = s
fromSrcText NoSourceText = ""
