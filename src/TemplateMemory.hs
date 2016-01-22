{-# LANGUAGE TemplateHaskell #-}

module TemplateMemory
 where

 import Types

 import Data.Word
 import Data.Bits
 import Data.Char
 import Data.Array.ST
 import Data.STRef
 
 import Control.Monad
 import Control.Monad.ST

 import Language.Haskell.TH 


 build_address_type :: Name -> Q [Dec]
 build_address_type name = do
   TyConI (DataD _ _ _ [RecC _ fields] _) <- reify name
   let names = map (\(fieldName,_,_) -> mkName $ map toUpper (nameBase fieldName)) fields
   let type_list = map (\typeUpper -> NormalC typeUpper []) names
                   ++ [NormalC (mkName "OneRegister") [(NotStrict, regTyp)],
                       NormalC (mkName "MemAddr") [(NotStrict, wrdTyp)],
                       NormalC (mkName "VRAMAddr") [(NotStrict, wrdTyp)],
                       RecC (mkName "TwoRegister") [(mkName "registerA", NotStrict, regTyp),
                                                    (mkName "registerB", NotStrict, regTyp)]
                      ]
   return [ DataD [] (mkName "Address") [] type_list [] ]
     where regTyp = ConT $ mkName "Register"
           wrdTyp = ConT $ mkName "Word16"
       
 --Fix me!!!
 getRef :: Type -> Q Exp
 getRef (AppT _ (ConT name)) = case (nameBase name) of
   "Word8" -> [| MemVal8 |]
   "Word16" -> [| MemVal16 |]
   "Bool" -> [| Flag |]
   "GPUMode" -> [| Mode |]
   _ -> [| Mode |]


 getTypeName :: Type -> Name
 getTypeName (AppT _ (ConT name)) = mkName $ case (nameBase name) of
   "Word8" -> "MemVal8"
   "Word16" -> "MemVal16"
   "Bool" -> "Flag"
   "GPUMode" -> "Mode"
   
 
 read_expr :: Name -> Type -> Q Exp
 read_expr name typ = [| readSTRef ($(varE name) . memRefs $ $(varE $ mkName "mem") ) >>= \n -> return $ $(getRef typ) n|]

 write_expr :: Name -> Type -> Q Exp
 write_expr name typ = [| writeSTRef ($(varE name) . memRefs $ $(varE $ mkName "mem") ) $(varE $ mkName "w") |]


 build_read_write :: Name -> Q [Dec]
 build_read_write name = do
   TyConI (DataD _ _ _ [RecC _ fields] _) <- reify name
   read_exps  <- mapM (\(fName,_,fType) -> read_expr  fName fType) fields
   write_exps <- mapM (\(fName,_,fType) -> write_expr fName fType) fields
   let names = map fst3 fields
   let types = map thd3 fields
   let readFuns = [ FunD (mkName "readAccess") (toReadClauses names read_exps) ]
   let writeFuns = [FunD (mkName "writeAccess") (toWriteClauses names types write_exps) ]
   return $ readFuns ++ writeFuns
         where toReadClauses names read_exps = (map (\(fName,fExp) ->
                        (Clause
                          [VarP $ mkName "mem", ConP (mkName $ map toUpper (nameBase fName)) []]
                          (NormalB fExp) []
                        )) (zip names read_exps))
               toWriteClauses names types write_exps = (map (\(fName,fType,fExp) -> 
                        (Clause
                          [VarP $ mkName "mem", ConP (mkName $ map toUpper (nameBase fName)) [], 
                           ConP (getTypeName fType) [VarP $ mkName "w"]
                          ]
                          (NormalB fExp) []
                        )) (zip3 names types write_exps))
