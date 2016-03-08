{-|
Module      : Language.MASMGen.Core
Description : Provides the core functions of the library
Copyright   : (c) Ruey-Lin Hsu (petercommand)
License     : LGPL-3
Maintainer  : petercommand@gmail.com
Stability   : provisional
Portability : portable
|-}
module Language.MASMGen.Core ( newGlobalVar
                             , mkFunc
                             , initFuncState
                             , initProgState
                             , section
                             , output
                             , produceAsm
                             , produceAsmOptions
                             , produceAsmInclude
                             , produceAsmProg
                             , produceAsmGlobalVarMap
                             , produceAsmFuncs
                             , printShowableInstr
                             , add
                             , addb
                             , addw
                             , addl
                             , sub
                             , subb
                             , subw
                             , subl
                             , imul
                             , idiv
                             , inc
                             , dec
                             , mov
                             , movb
                             , movw
                             , movl
                             , goto
                             , push
                             , pushl
                             , pop
                             , popl
                             , label
                             , comment
                             )

                                   where
import Language.MASMGen.Types
import qualified Data.Map as M
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy

import Data.List
import Data.Word


mkFunc :: String -> MASMFuncM () -> MASMProgM ()
mkFunc name thisFunc = do
  f <- gets funcs
  modify $ \s -> s { funcs = Func (execState thisFunc (initFuncState name)) : f }

newGlobalVar :: String -> MASMVar -> MASMProgM Var
newGlobalVar name value = do
  map <- gets globalVarMap
  modify $ \s -> s { globalVarMap = M.insert name value map }  
  return $ mkVar name (fst value)


initFuncState :: String -> MASMFunc
initFuncState s = MASMFunc { funcName = s
                           , instrs = []
                           }
initProgState :: MASMProg
initProgState = MASMProg { globalVarMap = M.empty
                         , funcs = []
                         }

           

section :: String -> Writer [MASMOutput] ()
section x = stell $ MASMOutput $ '.' : x 

output :: [MASMOutput] -> [String]
output x = let output' :: Int -> [MASMOutput] -> [String]
               output' indent (y:ys) = case y of
                                         MASMOutput str -> (replicate indent ' ') <> str : output' indent ys
                                         MASMOutputNoIndent str -> str : output' indent ys
                                         Indent -> output' (indent + 4) ys
                                         Dedent -> case indent - 4 >= 0 of
                                                     True -> output' (indent - 4) ys
                                                     False -> output' 0 ys
                                         NewLine -> "" : output' indent ys
               output' _ [] = []
           in output' 0 x
produceAsm :: MASM -> Writer [MASMOutput] ()
produceAsm (MASM { masmProgMode = progMode
                 , masmProgOptions = progOptions
                 , masmInclude = include
                 , masmProg = prog
                 }) = do
  stell $ MASMOutput $ case progMode of
                                 Mode386 -> ".386"
                                 Mode486 -> ".486"
                                 Mode586 -> ".586"
                                 Mode686 -> ".686"
  produceAsmOptions progOptions
  produceAsmInclude include
  produceAsmProg prog

produceAsmOptions :: [String] -> Writer [MASMOutput] ()
produceAsmOptions = tell . map (MASMOutput . ("option " <>))

produceAsmInclude :: [MASMInclude] -> Writer [MASMOutput] ()
produceAsmInclude = tell . map (\item -> MASMOutput (case item of
                                                       MASMInclude a -> "include " <> a
                                                       MASMIncludeLib a -> "includelib " <> a))
                                            
produceAsmProg :: MASMProgM () -> Writer [MASMOutput] ()
produceAsmProg prog = let finalProg = execState prog initProgState
                      in do
                        section "DATA"
                        produceAsmGlobalVarMap $ globalVarMap finalProg
                        section "CODE"
                        produceAsmFuncs $ reverse $ funcs finalProg

produceAsmGlobalVarMap :: MASMVarMap -> Writer [MASMOutput] ()
produceAsmGlobalVarMap varMap = let assocsList = M.assocs varMap
                                    printVar :: (String, (MASMType, Maybe [Lit])) -> Writer [MASMOutput] ()
                                    printVar (name, (varType, val)) =
                                        let result = case val of
                                                       Just x -> intersperse ',' (concat . map show $ x)
                                                       Nothing -> "?"
                                        in
                                          stell $ MASMOutput $ name <> " " <> show varType <> " " <> result
                                in do
                                  sequence_ $ map printVar assocsList
produceAsmFuncs :: [MASMTopLevel] -> Writer [MASMOutput] ()
produceAsmFuncs (x:xs) = do
  case x of
    Func func -> let name = funcName func
                     ins = reverse $ instrs func
                 in do
                   stell $ MASMOutput $ name <> " PROC"
                   stell $ Indent
                   sequence_ $ map printShowableInstr ins
                   stell $ Dedent
                   stell $ MASMOutput $ name <> " ENDP"
  stell $ NewLine
  produceAsmFuncs xs
produceAsmFuncs [] = return ()


printShowableInstr :: MASMInstr -> Writer [MASMOutput] ()
printShowableInstr instr = let binOp m x y = stell $ MASMOutput $ m <> " " <> show x <> ", " <> show y
                               sizedBinOp m size x y = case size of
                                                         Just size -> stell $ MASMOutput $ m <> " " <> show size <> " " <> show x <> ", " <> show y
                                                         Nothing -> binOp m x y
                                                                      
                               sinOp m x = stell $ MASMOutput $ m <> " " <> show x
                               sizedSinOp m size x = case size of
                                                       Just size -> stell $ MASMOutput $ m <> " " <> show size <> " " <> show x
                                                       Nothing -> sinOp m x
                                           
                           in case instr of
                                MASMAdd size x y -> sizedBinOp "ADD" size x y
                                MASMSub size x y -> sizedBinOp "SUB" size x y
                                MASMMul size x y -> sizedBinOp "IMUL" size x y
                                MASMDiv size x y -> sizedBinOp "IDIV" size x y
                                MASMInc size x -> sizedSinOp "INC" size x
                                MASMDec size x -> sizedSinOp "DEC" size x
                                MASMMov size x y -> sizedBinOp "MOV" size x y
                                MASMFuncCall name convention _ -> error "func call not implemented"
                                MASMGoto x -> sinOp "GOTO" x
                                MASMLabel x -> stell $ MASMOutputNoIndent $ x <> ":"
                                MASMPush size x -> sizedSinOp "PUSH" size x
                                MASMPop size x -> sizedSinOp "POP" size x
                                MASMComment x -> stell $ MASMOutput $ ';' : x

modFun :: MASMInstr -> MASMFuncM ()
modFun x = modify (\f -> let i = instrs f
                         in f { instrs = x : i })

add :: Operand -> Operand -> MASMFuncM ()
add x y = modFun $ MASMAdd Nothing x y

addb :: Operand -> Operand -> MASMFuncM ()
addb x y = modFun $ MASMAdd (Just DB) x y

addw :: Operand -> Operand -> MASMFuncM ()
addw x y = modFun $ MASMAdd (Just DW) x y

addl :: Operand -> Operand -> MASMFuncM ()
addl x y = modFun $ MASMAdd (Just DD) x y
          
sub :: Operand -> Operand -> MASMFuncM ()
sub x y = modFun $ MASMSub Nothing x y

subb :: Operand -> Operand -> MASMFuncM ()
subb x y = modFun $ MASMSub (Just DB) x y

subw :: Operand -> Operand -> MASMFuncM ()
subw x y = modFun $ MASMSub (Just DW) x y

subl :: Operand -> Operand -> MASMFuncM ()
subl x y = modFun $ MASMSub (Just DD) x y

           
imul :: Operand -> Operand -> MASMFuncM ()
imul x y = modFun $ MASMMul Nothing x y
           
idiv :: Operand -> Operand -> MASMFuncM ()
idiv x y = modFun $ MASMDiv Nothing x y
           
inc :: Operand -> MASMFuncM ()
inc x = modFun $ MASMInc Nothing x

dec :: Operand -> MASMFuncM ()
dec x = modFun $ MASMDec Nothing x

mov :: Operand -> Operand -> MASMFuncM ()
mov x y = modFun $ MASMMov Nothing x y

-- Do not pass the arg 'ty' pointer types, it is designed to use with DB / DW / DD
typedSinOp :: TypedMASMInstrSinCon -> MASMType -> Operand -> MASMFuncM ()
typedSinOp instr ty x = case operandClass x of
                          Pointer -> modFun $ instr (Just (Ptr ty)) x
                          _ -> modFun $ instr (Just ty) x

typedBinOp :: TypedMASMInstrBinCon -> MASMType -> Operand -> Operand -> MASMFuncM ()
typedBinOp instr ty x y = case operandClass x of
                            Pointer -> modFun $ instr (Just (Ptr ty)) x y
                            _ -> modFun $ instr (Just ty) x y
                       
movb :: Operand -> Operand -> MASMFuncM ()
movb = typedBinOp MASMMov DB

movw :: Operand -> Operand -> MASMFuncM ()
movw = typedBinOp MASMMov DW

movl :: Operand -> Operand -> MASMFuncM ()
movl = typedBinOp MASMMov DD

goto :: String -> MASMFuncM ()
goto x = modFun $ MASMGoto x

push :: Operand -> MASMFuncM ()
push x = modFun $ MASMPush Nothing x

pushl :: Operand -> MASMFuncM ()
pushl x = typedSinOp MASMPush DD x
         
pop :: Operand -> MASMFuncM ()
pop x = modFun $ MASMPop Nothing x

popl :: Operand -> MASMFuncM ()
popl x = typedSinOp MASMPop DD x

        
label :: String -> MASMFuncM ()
label x = modFun $ MASMLabel x

comment :: String -> MASMFuncM ()
comment x = modFun $ MASMComment x
          
stell :: (Monad m, Monoid (m a)) => a -> Writer (m a) ()
stell = tell . return


