module Core where
import Types
import qualified Data.Map as M
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy

import Data.List
import Data.Word

mkFunc :: String -> MASMFuncM () -> MASMProgM ()
mkFunc name thisFunc = do
  f <- gets funcs
  modify $ \s -> s { funcs = Func (execState thisFunc (initFuncState name)) : f }



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
                                    printVar :: (String, (MASMVarType, [Word8])) -> Writer [MASMOutput] ()
                                    printVar (name, (varType, val)) = stell $ MASMOutput $ name <> " " <> show varType <> " " <> intersperse ',' (concat . map show $ val)
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
                               sinOp m x = stell $ MASMOutput $ m <> " " <> show x
                           in case instr of
                                MASMAdd x y -> binOp "ADD" x y
                                MASMSub x y -> binOp "SUB" x y
                                MASMMul x y -> binOp "IMUL" x y
                                MASMDiv x y -> binOp "IDIV" x y
                                MASMInc x -> sinOp "INC" x
                                MASMDec x -> sinOp "DEC" x
                                MASMMov x y -> binOp "MOV" x y
                                MASMFuncCall name convention _ -> error "func call not implemented"
                                MASMGoto x -> sinOp "GOTO" x
                                MASMLabel x -> stell $ MASMOutputNoIndent $ x <> ":"
                                MASMPush x -> sinOp "PUSH" x
                                MASMPop x -> sinOp "POP" x
                                MASMComment x -> stell $ MASMOutput $ ';' : x

modFun :: MASMInstr -> MASMFuncM ()
modFun x = modify (\f -> let i = instrs f
                         in f { instrs = x : i })

add :: Operand -> Operand -> MASMFuncM ()
add x y = modFun $ MASMAdd x y

sub :: Operand -> Operand -> MASMFuncM ()
sub x y = modFun $ MASMSub x y

imul :: Operand -> Operand -> MASMFuncM ()
imul x y = modFun $ MASMMul x y
           
idiv :: Operand -> Operand -> MASMFuncM ()
idiv x y = modFun $ MASMDiv x y
           
inc :: Operand -> MASMFuncM ()
inc x = modFun $ MASMInc x

dec :: Operand -> MASMFuncM ()
dec x = modFun $ MASMDec x

mov :: Operand -> Operand -> MASMFuncM ()
mov x y = modFun $ MASMMov x y
        
goto :: String -> MASMFuncM ()
goto x = modFun $ MASMGoto x

push :: Operand -> MASMFuncM ()
push x = modFun $ MASMPush x

pop :: Operand -> MASMFuncM ()
pop x = modFun $ MASMPop x
         
label :: String -> MASMFuncM ()
label x = modFun $ MASMLabel x

comment :: String -> MASMFuncM ()
comment x = modFun $ MASMComment x
          
stell :: (Monad m, Monoid (m a)) => a -> Writer (m a) ()
stell = tell . return


