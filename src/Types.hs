{-# LANGUAGE GADTs, ExistentialQuantification, FlexibleContexts, StandaloneDeriving #-}
module Types where
import qualified Data.Map as M
import Data.Word
import Control.Monad.State.Lazy
class Def a where
    def :: a
data MASM = MASM { masmProgMode :: MASMMode
                 , masmProgOptions :: [String]
                 , masmInclude :: [MASMInclude]
                 , masmProg :: MASMProgM ()
                 }
instance Def MASM where
    def = MASM { masmProgMode = Mode386
               , masmProgOptions = []
               , masmInclude = []
               , masmProg = return ()
               }

data Lit = IntLit Word8 | Lits [Lit]
type Addr = Word16
type Scale = Int
type Displacement = Int
data Operand where
    Imm :: Word16 -> Operand -- to be fixed
    Direct :: Addr -> Operand
    Reg :: forall a. Reg a => a -> Operand
    RegIndirect :: forall a. Reg a => a -> Operand
    RegIndex :: forall a. Reg a => a -> Displacement -> Operand
    RegIndexScale :: forall a. Reg a => a -> a -> Scale -> Displacement -> Operand
instance Show Operand where
    show (Imm x) = show x ++ "D"
    show (Direct addr) = show addr ++ "D" -- Decimal
    show (Reg reg) = show reg
    show (RegIndirect reg) = "[ " ++ show reg ++ " ]"
    show (RegIndex reg disp) = "[ " ++ show reg ++ " + " ++ show disp ++ " ]"
    show (RegIndexScale baseReg indexReg scale disp) = "[ " ++ show baseReg ++ " + " ++ show indexReg ++ "*" ++ show scale ++ " + " ++ show disp ++ " ]" 
data MASMMode = Mode386 | Mode486 | Mode586 | Mode686
data Reg32 = EAX | EBX | ECX | EDX | ESI | EDI | ESP | EBP deriving Show
data Reg16 = AX | BX | CX | DX | SI | DI | SP | BP deriving Show
data Reg8 = AH | AL | BH | BL | CH | CL | DH | DL | SPL | BPL | SIL | DIL deriving Show
data RegXMM = XMM0 | XMM1 | XMM2 | XMM3 | XMM4 | XMM5 | XMM6 | XMM7 | XMM8 | XMM9 | XMM10 | XMM11 | XMM12 | XMM13 | XMM14 | XMM15 deriving Show
data RegClass = Reg32 | Reg16 | Reg8 | RegXMM
class Show a => Reg a where
    showReg :: a -> String
    showReg = show
    regClass :: a -> RegClass
instance Reg Reg32 where
    regClass = const Reg32
instance Reg Reg16 where
    regClass = const Reg16
instance Reg Reg8 where
    regClass = const Reg8
instance Reg RegXMM where
    regClass = const RegXMM
data MASMInclude = MASMInclude String | MASMIncludeLib String
data MASMVarType = DB | DW | DD deriving Show
type MASMVar = (MASMVarType, [Word8])
type MASMVarMap = M.Map String MASMVar
data CallingConvention = Default | Cdecl | FastCall | StdCall
data MASMInstr = MASMAdd Operand Operand
               | MASMSub Operand Operand
               | MASMMul Operand Operand
               | MASMDiv Operand Operand
               | MASMMov Operand Operand
               | MASMInc Operand
               | MASMDec Operand
               | MASMPush Operand
               | MASMPop Operand
               | MASMFuncCall String CallingConvention [FuncArg]
               | MASMGoto String
               | MASMLabel String
data FuncArg = forall a. Show Operand => FuncArg a
data MASMFunc = MASMFunc { funcName :: String
                         , instrs :: [MASMInstr]
                         }
type MASMFuncM a = State MASMFunc a
type MASMProgM a = State MASMProg a

data MASMTopLevel = Func MASMFunc
data MASMProg = MASMProg { globalVarMap :: MASMVarMap
                         , funcs :: [MASMTopLevel]
                         }

data MASMOutput = MASMOutput String | MASMOutputNoIndent String | Indent | Dedent | NewLine
