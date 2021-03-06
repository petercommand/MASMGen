{-|
Module      : Language.MASMGen.Types
Description : Provides the data types used by the library
Copyright   : (c) Ruey-Lin Hsu (petercommand)
License     : LGPL-3
Maintainer  : petercommand@gmail.com
Stability   : provisional
Portability : portable
-}
{-# LANGUAGE GADTs, ExistentialQuantification, FlexibleContexts, StandaloneDeriving #-}
module Language.MASMGen.Types ( MASM(..)
                              , Lit(..)
                              , Var
                              , Addr
                              , Scale
                              , Displacement
                              , mkVar
                              , Operand(..)
                              , OpClass(..)
                              , MASMMode(..)
                              , Reg32(..)
                              , Reg16(..)
                              , Reg8(..)
                              , RegXMM(..)
                              , RegClass(..)
                              , MASMInclude(..)
                              , MASMType(..)
                              , MASMVar
                              , MASMVarMap
                              , CallingConvention(..)
                              , MASMInstr(..)
                              , FuncArg
                              , MASMFunc(..)
                              , MASMFuncM
                              , MASMProgM
                              , MASMTopLevel(..)
                              , MASMProg(..)
                              , MASMOutput(..)
                              , UntypedMASMInstrSinCon
                              , UntypedMASMInstrBinCon
                              , TypedMASMInstrSinCon
                              , TypedMASMInstrBinCon
                              , operandClass
                              , def
                              , showReg
                              , regClass
                              )

                               where
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

data Lit = Lit8 Word8 | Lit16 Word16 | Lit32 Word32
instance Show Lit where
    show (Lit8 x) = show x
    show (Lit16 x) = show x
    show (Lit32 x) = show x
                     
                     
type Addr = Word16
type Scale = Int
type Displacement = Int

data Var = Var { varName :: String
               , varType :: MASMType
               }

instance Show Var where
    show = varName

mkVar :: String -> MASMType -> Var
mkVar name var = Var { varName = name
                     , varType = var
                     }
    
data Operand where
    Imm :: Word32 -> Operand -- to be fixed
    Direct :: Addr -> Operand
    Reg :: forall a. Reg a => a -> Operand
    RegIndirect :: forall a. Reg a => a -> Operand
    RegIndex :: forall a. Reg a => a -> Displacement -> Operand
    RegIndexScale :: forall a. Reg a => a -> a -> Scale -> Displacement -> Operand
    VarAddr :: Var -> Operand

data OpClass = Pointer | Register RegClass | Immediate
               
class OperandClass a where
    operandClass :: a -> OpClass

instance OperandClass Operand where
    operandClass (Imm _) = Immediate
    operandClass (Direct _) = Pointer
    operandClass (Reg x) = Register (regClass x)
    operandClass (RegIndirect _) = Pointer
    operandClass (RegIndex _ _) = Pointer
    operandClass (RegIndexScale _ _ _ _) = Pointer
    operandClass (VarAddr _) = Pointer

instance Show Operand where
    show (Imm x) = show x ++ "D"
    show (Direct addr) = show addr ++ "D" -- Decimal
    show (Reg reg) = show reg
    show (RegIndirect reg) = "[" ++ show reg ++ "]"
    show (RegIndex reg disp) = "[" ++ show reg ++ " + " ++ show disp ++ "]"
    show (RegIndexScale baseReg indexReg scale disp) = "[" ++ show baseReg
                                                       ++ " + " ++ show indexReg
                                                       ++ "*" ++ show scale
                                                       ++ " + " ++ show disp
                                                       ++ "]"
    show (VarAddr x) = "[" ++ show x ++ "]"
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
data MASMType where
    DB :: MASMType
    DW :: MASMType
    DD :: MASMType
    Ptr :: MASMType -> MASMType
instance Show MASMType where
    show DB = "BYTE"
    show DW = "WORD"
    show DD = "DWORD"
    show (Ptr x) = show x ++ " PTR"

type MASMVar = (MASMType, Maybe [Lit])
type MASMVarMap = M.Map String MASMVar
data CallingConvention = Default | Cdecl | FastCall | StdCall
data MASMInstr = MASMAdd (Maybe MASMType) Operand Operand
               | MASMSub (Maybe MASMType) Operand Operand
               | MASMMul (Maybe MASMType) Operand Operand
               | MASMDiv (Maybe MASMType) Operand Operand
               | MASMMov (Maybe MASMType) Operand Operand
               | MASMMovsx Operand Operand
               | MASMMovzx Operand Operand
               | MASMInc (Maybe MASMType) Operand
               | MASMDec (Maybe MASMType) Operand
               | MASMPush (Maybe MASMType) Operand
               | MASMPop (Maybe MASMType) Operand
               | MASMShl Operand Operand
               | MASMSal Operand Operand
               | MASMShr Operand Operand
               | MASMSar Operand Operand
               | MASMLea Operand Operand
               | MASMFuncCall String CallingConvention [FuncArg]
               | MASMGoto String
               | MASMLabel String
               | MASMComment String
type FuncArg = Operand
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

type UntypedMASMInstrSinCon = (Operand -> MASMInstr)
type UntypedMASMInstrBinCon = (Operand -> Operand -> MASMInstr)
type TypedMASMInstrSinCon = (Maybe MASMType) -> Operand -> MASMInstr
type TypedMASMInstrBinCon = (Maybe MASMType) -> Operand -> Operand -> MASMInstr
    
