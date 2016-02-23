module Main where

import Control.Monad
import Control.Applicative
import Core
import Types
import Control.Monad.Writer.Lazy
import qualified Data.Map as M
    
main :: IO ()
main =
    let
        masm = def { masmProgMode = Mode386
                   , masmProgOptions = ["casemap :none"]
                   , masmInclude = [ MASMInclude "\\masm32\\include\\windows.inc"
                                   , MASMInclude "\\masm32\\include\\kernel32.inc"
                                   , MASMInclude "\\masm32\\include\\masm32.inc"
                                   , MASMIncludeLib "\\masm32\\lib\\kernel32.lib"
                                   , MASMIncludeLib "\\masm32\\lib\\masm32.lib"
                                   ]
                   , masmProg = prog
                   }
        prog = do
          mkFunc "start" $ do
                 mov (Reg AX) (Imm 0)
                 add (Reg AX) (Imm 10)
                 sub (Reg AX) (Imm 100)
    in
      (mapM_ putStrLn) . output . snd . runWriter . produceAsm $ masm
         

         
