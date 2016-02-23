module Main where

import Core
import Types
import Control.Monad.Writer.Lazy
    
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
          mkFunc "testFunc1" $ do
                 mov (Reg EAX) (Imm 0)
                 label "testLabel"
    in
      (mapM_ putStrLn) . output . snd . runWriter . produceAsm $ masm
