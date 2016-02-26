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
                 movl (RegIndirect EAX) (Reg EBX)
                 addw (Reg AX) (Imm 10)
                 sub (Reg AX) (Imm 100)
          mkFunc "testFunc1" $ do
                 let loop n = if n > 0
                              then do
                                mov (Reg EAX) (Imm $ n * n)
                                loop (n - 1)
                              else
                                  return ()
                 loop 20
                 comment "this is a comment"
                 label "testLabel"
    in
      (mapM_ putStrLn) . output . snd . runWriter . produceAsm $ masm
