{-# CFILES c/cbits.c #-}

module JIT where

import Data.Int
import Data.Bool
import Data.Word
import Foreign.Ptr ( FunPtr, castFunPtr )

import Control.Monad.Error

import LLVM.General.Target
import LLVM.General.Context
import LLVM.General.CodeModel
import LLVM.General.Module as Mod
import qualified LLVM.General.AST as AST

import LLVM.General.PassManager
import LLVM.General.Transforms
import LLVM.General.Analysis

import qualified LLVM.General.ExecutionEngine as EE

foreign import ccall "dynamic" haskFun :: FunPtr (IO Double) -> (IO Double)

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }

runJIT :: AST.Module -> Bool -> IO (Either String AST.Module)
runJIT mod evaluate = do
  withContext $ \context ->
    jit context $ \executionEngine ->
      runErrorT $ withModuleFromAST context mod $ \m ->
        withPassManager passes $ \pm -> do
          runPassManager pm m
          optmod <- moduleAST m
          s <- moduleLLVMAssembly m
          putStrLn s
-- uncomment for REPL
          EE.withModuleInEngine executionEngine m $ \ee -> do
            mainfn <- EE.getFunction ee (AST.Name "main")
            case (mainfn, evaluate) of
              (Just fn, True) -> do
                res <- run fn
                putStrLn $ "Evaluated to: " ++ show res
              _ -> return ()
--
          return optmod

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optLevel model ptrelim fastins
  where
    optLevel = Just 2
    model = Nothing
    ptrelim = Nothing
    fastins = Nothing

run :: FunPtr a -> IO Double
run fn = haskFun (castFunPtr fn :: FunPtr (IO Double))


