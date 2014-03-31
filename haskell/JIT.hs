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
import qualified LLVM.General.ExecutionEngine as EE
import LLVM.General.PassManager
import LLVM.General.Transforms
import LLVM.General.Analysis

foreign import ccall "dynamic" haskFun :: FunPtr (IO Double) -> (IO Double)

data CodegenMode
  = GenerateLLVM
  | REPL

runInMode :: AST.Module -> CodegenMode -> IO (Either String AST.Module)
runInMode mod mode = do
  withContext $ \context ->
    runErrorT $ withModuleFromAST context mod $ \m ->
      case mode of 
         REPL -> runJIT context m
         GenerateLLVM -> genreatellvm context m

runJIT :: Context -> Module -> IO AST.Module
runJIT context m =
  jit context $ \executionEngine ->
    withPassManager passes $ \pm -> do
    runPassManager pm m
    optmod <- moduleAST m
    EE.withModuleInEngine executionEngine m $ \ee -> do
      mainfn <- EE.getFunction ee (AST.Name "main")
      case mainfn of
        Just fn -> do
          result <- runCodeAtFunctionPointer fn
          putStrLn $ show result
        Nothing -> return ()
      return optmod

genreatellvm :: Context -> Module -> IO AST.Module
genreatellvm context m = do
  optmod <- moduleAST m
  s <- moduleLLVMAssembly m
  putStrLn s
  return optmod

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optLevel model ptrelim fastins
  where
    optLevel = Just 3
    model = Nothing
    ptrelim = Nothing
    fastins = Nothing

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }

runCodeAtFunctionPointer :: FunPtr a -> IO Double
runCodeAtFunctionPointer fn = haskFun (castFunPtr fn :: FunPtr (IO Double))


