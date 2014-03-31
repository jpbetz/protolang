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

data CodegenMode
  = GenerateLLVM
  | REPL

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }

runInMode :: AST.Module -> CodegenMode -> IO (Either String AST.Module)
runInMode mod mode = do
  withContext $ \context ->
    runErrorT $ withModuleFromAST context mod $ \m ->
      jit context $ \executionEngine ->
        withPassManager passes $ \pm -> do
          runPassManager pm m
          optmod <- moduleAST m
          case mode of
            REPL ->
					  	EE.withModuleInEngine executionEngine m $ \ee -> do
						    mainfn <- EE.getFunction ee (AST.Name "main")
						    case mainfn of
						      Just fn -> do
						        res <- run fn
						        putStrLn $ show res
						      Nothing -> return ()
            GenerateLLVM -> do
              s <- moduleLLVMAssembly m;
	            putStrLn s
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


