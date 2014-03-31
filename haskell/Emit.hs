module Emit where
  
import LLVM.General.Module
import LLVM.General.Context

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.FloatingPointPredicate as FP
import qualified LLVM.General.AST.Instruction as I

import Data.Word
import Data.Int
import Data.Bool
import Control.Monad.Error
import Control.Applicative
import qualified Data.Map as Map

import Codegen
import JIT
import qualified Syntax as S

one = cons $ C.Float (F.Double 1.0)
zero = cons $ C.Float (F.Double 0.0)
false = zero
true = one

toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name x))

codegenTop :: S.Expr -> LLVM ()
codegenTop (S.Function name args body) = do
  define double name fnargs bls
  where
    fnargs = toSig args
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM args $ \a -> do
        var <- alloca double
        store var (local (AST.Name a))
        assign a var
      cgen body >>= ret

codegenTop (S.Extern name args) = do
  external double name fnargs
  where fnargs = toSig args

codegenTop exp = do
  define double "main" [] blks
  where
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      cgen exp >>= ret

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------
lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt a b = do
  test <- fcmp FP.ULT a b
  uitofp double test

binops = Map.fromList [
    ("+", fadd),
    ("-", fsub),
    ("*", fmul),
    ("/", fdiv),
    ("<", lt)
  ]

cgen :: S.Expr -> Codegen AST.Operand
cgen (S.Float n) = return $ cons $ C.Float (F.Double n)
cgen (S.Var x) = getvar x >>= load
cgen (S.Call fn args) = do
  largs <- mapM cgen args
  call (externf (AST.Name fn)) largs
cgen (S.BinaryOp op a b) = do
  case Map.lookup op binops of
    Just f -> do
      ca <- cgen a
      cb <- cgen b
      f ca cb
    Nothing -> error "No such operator"
cgen (S.If cond tr fl) = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"

  -- %entry
  cond <- cgen cond
  test <- fcmp FP.ONE false cond -- FP.ONE: true if ordered and operands are unequal
  cbr test ifthen ifelse

  -- if.then
  setBlock ifthen
  trval <- cgen tr
  br ifexit
  ifthen <- getBlock

  -- if.else
  setBlock ifelse
  flval <- cgen fl
  br ifexit
  ifelse <- getBlock

  -- if.exit
  setBlock ifexit
  phi double [(trval, ifthen), (flval, ifelse)]

cgen (S.For ivar start cond step body) = do
  forloop <- addBlock "for.loop"
  forexit <- addBlock "for.exit"
  -- entry
  i <- alloca double
  istart <- cgen start
  stepval <- cgen step
  store i istart
  assign ivar i
  br forloop
  -- for loop
  setBlock forloop
  cgen body
  ival <- load i
  inext <- fadd ival stepval
  store i inext
  cond <- cgen cond
  test <- fcmp FP.ONE false cond
  cbr test forloop forexit
  setBlock forexit
  return zero

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------
liftError :: ErrorT String IO a -> IO a
liftError = runErrorT >=> either fail return

codegen :: AST.Module -> CodegenMode -> [S.Expr] -> IO AST.Module
codegen mod mode fns = do
  res <- runInMode oldast mode
  case res  of
    Right newast -> return newast
    Left err     -> putStrLn ("error: " ++ err) >> return oldast
  where
    modn    = mapM codegenTop fns
    oldast  = runLLVM mod modn
