module Main where

import Control.Monad.Except.Trans
import Control.Monad.Rec.Class
import Control.Monad.State
import Control.Monad.State.Trans
import Data.Array
import Data.Bifunctor
import Data.Either
import Data.Enum
import Data.Functor
import Data.Generic.Rep
import Data.Int
import Data.Int.Bits
import Data.Maybe
import Data.Show.Generic
import Data.Tuple
import Node.Encoding
import Node.FS.Sync
import Node.Process
import Prelude
import Unsafe.Coerce

import Control.Monad.Reader (ReaderT(..), ask, runReaderT)
import Control.Monad.Rec.Class (forever)
import Data.Function (applyN)
import Data.Map as Mp
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (codePointAt, codePointFromChar)
import Data.String as St
import Data.String.CodePoints as St
import Data.String.CodePoints as Stc
import Data.String.Common as St
import Data.String.Pattern as St
import Data.String.Utils as St
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Console (log)

{-
@ : プログラムカウンタ
& : ポインタ
* : ポインタ上の値
! : レジスタ
# : 即値（dstには入らない）

……*と数値だけにしない？0番はPCで


命令:
in     dst        : dstに入力
out    v1         : v1を出力
set    dst v1     : dstにv1を代入
add    dst v1 v2  : dstにv1+v2を代入
sub    dst v1 v2  : dstにv1-v2を代入
mul    dst v1 v2  : dstにv1*v2を代入
div    dst v1 v2  : dstにv1*v2を代入
mod    dst v1 v2  : dstにv1*v2を代入
gt     dst v1 v2  : dstにv1>v2なら1、さもなくば0を代入
eq     dst v1 v2  : dstにv1==v2なら1、さもなくば0を代入
-}


data Address  = Addr Value
derive instance genericAddress :: Generic Address _
instance showAddress :: Show Address where
  show (Addr v) = "@" <> show v 

data Value = Ptr Value | Val Int
derive instance genericValue :: Generic Value _
instance showValue :: Show Value where
  show (Ptr v) = "@" <> show v
  show (Val i) = show i

asPtr ∷ Address → Value
asPtr (Addr a) = Ptr a

asAddrVal ∷ Address → Value
asAddrVal (Addr a) = a

data Command =
  ComNop |
  ComIn  Address |
  ComOut Value|
  ComSet Address Value |
  ComAdd Address Value Value |
  ComSub Address Value Value |
  ComMul Address Value Value |
  ComDiv Address Value Value |
  ComMod Address Value Value |
  ComAnd Address Value Value |
  ComOr  Address Value Value |
  ComNot Address Value       |
  ComXor Address Value Value |
  ComShr Address Value Value |
  ComShl Address Value Value |
  ComGt  Address Value Value |
  ComLe  Address Value Value |
  ComEq  Address Value Value 
derive instance genericCommand :: Generic Command _
instance showCommand :: Show Command where
  show = genericShow

data SyntaxErrorType =
  WrongArity         |
  NeedValueOrAddress |
  NeedAddress        |
  NoSuchCommand
derive instance genericSyntaxErrorType :: Generic SyntaxErrorType _
instance showSyntaxErrorType :: Show SyntaxErrorType where
  show = genericShow

data RuntimeError =
  ZeroDiv      |
  InvalidAddress |
  NoInput      |
  PCOutOfRange 

derive instance genericRuntimeError :: Generic RuntimeError _
instance showRuntimeError :: Show RuntimeError where
  show = genericShow

data ExitReason =
  RuntimeError RuntimeError |
  StepLimit |
  Success

derive instance genericExitReason :: Generic ExitReason _
instance showExitReason :: Show ExitReason where
  show = genericShow


readSource :: String -> Either (Tuple Int SyntaxErrorType) (Array Command)
readSource str_ = map catMaybes $ sequence $ mapWithIndex (\i x -> lmap (\e -> Tuple (i+1) e) $ parseLine x) $ St.split (St.Pattern "\n") str_
  where
    readAtmarkedNum :: String -> Maybe (Tuple Int Int)
    readAtmarkedNum str =
      (\{ head: x, tail: xs } -> 
          if x == St.codePointFromChar '@'
          then lmap (_+1) <$> readAtmarkedNum xs 
          else (Tuple 0) <$> fromString str
      ) =<< (St.uncons $ St.trim str)
    
    readValue :: String -> Either SyntaxErrorType Value
    readValue str = maybe (Left NeedValueOrAddress) Right $ (\(Tuple n x) -> applyN Ptr n (Val x)) <$> readAtmarkedNum str

    readAddress :: String -> Either SyntaxErrorType Address
    readAddress str = 
      case readValue str of
        Right (Ptr v) -> Right $ Addr v
        _             -> Left NeedAddress
    
    --St.split " " >>> filter (_ \= "") >>>
    parseLine str =
      case (St.split (St.Pattern " ") >>> filter (_ /= "")) $ St.trim str of
        []                -> Right Nothing
        ["_"]             -> Right (Just ComNop)
        ["?", a1]         -> Just <$> (ComIn  <$> readAddress a1)
        ["!", v1]         -> Just <$> (ComOut <$> readValue v1)
        [":", a1, v1]     -> Just <$> (ComSet <$> readAddress a1 <*> readValue v1)
        ["+", a1, v1, v2] -> Just <$> (ComAdd <$> readAddress a1 <*> readValue v1 <*> readValue v2)
        ["-", a1, v1, v2] -> Just <$> (ComSub <$> readAddress a1 <*> readValue v1 <*> readValue v2)
        ["*", a1, v1, v2] -> Just <$> (ComMul <$> readAddress a1 <*> readValue v1 <*> readValue v2)
        ["/", a1, v1, v2] -> Just <$> (ComDiv <$> readAddress a1 <*> readValue v1 <*> readValue v2)
        ["%", a1, v1, v2] -> Just <$> (ComMod <$> readAddress a1 <*> readValue v1 <*> readValue v2)
        ["&", a1, v1, v2] -> Just <$> (ComAnd <$> readAddress a1 <*> readValue v1 <*> readValue v2)
        ["|", a1, v1, v2] -> Just <$> (ComOr  <$> readAddress a1 <*> readValue v1 <*> readValue v2)
        ["~", a1, v1]     -> Just <$> (ComNot <$> readAddress a1 <*> readValue v1)
        ["^", a1, v1, v2] -> Just <$> (ComXor <$> readAddress a1 <*> readValue v1 <*> readValue v2)
        ["}", a1, v1, v2] -> Just <$> (ComShr <$> readAddress a1 <*> readValue v1 <*> readValue v2)
        ["{", a1, v1, v2] -> Just <$> (ComShl <$> readAddress a1 <*> readValue v1 <*> readValue v2)
        [">", a1, v1, v2] -> Just <$> (ComGt  <$> readAddress a1 <*> readValue v1 <*> readValue v2)
        ["<", a1, v1, v2] -> Just <$> (ComLe  <$> readAddress a1 <*> readValue v1 <*> readValue v2)
        ["=", a1, v1, v2] -> Just <$> (ComEq  <$> readAddress a1 <*> readValue v1 <*> readValue v2)
        x | Just true == (St.startsWith "//" <$> index x 0)
                      -> Right Nothing
          | isJust ((\c -> findIndex (_ == c) ["_","?","!",":","+","-","*","/","%","&","|","~","^","}","{",">","<","="]) =<< index x 0)
                      -> Left WrongArity
          | otherwise -> Left NoSuchCommand

comToStr :: Command -> String
comToStr com =
  case com of
    ComNop       -> "_"
    ComIn  x     -> "?" <> " " <> show x
    ComOut x     -> "!" <> " " <> show x
    ComSet x y   -> ":" <> " " <> show x <> " " <> show y
    ComAdd x y z -> "+" <> " " <> show x <> " " <> show y <> " " <> show z
    ComSub x y z -> "-" <> " " <> show x <> " " <> show y <> " " <> show z
    ComMul x y z -> "*" <> " " <> show x <> " " <> show y <> " " <> show z
    ComDiv x y z -> "/" <> " " <> show x <> " " <> show y <> " " <> show z
    ComMod x y z -> "%" <> " " <> show x <> " " <> show y <> " " <> show z
    ComAnd x y z -> "&" <> " " <> show x <> " " <> show y <> " " <> show z
    ComOr  x y z -> "|" <> " " <> show x <> " " <> show y <> " " <> show z
    ComNot x y   -> "~" <> " " <> show x <> " " <> show y
    ComXor x y z -> "^" <> " " <> show x <> " " <> show y <> " " <> show z
    ComShr x y z -> "}" <> " " <> show x <> " " <> show y <> " " <> show z
    ComShl x y z -> "{" <> " " <> show x <> " " <> show y <> " " <> show z
    ComGt  x y z -> ">" <> " " <> show x <> " " <> show y <> " " <> show z
    ComLe  x y z -> "<" <> " " <> show x <> " " <> show y <> " " <> show z
    ComEq  x y z -> "=" <> " " <> show x <> " " <> show y <> " " <> show z

type Memory = Mp.Map Int Int
type Machine = {
    maxTime :: Int,
    time    :: Int,
    mem     :: Memory,
    code    :: Array Command
  }

readmem :: Memory -> Int -> Either ExitReason Int
readmem mem realaddr = 
  if realaddr < 0 || 1048576 < realaddr
  then Left (RuntimeError InvalidAddress)
  else Right $ fromMaybe 0 $ Mp.lookup realaddr mem

readPC :: Memory -> Int
readPC mem = fromMaybe 0 $ Mp.lookup 0 mem

evalValue :: Memory -> Value -> Either ExitReason Int
evalValue mem (Ptr p) = readmem mem =<< evalValue mem p
evalValue _   (Val v) = Right v

evalRewrite :: Memory -> Address -> Int -> Either ExitReason Memory
evalRewrite mem (Addr a) v =
  (\dest ->
      if dest < 0 || 65535 < dest
      then Left (RuntimeError InvalidAddress) 
      else Right $ Mp.insert dest v mem
  ) =<< evalValue mem a

type IOManager m = { getInt :: m (Maybe Int), putInt :: Int -> m Unit, debug :: String -> m Unit, onTick :: Machine -> m Unit }
type StdIOableT m = ReaderT (IOManager m) m
type PlugedT m a = Monad m => ExceptT ExitReason (StateT Machine (StdIOableT m)) a




liftPlug :: forall (m :: Type -> Type) (a :: Type). Monad m => m a -> PlugedT m a
liftPlug x = (lift >>> lift >>> lift) x

step :: forall (m :: Type -> Type). Monad m => PlugedT m Unit
step = do
  interface <- lift $ lift ask
  let getInt   = liftPlug $ interface.getInt
  let putInt x = liftPlug $ interface.putInt x  
  let debug  x = liftPlug $ interface.debug  x  
  let onTick x = liftPlug $ interface.onTick x  
  --let putInt = \x -> lift >>> lift >>> lift $ interface.putInt x  :: Int -> PlugedT m Unit
  --let debug  = lift $ lift $ lift interface.debug  :: PlugedT m Int
  old <- get
  onTick old
  let pc = readPC old.mem
  case index old.code pc of
    Nothing | pc == length old.code  -> throwError Success
            | otherwise              -> throwError (RuntimeError PCOutOfRange)

    Just com | old.maxTime < old.time -> throwError StepLimit
             | otherwise -> do
      let ticked = old {
              time = old.time + 1,
              mem  = Mp.insert 0 (pc+1) old.mem
            }
      -- when (time m `mod` 100000 == 0) ((ask))
      let read  v   = except $ evalValue ticked.mem v
      let write a v = except $ evalRewrite ticked.mem a v

      newmem <-
        case com of
          ComNop          -> pure ticked.mem
          ComIn  a1       -> getInt >>= maybe (throwError (RuntimeError NoInput)) pure >>= write a1
          ComOut v1       -> read v1 >>= putInt >>= \_ -> pure ticked.mem
          ComSet a1 v1    -> read v1 >>= write a1
          ComAdd a1 v1 v2 -> ((+) <$> read v1 <*> read v2) >>= write a1
          ComSub a1 v1 v2 -> ((-) <$> read v1 <*> read v2) >>= write a1
          ComMul a1 v1 v2 -> ((*) <$> read v1 <*> read v2) >>= write a1
          ComDiv a1 v1 v2 -> ((/) <$> read v1 <*> read v2) >>= write a1
          ComMod a1 v1 v2 -> (mod <$> read v1 <*> read v2) >>= write a1
          ComAnd a1 v1 v2 -> (and <$> read v1 <*> read v2) >>= write a1
          ComOr  a1 v1 v2 -> (or  <$> read v1 <*> read v2) >>= write a1
          ComXor a1 v1 v2 -> (xor <$> read v1 <*> read v2) >>= write a1
          ComNot a1 v1    -> (xor 0 <$> read v1) >>= write a1
          ComShr a1 v1 v2 -> (shr <$> read v1 <*> read v2) >>= write a1
          ComShl a1 v1 v2 -> (shl <$> read v1 <*> read v2) >>= write a1
          ComEq  a1 v1 v2 -> ((\a b -> if a == b then 1 else 0) <$> read v1 <*> read v2) >>= write a1
          ComLe  a1 v1 v2 -> ((\a b -> if a <  b then 1 else 0) <$> read v1 <*> read v2) >>= write a1
          ComGt  a1 v1 v2 -> ((\a b -> if a >  b then 1 else 0) <$> read v1 <*> read v2) >>= write a1
          --_               -> pure ticked.mem
      put $ ticked {
          mem = newmem
        }

stepWhileStop :: forall (m :: Type -> Type). MonadRec m => PlugedT m Unit
stepWhileStop = forever step

newMachine :: Int -> Array Command -> Machine
newMachine maxTime code = {
    maxTime : maxTime,
    time    : 0,
    mem     : Mp.empty,
    code    : code
  } 

debugIO :: Effect (Maybe Int) -> IOManager Effect
debugIO input = {
    getInt : input,
    putInt : \i -> log $ "\x1b[35mput: " <> show i <> "\x1b[0m",
    debug  : \s -> log $ "debug: " <> s,
    onTick : \m -> log $ "\ntick " <> show m.time <> "\n- PC: " <> (show (readPC m.mem)) <> "\n- command: " <> fromMaybe "[error]" (show <$> index m.code (readPC m.mem)) <> "\n- mem: " <> show m.mem 
  }

--run :: forall (m :: Type -> Type). MonadRec m => IOManager m -> Int -> Array Command -> m Machine
--run iomanager maxTime code = flip runReaderT iomanager $ flip execStateT (newMachine maxTime code) $ runExceptT $ stepWhileStop

runWithError :: forall (m :: Type -> Type). MonadRec m => IOManager m -> Int -> Array Command -> m (Tuple ExitReason Machine)
runWithError iomanager maxTime code = (lmap (fromLeft Success)) <$> (flip runReaderT iomanager $ flip runStateT (newMachine maxTime code) $ runExceptT $ stepWhileStop)

runCode :: forall (m :: Type -> Type). MonadRec m => IOManager m -> Int -> String -> m Unit
runCode iomanager maxTime str = 
  case readSource str of
    Right cmds            -> iomanager.debug =<< show <$> runWithError iomanager maxTime cmds
    Left (Tuple line err) -> iomanager.debug $ show err <> " on line " <> show line



type IOStateMini = {input :: Array Int, output :: Array Int}
genIOStateMini input = {input : input, output : []}
simpleNumIO :: IOManager (State IOStateMini)
simpleNumIO = {
    getInt : do
      ios <- get
      put (ios {input = fromMaybe [] (tail ios.input)})
      pure (head ios.input),
    putInt : \i -> (modify (\ios -> ios {output = snoc ios.output i}) >>= \_ -> pure unit),
    debug  : \s -> pure unit,
    onTick : \m -> pure unit
  }

compartibleNumIO :: IOManager (StateT IOStateMini Effect)
compartibleNumIO = {
    getInt : do
      ios <- get
      put (ios {input = fromMaybe [] (tail ios.input)})
      pure (head ios.input),
    putInt : \i -> (modify (\ios -> ios {output = snoc ios.output i}) >>= \_ -> pure unit),
    debug  : \s -> pure unit,
    onTick : \m -> lift $ log $ fromMaybe "[error]" (showComCompartible m.mem <$> index m.code (readPC m.mem))
  }

main ∷ Effect Unit
main = do
  let nummode = false
  let args = St.split (St.Pattern " ") "./data/dijkstra/cmds.sasm ./data/dijkstra/in1.txt ./data/dijkstra/out1.txt"
  case args of
    [fnamecode, fnamein, fnameout] -> do
      str    <- readTextFile UTF8 fnamecode
      numin  <- (if nummode then mapMaybe fromString <<< St.split (St.Pattern " ") else (St.toCodePointArray >>> map fromEnum >>> flip snoc 0)) <$> readTextFile UTF8 fnamein
      numout <- (if nummode then mapMaybe fromString <<< St.split (St.Pattern " ") else (St.toCodePointArray >>> map fromEnum >>> flip snoc 0)) <$> readTextFile UTF8 fnameout
      case readSource str of
        Left (Tuple line err) ->
          log $ "[CE] Error:" <> show err <> " on command " <> show line
        Right cmds            -> do
          (Tuple (Tuple exitreason machine) ioresult) <- flip runStateT (genIOStateMini numin) $ runWithError compartibleNumIO 10000000 cmds
          case exitreason of
            Success
              | (ioresult.output <> [0]) == numout -> log $ "[AC] "  <> show machine.time <> "steps"
              | otherwise                 -> log $ "[WA] "  <> show machine.time <> "steps"
            RuntimeError e                -> log $ "[RE] "  <> show machine.time <> "steps" <> " Error:" <> show e
            StepLimit                     -> log $ "[SLE] " <> show machine.time <> "+steps"
    _ -> log "arguments: [code] [in] [out]"



showCom :: Command -> String
showCom (ComNop        ) = "_"
showCom (ComIn  a      ) = "? " <> show a
showCom (ComOut   v1   ) = "! " <>                  show v1
showCom (ComSet a v1   ) = ": " <> show a <> " " <> show v1
showCom (ComAdd a v1 v2) = "+ " <> show a <> " " <> show v1 <> " " <> show v2
showCom (ComSub a v1 v2) = "- " <> show a <> " " <> show v1 <> " " <> show v2
showCom (ComMul a v1 v2) = "* " <> show a <> " " <> show v1 <> " " <> show v2
showCom (ComDiv a v1 v2) = "/ " <> show a <> " " <> show v1 <> " " <> show v2
showCom (ComMod a v1 v2) = "% " <> show a <> " " <> show v1 <> " " <> show v2
showCom (ComAnd a v1 v2) = "& " <> show a <> " " <> show v1 <> " " <> show v2
showCom (ComOr  a v1 v2) = "| " <> show a <> " " <> show v1 <> " " <> show v2
showCom (ComNot a v1   ) = "~ " <> show a <> " " <> show v1
showCom (ComXor a v1 v2) = "^ " <> show a <> " " <> show v1 <> " " <> show v2
showCom (ComShr a v1 v2) = "} " <> show a <> " " <> show v1 <> " " <> show v2
showCom (ComShl a v1 v2) = "{ " <> show a <> " " <> show v1 <> " " <> show v2
showCom (ComGt  a v1 v2) = "> " <> show a <> " " <> show v1 <> " " <> show v2
showCom (ComLe  a v1 v2) = "< " <> show a <> " " <> show v1 <> " " <> show v2
showCom (ComEq  a v1 v2) = "= " <> show a <> " " <> show v1 <> " " <> show v2

showComWithComment (Tuple com Nothing) = showCom com
showComWithComment (Tuple com (Just comment)) = showCom com <> " // " <> comment

evalValue' mem = fromRight 0 <<< evalValue mem
showComCompartible :: Memory -> Command -> String
showComCompartible mem (ComNop               ) = "_"
showComCompartible mem (ComIn  (Addr a)      ) = "? " <> show (evalValue' mem a)
showComCompartible mem (ComOut          v1   ) = "! " <>                                   show (evalValue' mem v1)
showComCompartible mem (ComSet (Addr a) v1   ) = ": " <> show (evalValue' mem a) <> " " <> show (evalValue' mem v1)
showComCompartible mem (ComAdd (Addr a) v1 v2) = "+ " <> show (evalValue' mem a) <> " " <> show (evalValue' mem v1) <> " " <> show (evalValue' mem v2)
showComCompartible mem (ComSub (Addr a) v1 v2) = "- " <> show (evalValue' mem a) <> " " <> show (evalValue' mem v1) <> " " <> show (evalValue' mem v2)
showComCompartible mem (ComMul (Addr a) v1 v2) = "* " <> show (evalValue' mem a) <> " " <> show (evalValue' mem v1) <> " " <> show (evalValue' mem v2)
showComCompartible mem (ComDiv (Addr a) v1 v2) = "/ " <> show (evalValue' mem a) <> " " <> show (evalValue' mem v1) <> " " <> show (evalValue' mem v2)
showComCompartible mem (ComMod (Addr a) v1 v2) = "% " <> show (evalValue' mem a) <> " " <> show (evalValue' mem v1) <> " " <> show (evalValue' mem v2)
showComCompartible mem (ComAnd (Addr a) v1 v2) = "& " <> show (evalValue' mem a) <> " " <> show (evalValue' mem v1) <> " " <> show (evalValue' mem v2)
showComCompartible mem (ComOr  (Addr a) v1 v2) = "| " <> show (evalValue' mem a) <> " " <> show (evalValue' mem v1) <> " " <> show (evalValue' mem v2)
showComCompartible mem (ComNot (Addr a) v1   ) = "~ " <> show (evalValue' mem a) <> " " <> show (evalValue' mem v1)
showComCompartible mem (ComXor (Addr a) v1 v2) = "^ " <> show (evalValue' mem a) <> " " <> show (evalValue' mem v1) <> " " <> show (evalValue' mem v2)
showComCompartible mem (ComShr (Addr a) v1 v2) = "} " <> show (evalValue' mem a) <> " " <> show (evalValue' mem v1) <> " " <> show (evalValue' mem v2)
showComCompartible mem (ComShl (Addr a) v1 v2) = "{ " <> show (evalValue' mem a) <> " " <> show (evalValue' mem v1) <> " " <> show (evalValue' mem v2)
showComCompartible mem (ComGt  (Addr a) v1 v2) = "> " <> show (evalValue' mem a) <> " " <> show (evalValue' mem v1) <> " " <> show (evalValue' mem v2)
showComCompartible mem (ComLe  (Addr a) v1 v2) = "< " <> show (evalValue' mem a) <> " " <> show (evalValue' mem v1) <> " " <> show (evalValue' mem v2)
showComCompartible mem (ComEq  (Addr a) v1 v2) = "= " <> show (evalValue' mem a) <> " " <> show (evalValue' mem v1) <> " " <> show (evalValue' mem v2)