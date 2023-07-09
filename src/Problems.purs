module Problems where

import Control.Monad.State
import Data.Array
import Data.Either
import Data.Enum
import Data.Maybe
import Data.String
import Data.Traversable
import Data.Tuple
import Effect.Class.Console
import Effect.Random
import Macro
import Main
import Node.Encoding
import Node.FS.Sync
import Prelude
import Effect

import Data.Int as I
import Data.Map as Mp
import Data.Traversable (sequence)

debugSimpleNumIO :: IOManager (State IOState)
debugSimpleNumIO = {
    getInt : do
      ios <- get
      put (ios {input = fromMaybe [] (tail ios.input)})
      pure (head ios.input),
    putInt : \i -> (modify (\ios -> ios {output = snoc ios.output i}) >>= \_ -> pure unit),
    debug  : \s -> pure unit,
    onTick : \m -> modify (\ios -> ios {linelog = Mp.alter (Just <<< maybe 1 (_+1)) (readPC m.mem) ios.linelog}) >>= \_ -> pure unit
  }

checkProgramWithFile ∷ Boolean -> String → String → Array Command → Effect Unit
checkProgramWithFile debug fnamein fnameout program = do
  numin  <- (toCodePointArray >>> map fromEnum >>> flip snoc 0) <$> readTextFile UTF8 fnamein
  numout <- (toCodePointArray >>> map fromEnum >>> flip snoc 0) <$> readTextFile UTF8 fnameout
  (Tuple (Tuple exitreason machine) ioresult) <- 
    if debug
    then        flip runStateT ioStateDefault {input = numin} $ runWithError debugIO2 20000 program
    else pure $ flip runState ioStateDefault {input = numin} $ runWithError debugSimpleNumIO 1000000 program
  logShow $ exitreason
  logShow $ machine.time
  logShow $ ioresult.output
  log $ joinWith "" $ map (\i -> fromMaybe "�" (fromCodePointArray <$> (sequence [toEnum i]))) $ ioresult.output
  timeWrite  (joinWith "\n" $ map (\(Tuple l c) -> show l <> "\t" <> show c) $ Mp.toUnfoldable ioresult.linelog)


checkProgramWithFileL2 debug fnamein fnameout program = do
  writeTextFile UTF8 "./debug.log" ""
  writeTextFile UTF8 "./time.log" ""
  case compileL2 program of
    Left err   -> logShow err
    Right cmds -> do
      writeTextFile UTF8 "./cmds.sasm" (joinWith "\n" (map showComWithComment cmds))
      checkProgramWithFile debug fnamein fnameout (map fst cmds)

compileToFile program dir = do
  case compileL2 program of
    Left err   -> logShow err
    Right cmds -> do
      writeTextFile UTF8 (dir <>"/"<>"cmds.sasm") (joinWith "\n" (map showComWithComment cmds))


newvar :: ∀ a. MonadL2Local (L2LocalVariable a)
newvar = l2NewLocalVar

newsvar = l2NewStaticVar
newgvar = l2NewGlobalVar

genDataKnapsack ∷ Int → Effect String
genDataKnapsack count = do
  weightmax <- randomInt (count) (count*2)
  wvdata <- for (1..count) (\_ -> joinWith " " <$> map show <$> sequence [randomInt 1 10, randomInt 1 10])
  pure $ show count <> " " <> show weightmax <> "\n" <> joinWith "\n" wvdata

genDataKnapsack_fs ∷ Effect Unit
genDataKnapsack_fs = do
  writeTextFile UTF8 "./data/14_knapsack/in1.txt" =<< genDataKnapsack 5
  writeTextFile UTF8 "./data/14_knapsack/in2.txt" =<< genDataKnapsack 10
  writeTextFile UTF8 "./data/14_knapsack/in3.txt" =<< genDataKnapsack 20
  writeTextFile UTF8 "./data/14_knapsack/in4.txt" =<< genDataKnapsack 30
  writeTextFile UTF8 "./data/14_knapsack/in5.txt" =<< genDataKnapsack 50
  writeTextFile UTF8 "./data/14_knapsack/in6.txt" =<< genDataKnapsack 70
{-
input :
cnt
weight value
...

-}
prob_knapsak :: MonadL2Global Unit
prob_knapsak = do
  stdio <- importAs "stdio" l2LibStdIO
  libpq <- importAs "libpq" l2LibMinPriorityQueue
  
  main :: L2F (MonadL2Local Unit) <- l2VirtualFuncMain
  l2SetRealFunc main (do
    cnt       <- newsvar
    weightmax <- newsvar
    wmp1      <- newsvar
    cnt       <<- (f stdio.getInt :: ML2E Int)
    weightmax <<- (f stdio.getInt :: ML2E Int)
    wmp1      <<- weightmax +^ 1

    weights <- newsvar
    values  <- newsvar
    weights <<- (l2PrimGetHeap cnt :: ML2E (L2Arr Int))
    values  <<- (l2PrimGetHeap cnt :: ML2E (L2Arr Int))

    dp  <- newsvar
    dp <<- (l2PrimGetHeap (cnt +^ 1) :: ML2E (L2Arr (L2Arr Int)))

    i <- newsvar
    i <<- 0
    l2While (i <^ cnt) (do
      values  ##^ i <<- (f stdio.getInt :: ML2E Int)
      weights ##^ i <<- (f stdio.getInt :: ML2E Int)
      dp      ##^ i <<- (l2PrimGetHeap wmp1 :: ML2E (L2Arr Int))
      i <<- i +^ 1
    )
    dp ##^ cnt <<- (l2PrimGetHeap wmp1 :: ML2E (L2Arr Int))

    w    <- newsvar
    wnow <- newsvar
    vnow <- newsvar
    dpi  <- newsvar
    dpi1 <- newsvar
    v1   <- newsvar
    v2   <- newsvar
    i <<- 0
    l2While (i <^ cnt) (do
      w <<- 0
      vnow <<- values #^ i
      dpi  <<- dp #^ i
      dpi1 <<- dp #^ (i+^1)
      l2While (w <^ wmp1) (do
        wnow <<- weights #^ i
        l2IfElse (w <^ wnow) (do
          dpi1 ##^ w <<- dpi #^ w
        )(do
          v1 <- dpi #^ w
          v2 <- (dpi #^ (w -^ wnow)) +^ vnow
          l2IfElse (v1 <^ v2) (do
            dpi1 ##^ w <<- v2
          ) (do
            dpi1 ##^ w <<- v1
          )
        )
        w <<- w +^ 1
      )
      i <<- i +^ 1
    )

    u $ f stdio.printInt (dp #^ cnt #^ weightmax)

    pure unit
  )

  pure unit



genDataDijkstra towns = do
  roads <- randomInt (towns * 3) (towns * 4)
  start <- randomInt 0 (towns-1)
  goal  <- randomInt 0 (towns-1)
  roaddata <- for (1..roads) (\_ -> joinWith " " <$> map show <$> sequence [randomInt 0 (towns-1), randomInt 0 (towns-1), randomInt 1 100])
  pure $ show towns <> " " <> show roads <> " " <> show start <> " " <> show goal <> "\n" <> joinWith "\n" roaddata

genDataDijkstra_fs = do
  writeTextFile UTF8 "./data/16_dijkstra/in1.txt" =<< genDataDijkstra 5
  writeTextFile UTF8 "./data/16_dijkstra/in2.txt" =<< genDataDijkstra 10
  writeTextFile UTF8 "./data/16_dijkstra/in3.txt" =<< genDataDijkstra 20
  writeTextFile UTF8 "./data/16_dijkstra/in4.txt" =<< genDataDijkstra 50
  writeTextFile UTF8 "./data/16_dijkstra/in5.txt" =<< genDataDijkstra 80
  writeTextFile UTF8 "./data/16_dijkstra/in6.txt" =<< genDataDijkstra 100

{-
input :
cnt_towns cnt_roads start goal
from to cost
...

-}
prob_dijkstra :: MonadL2Global Unit
prob_dijkstra = do
  stdio <- importAs "stdio" l2LibStdIO
  libpq <- importAs "libpq" l2LibMinPriorityQueue
  
  main :: L2F (MonadL2Local Unit) <- l2VirtualFuncMain
  l2SetRealFunc main (do
      cnt_towns <- newsvar
      cnt_roads <- newsvar
      start     <- newsvar
      goal      <- newsvar
      costmax   <- newsvar
      
      cnt_towns <<- (f stdio.getInt :: ML2E Int)
      cnt_roads <<- (f stdio.getInt :: ML2E Int)
      start     <<- (f stdio.getInt :: ML2E Int)
      goal      <<- (f stdio.getInt :: ML2E Int)

      arr_roads_cost  <- newsvar
      arr_roads_exist <- newsvar

      arr_roads_cost  <<- (l2PrimGetHeap cnt_towns :: ML2E (L2Arr (L2Arr Int)))
      arr_roads_exist <<- (l2PrimGetHeap cnt_towns :: ML2E (L2Arr (L2Arr Boolean)))

      i <- newsvar
      i <<- 0
      l2While (i <^ cnt_towns) (do
        arr_roads_cost  ##^ i <<- l2PrimGetHeap cnt_towns
        arr_roads_exist ##^ i <<- l2PrimGetHeap cnt_towns
        i <<- i +^ 1
      )

      tmp_from <- newsvar
      tmp_to   <- newsvar
      tmp_cost <- newsvar
      i <<- 0
      l2While (i <^ cnt_roads) (do
        tmp_from <<- (f stdio.getInt :: ML2E Int)
        tmp_to   <<- (f stdio.getInt :: ML2E Int)
        tmp_cost <<- (f stdio.getInt :: ML2E Int)
        l2IfElse (costmax <^ tmp_cost) (do
            costmax <<- tmp_cost
          )(nop)
        l2IfElse (arr_roads_exist #^ tmp_from #^ tmp_to) (do
            l2IfElse (tmp_cost <^ (arr_roads_cost #^ tmp_from #^ tmp_to)) (do
              (arr_roads_cost  #^ tmp_from ##^ tmp_to) <<- tmp_cost
            ) (nop)
          ) (do
              (arr_roads_cost  #^ tmp_from ##^ tmp_to) <<- tmp_cost
              (arr_roads_exist #^ tmp_from ##^ tmp_to) <<- true
          )
        i <<- i +^ 1
      )


      costmax <<- costmax *^ cnt_towns *^ 2

      arr_prev <- newsvar
      arr_seen <- newsvar

      arr_prev <<- (l2PrimGetHeap cnt_towns :: ML2E (L2Arr Int))
      arr_seen <<- (l2PrimGetHeap cnt_towns :: ML2E (L2Arr Boolean))

      --arr_seen ##^ start <<- true

      queue <- newsvar
      queue <<- (f libpq.new cnt_roads :: ML2E (L2MinPriorityQueue Int))
      u $ f libpq.push queue start 0

      tmp_town   <- newsvar
      tmp_cost   <- newsvar
      re_to      <- newsvar
      rc_to      <- newsvar
      new_cost   <- newsvar
      tmp_town2  <- newsvar

      l2While (true) (do
        l2IfElse (f libpq.empty queue :: ML2E Boolean) (do
          stdio.macroPutString "no"
          l2Return 0
        )(do
          tmp_cost <<- (f libpq.getPriority queue :: ML2E Int)
          tmp_town <<- (f libpq.getValue queue :: ML2E Int)
          u $ f libpq.pop queue

          l2IfElse (arr_seen #^ tmp_town) (nop) (do
            arr_seen ##^ tmp_town <<- true
            l2IfElse (tmp_town ==^ goal) (do
              u $ f stdio.printInt tmp_cost
              l2Return 0
            )(do
              re_to <<- arr_roads_exist #^ tmp_town
              rc_to <<- arr_roads_cost  #^ tmp_town
              i <<- 0
              l2While (i <^ cnt_towns) (do
                l2IfElse ((l2ComNot $ arr_seen #^ i) &^ (re_to #^ i)) (
                  f libpq.push queue i (tmp_cost +^ (rc_to #^ i))
                ) (nop)
                i <<- i +^ 1
              )
            )
          )
        ) 
        --asL2 $ asL1 $ ComOut (Val 88888)
      )
      --asL2 $ asL1 $ ComOut (Val 99999)

      pure unit
    )

  pure unit


prob_numinput :: MonadL2Global Unit
prob_numinput = do
  stdio <- importAs "stdio" l2LibStdIO
  libpq <- importAs "libpq" l2LibMinPriorityQueue
  
  main :: L2F (MonadL2Local Unit) <- l2VirtualFuncMain
  l2SetRealFunc main (do
      i <- newsvar
      i <<- (f stdio.getInt :: ML2E Int)
      u $ f stdio.putChar (cast i)
      pure unit
    )

  pure unit

prob_numoutput :: MonadL2Global Unit
prob_numoutput = do
  stdio <- importAs "stdio" l2LibStdIO
  libpq <- importAs "libpq" l2LibMinPriorityQueue
  
  main :: L2F (MonadL2Local Unit) <- l2VirtualFuncMain
  l2SetRealFunc main (do
      i <- newsvar
      i <<- (l2ComIn :: ML2E Int)
      u $ f stdio.printInt i
      pure unit
    )

  pure unit



prob_mergesort :: MonadL2Global Unit
prob_mergesort = do
  stdio <- importAs "stdio" l2LibStdIO
  libpq <- importAs "libpq" l2LibMinPriorityQueue
  
  main :: L2F (MonadL2Local Unit) <- l2VirtualFuncMain
  l2SetRealFunc main (do
      i <- newsvar
      i <<- (l2ComIn :: ML2E Int)
      u $ f stdio.printInt i
      pure unit
    )

  pure unit




genDataSort n = 
  (joinWith "\n" <<< map show) <$> (sequence $ replicate n $ (randomInt 0 (n*10)) :: Effect (Array Int))

genDataSort_fs = do
  writeTextFile UTF8 "./data/11_sort/in1.txt" =<< genDataSort 5
  writeTextFile UTF8 "./data/11_sort/in2.txt" =<< genDataSort 20
  writeTextFile UTF8 "./data/11_sort/in3.txt" =<< genDataSort 50
  writeTextFile UTF8 "./data/11_sort/in4.txt" =<< genDataSort 100
  writeTextFile UTF8 "./data/11_sort/in5.txt" =<< genDataSort 200
  writeTextFile UTF8 "./data/11_sort/in6.txt" =<< genDataSort 500

prob_sort :: MonadL2Global Unit
prob_sort = do
  stdio <- importAs "stdio" l2LibStdIO
  libpq <- importAs "libpq" l2LibMinPriorityQueue
  
  length <- newgvar
  arr_origin <- newgvar
  arr_temp1  <- newgvar
  arr_temp2  <- newgvar
  temp :: L2GlobalVariable Int <- newgvar

  merge :: L2F (L2E Int -> L2E Int -> L2E Int -> MonadL2Local Unit) <- l2VirtualFunc "merge"
  l2SetRealFunc merge (\left mid right -> do
      let inf = 2147483647
      i <- newsvar
      j <- newsvar
      k <- newsvar
      n1 <- newsvar
      n2 <- newsvar
      n1 <<- mid -^ left
      n2 <<- right -^ mid

      i <<- 0
      l2While (i <^ n1) (do
          arr_temp1 ##^ i <<- arr_origin #^ (left +^ i)
          i <<- i +^ 1
      )
      arr_temp1 ##^ i <<- unsafecast 2147483647
      i <<- 0
      l2While (i <^ n2) (do
          arr_temp2 ##^ i <<- arr_origin #^ (mid +^ i)
          i <<- i +^ 1
      )
      arr_temp2 ##^ i <<- unsafecast 2147483647

      j <<- 0
      k <<- 0
      i <<- left
      l2While (i <^ right) (do
          l2IfElse ((arr_temp1 #^ j) >^ (arr_temp2 #^ k)) (do
              arr_origin ##^ i <<- arr_temp2 #^ k
              k <<- k +^ 1
            )(do
              arr_origin ##^ i <<- arr_temp1 #^ j
              j <<- j +^ 1
            )
          i <<- i +^ 1
      )
  )

  sort :: L2F (L2E Int -> L2E Int -> MonadL2Local Unit) <- l2VirtualFunc "sort"
  l2SetRealFunc sort (\left right -> do
      l2IfElse (left +^ 1 <^ right) (do
          mid <- newvar
          mid <<- (left +^ right) /^ 2
          u $ f sort left mid
          u $ f sort mid  right
          u $ f merge left mid right
      )(pure unit)
  )

  main :: L2F (MonadL2Local Unit) <- l2VirtualFuncMain
  l2SetRealFunc main (do
      arr_origin  <<- (l2PrimGetHeap 0 :: ML2E (L2Arr Int))

      arr_origin ##^ length <<- (f stdio.getInt :: ML2E Int)
      length <<- length +^ 1
      l2While (0 <^ (L2E $ L1EVal $ ptr realAddrRegLastChar)) (do
        arr_origin ##^ length <<- (f stdio.getInt :: ML2E Int)
        length <<- length +^ 1
      )
      arr_origin <<- (l2PrimGetHeap length :: ML2E (L2Arr Int))
      arr_temp1  <<- (l2PrimGetHeap length :: ML2E (L2Arr Int))
      arr_temp2  <<- (l2PrimGetHeap length :: ML2E (L2Arr Int))
      u $ f sort 0 length

      i <- newsvar
      i <<- 1
      u $ f stdio.printInt (arr_origin #^ 0)
      l2While (i <^ length) (do
          asL2 $ l1Fold [asL1 $ ComOut (Val 32)]
          u $ f stdio.printInt (arr_origin #^ i)
          i <<- i +^ 1
      )
      pure unit
  )

  pure unit



compressChars = ["A","B","C","D","E","F","G"]
genDataCompress n = 
  (joinWith "" <<< map (\(Tuple x c) -> joinWith "" (replicate c $ fromMaybe "" $ index compressChars x))) <$> (sequence $ replicate n $ (Tuple <$> randomInt 0 6 <*> randomInt 1 20) :: Effect (Array (Tuple Int Int)))

genDataCompress_fs = do
  writeTextFile UTF8 "./data/12_compress/in1.txt" =<< genDataCompress 5
  writeTextFile UTF8 "./data/12_compress/in2.txt" =<< genDataCompress 20
  writeTextFile UTF8 "./data/12_compress/in3.txt" =<< genDataCompress 50
  writeTextFile UTF8 "./data/12_compress/in4.txt" =<< genDataCompress 100
  writeTextFile UTF8 "./data/12_compress/in5.txt" =<< genDataCompress 200
  writeTextFile UTF8 "./data/12_compress/in6.txt" =<< genDataCompress 500

prob_compress :: MonadL2Global Unit
prob_compress = do
  stdio <- importAs "stdio" l2LibStdIO
  libpq <- importAs "libpq" l2LibMinPriorityQueue
  
  main :: L2F (MonadL2Local Unit) <- l2VirtualFuncMain
  l2SetRealFunc main (do
      char  <- newsvar
      count <- newsvar
      now   <- newsvar
      count <<- 0
      now   <<- (l2ComIn :: ML2E Char)
      char  <<- now
      l2While (intcast now >^ 0) (do
        l2IfElse (char ==^ now) (do
          count <<- count +^ 1
        ) (do
          u $ f stdio.putChar char
          u $ f stdio.printInt count
          count <<- 1
          char  <<- now
        )
        now   <<- l2ComIn
      )
      u $ f stdio.putChar char
      u $ f stdio.printInt count
      pure unit
  )

  pure unit


prob_decompress :: MonadL2Global Unit
prob_decompress = do
  stdio <- importAs "stdio" l2LibStdIO
  libpq <- importAs "libpq" l2LibMinPriorityQueue
  
  main :: L2F (MonadL2Local Unit) <- l2VirtualFuncMain
  l2SetRealFunc main (do
      char  <- newsvar
      count <- newsvar
      i     <- newsvar
      char  <<- l2ComIn
      count <<- (f stdio.getInt :: ML2E Int) 
      l2RepNtimes count (
        u $ f stdio.putChar char
      )
      l2While (intcast (L2E $ L1EVal $ ptr realAddrRegLastChar) >^ 0) (do
        char  <<- (L2E $ L1EVal $ ptr realAddrRegLastChar  :: L2E Char)
        count <<- (f stdio.getInt :: ML2E Int) 
        l2RepNtimes count (
          u $ f stdio.putChar char
        )
      )
      pure unit
  )

  pure unit



gcd_str :: MonadL2Global Unit
gcd_str = do
  stdio <- importAs "stdio" l2LibStdIO
  libpq <- importAs "libpriorityqueue" l2LibMinPriorityQueue
  
  gcd :: L2F (L2E Int -> L2E Int -> MonadL2Local (L2E Int)) <- l2RealFunc "gcd" (\n m -> do
      g <- l2NewLocalVar
      l <- l2NewLocalVar
      t <- l2NewLocalVar
      l2IfElse (n <^ m) (do
        g <<- m
        l <<- n
      ) (do
        g <<- n
        l <<- m
      )

      l2While (l >^ 0) (do
        t <<- l
        l <<- g %^ l
        g <<- t
      )

      toML2E g
    )
  
  main :: L2F (MonadL2Local Unit) <- l2VirtualFuncMain
  l2SetRealFunc main (do
      a <- l2NewStaticVar
      b <- l2NewStaticVar
      --a <<- (l2ComIn :: ML2E Int)
      --b <<- (l2ComIn :: ML2E Int)
      --u $ f stdio.putChar (cast (f gcd a b :: ML2E Int))
      a <<- (f stdio.getInt :: ML2E Int)
      b <<- (f stdio.getInt :: ML2E Int)
      u $ f stdio.printInt (f gcd a b :: ML2E Int)
      pure unit
    )

  pure unit



prob_interpreter :: MonadL2Global Unit
prob_interpreter = do
  stdio <- importAs "stdio" l2LibStdIO

  let macro_readaddr tmp_int tmp_char tmp_at code line column = do
        tmp_int  <<- 0
        tmp_at   <<- 0
        tmp_char <<- (l2ComIn :: ML2E Int)

        l2While (tmp_char ==^ 32) do
          tmp_char <<- (l2ComIn :: ML2E Int)
        
        l2While (tmp_char ==^ 64) do
          tmp_char <<- (l2ComIn :: ML2E Int)
          tmp_at <<- tmp_at +^ 1
                
        l2While ((47 <^ tmp_char) &^ (tmp_char <^ 58) ) do
          tmp_int  <<- tmp_int *^ 10 +^ (tmp_char -^ 48)
          tmp_char <<- l2ComIn

        code ##^ (line *^ 7 +^ column *^ 2 +^ 1) <<- tmp_at
        code ##^ (line *^ 7 +^ column *^ 2 +^ 2) <<- tmp_int

  let macro_readval tmp_int tmp_char tmp_at code line column = do
        tmp_int  <<- 0
        tmp_at   <<- 0
        tmp_char <<- (l2ComIn :: ML2E Int)

        l2While (tmp_char ==^ 32) do
          tmp_char <<- (l2ComIn :: ML2E Int)
        
        l2IfElse (tmp_char ==^ 45) (do
            l2While ((47 <^ tmp_char) &^ (tmp_char <^ 58) ) do
              tmp_int  <<- tmp_int *^ 10 +^ (tmp_char -^ 48)
              tmp_char <<- l2ComIn

            code ##^ (line *^ 7 +^ column *^ 2 +^ 1) <<- 0
            code ##^ (line *^ 7 +^ column *^ 2 +^ 2) <<- tmp_int *^ (-1)
        ) (do
            l2While (tmp_char ==^ 64) do
              tmp_char <<- (l2ComIn :: ML2E Int)
              tmp_at <<- tmp_at +^ 1
                    
            l2While ((47 <^ tmp_char) &^ (tmp_char <^ 58) ) do
              tmp_int  <<- tmp_int *^ 10 +^ (tmp_char -^ 48)
              tmp_char <<- l2ComIn

            code ##^ (line *^ 7 +^ column *^ 2 +^ 1) <<- tmp_at
            code ##^ (line *^ 7 +^ column *^ 2 +^ 2) <<- tmp_int
        )



  let l2ComOut = stdio.macroPutChar
 
  
  main :: L2F (MonadL2Local Unit) <- l2VirtualFuncMain
  l2SetRealFunc main (do
    code <- newsvar
    code <<- (l2PrimGetHeap 0 :: ML2E (L2Arr Int))

    {-
      "_" : ((m,a)=> {})
      "!" : ((m,a)=> {m.output.push(a[0]);})
      "?" : ((m,a)=> {checkAddress(a[0]); if(m.input.length < 0) m.mem[a[0]] = 0 ; else m.mem[a[0]] = m.input.shift() | 0;})
      ":" : ((m,a)=> {checkAddress(a[0]); m.mem[a[0]] = ( a[1]) | 0;}),
      "~" : ((m,a)=> {checkAddress(a[0]); m.mem[a[0]] = (~a[1]) | 0;})
      "+" : ((m,a)=> {checkAddress(a[0]); m.mem[a[0]] = (a[1] +  a[2]) | 0;}),
      "-" : ((m,a)=> {checkAddress(a[0]); m.mem[a[0]] = (a[1] -  a[2]) | 0;}),
      "*" : ((m,a)=> {checkAddress(a[0]); m.mem[a[0]] = (a[1] *  a[2]) | 0;}),
      "/" : ((m,a)=> {checkAddress(a[0]); m.mem[a[0]] = (a[1] /  a[2]) | 0;}),
      "%" : ((m,a)=> {checkAddress(a[0]); m.mem[a[0]] = (a[1] %  a[2]) | 0;}),
      "&" : ((m,a)=> {checkAddress(a[0]); m.mem[a[0]] = (a[1] &  a[2]) | 0;}),
      "|" : ((m,a)=> {checkAddress(a[0]); m.mem[a[0]] = (a[1] |  a[2]) | 0;}),
      "^" : ((m,a)=> {checkAddress(a[0]); m.mem[a[0]] = (a[1] ^  a[2]) | 0;}),
      "}" : ((m,a)=> {checkAddress(a[0]); m.mem[a[0]] = (a[1] >> a[2]) | 0;}),
      "{" : ((m,a)=> {checkAddress(a[0]); m.mem[a[0]] = (a[1] << a[2]) | 0;}),
      ">" : ((m,a)=> {checkAddress(a[0]); m.mem[a[0]] = (a[1] >  a[2]) | 0;}),
      "<" : ((m,a)=> {checkAddress(a[0]); m.mem[a[0]] = (a[1] <  a[2]) | 0;}),
      "=" : ((m,a)=> {checkAddress(a[0]); m.mem[a[0]] = (a[1] == a[2]) | 0;})
    -}

    flag <- newsvar
    flag <<- true
    op    <- newsvar
    line  <- newsvar
    atcnt <- newsvar
    int   <- newsvar
    tmp_char <- newsvar
    tmp_int  <- newsvar
    tmp_at   <- newsvar

    l2While (flag) (do
      tmp_char <<- (l2ComIn :: ML2E Int)

      l2IfElse (tmp_char ==^ cast ';') (flag <<- false) (do
        l2Case [
          Tuple (tmp_char ==^ cast '_') (op <<- 0 ),
          Tuple (tmp_char ==^ cast '!') (op <<- 1 ),
          Tuple (tmp_char ==^ cast '?') (op <<- 2 ),
          Tuple (tmp_char ==^ cast ':') (op <<- 3 ),
          Tuple (tmp_char ==^ cast '~') (op <<- 4 ),
          Tuple (tmp_char ==^ cast '+') (op <<- 5 ),
          Tuple (tmp_char ==^ cast '-') (op <<- 6 ),
          Tuple (tmp_char ==^ cast '*') (op <<- 7 ),
          Tuple (tmp_char ==^ cast '/') (op <<- 8 ),
          Tuple (tmp_char ==^ cast '%') (op <<- 9 ),
          Tuple (tmp_char ==^ cast '&') (op <<- 10),
          Tuple (tmp_char ==^ cast '|') (op <<- 11),
          Tuple (tmp_char ==^ cast '^') (op <<- 12),
          Tuple (tmp_char ==^ cast '}') (op <<- 13),
          Tuple (tmp_char ==^ cast '{') (op <<- 14),
          Tuple (tmp_char ==^ cast '>') (op <<- 15),
          Tuple (tmp_char ==^ cast '<') (op <<- 16),
          Tuple (tmp_char ==^ cast '=') (op <<- 17)
        ] (do
            stdio.macroPutString "syntax error: wrong operation at program["
            u $ f stdio.printInt line
            stdio.macroPutString "] \""
            u $ f stdio.putChar (cast tmp_char)
            u $ f stdio.putChar '"'
            l2Return 0
        )
        code ##^ (line *^ 7) <<- op

        l2Case [
          Tuple (op ==^ 0) (pure unit),
          Tuple (op ==^ 1) (do
            macro_readval  tmp_int tmp_char tmp_at code line 0
          ),
          Tuple (op ==^ 2) (do
            macro_readaddr tmp_int tmp_char tmp_at code line 0
          ),
          Tuple (op <^ 5) (do
            macro_readaddr tmp_int tmp_char tmp_at code line 0
            macro_readval  tmp_int tmp_char tmp_at code line 1
          )
        ] (do
            macro_readaddr tmp_int tmp_char tmp_at code line 0
            macro_readval  tmp_int tmp_char tmp_at code line 1
            macro_readval  tmp_int tmp_char tmp_at code line 2
        )


        l2While (tmp_char !=^ 10) do
          tmp_char <<- (l2ComIn :: ML2E Int)
        
        line <<- line +^ 1
      )
    )


    i <- newsvar
    i <<- 0
    l2While (i <^ line) (do
        op <<- code #^ (i *^ 7)
        l2Case [
          Tuple (op ==^ 0 ) (l2ComOut '_'                                                                                                                                                                                                                                                                                                                                                                         *> l2ComOut '\n'),
          Tuple (op ==^ 1 ) (l2ComOut '!' *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 1)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 2)))                                                                                                                                                                                                                                                 *> l2ComOut '\n'),
          Tuple (op ==^ 2 ) (l2ComOut '?' *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 1)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 2)))                                                                                                                                                                                                                                                 *> l2ComOut '\n'),
          Tuple (op ==^ 3 ) (l2ComOut ':' *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 1)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 2))) *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 3)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 4)))                                                                                                                         *> l2ComOut '\n'),
          Tuple (op ==^ 4 ) (l2ComOut '~' *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 1)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 2))) *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 3)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 4)))                                                                                                                         *> l2ComOut '\n'),
          Tuple (op ==^ 5 ) (l2ComOut '+' *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 1)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 2))) *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 3)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 4))) *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 5)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 6))) *> l2ComOut '\n'),
          Tuple (op ==^ 6 ) (l2ComOut '-' *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 1)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 2))) *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 3)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 4))) *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 5)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 6))) *> l2ComOut '\n'),
          Tuple (op ==^ 7 ) (l2ComOut '*' *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 1)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 2))) *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 3)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 4))) *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 5)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 6))) *> l2ComOut '\n'),
          Tuple (op ==^ 8 ) (l2ComOut '/' *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 1)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 2))) *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 3)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 4))) *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 5)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 6))) *> l2ComOut '\n'),
          Tuple (op ==^ 9 ) (l2ComOut '%' *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 1)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 2))) *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 3)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 4))) *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 5)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 6))) *> l2ComOut '\n'),
          Tuple (op ==^ 10) (l2ComOut '&' *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 1)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 2))) *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 3)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 4))) *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 5)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 6))) *> l2ComOut '\n'),
          Tuple (op ==^ 11) (l2ComOut '|' *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 1)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 2))) *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 3)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 4))) *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 5)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 6))) *> l2ComOut '\n'),
          Tuple (op ==^ 12) (l2ComOut '^' *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 1)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 2))) *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 3)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 4))) *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 5)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 6))) *> l2ComOut '\n'),
          Tuple (op ==^ 13) (l2ComOut '}' *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 1)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 2))) *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 3)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 4))) *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 5)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 6))) *> l2ComOut '\n'),
          Tuple (op ==^ 14) (l2ComOut '{' *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 1)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 2))) *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 3)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 4))) *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 5)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 6))) *> l2ComOut '\n'),
          Tuple (op ==^ 15) (l2ComOut '>' *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 1)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 2))) *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 3)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 4))) *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 5)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 6))) *> l2ComOut '\n'),
          Tuple (op ==^ 16) (l2ComOut '<' *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 1)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 2))) *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 3)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 4))) *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 5)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 6))) *> l2ComOut '\n'),
          Tuple (op ==^ 17) (l2ComOut '=' *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 1)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 2))) *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 3)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 4))) *> l2ComOut ' ' *> l2RepNtimes (code #^ (i *^ 7 +^ 5)) (l2ComOut '@') *> (u $ f stdio.printInt (code #^ (i *^ 7 +^ 6))) *> l2ComOut '\n')
        ] (pure unit)
        i <<- i +^ 1
        pure unit
      )
  )

  pure unit




test_case :: MonadL2Global Unit
test_case = do
  stdio <- importAs "stdio" l2LibStdIO
  
  main :: L2F (MonadL2Local Unit) <- l2VirtualFuncMain
  l2SetRealFunc main (do
    l2Case [
      Tuple (3 ==^ 0) (l2ComIn *> stdio.macroPutString "0" ),
      Tuple (3 ==^ 1) (l2ComIn *> stdio.macroPutString "1" ),
      Tuple (3 ==^ 2) (l2ComIn *> stdio.macroPutString "2" ),
      Tuple (3 ==^ 3) (l2ComIn *> stdio.macroPutString "3" ),
      Tuple (3 ==^ 4) (l2ComIn *> stdio.macroPutString "4" )
    ] (do 
        stdio.macroPutString "syntax error: wrong operation"
        l2Return 0
    )
  )

  pure unit