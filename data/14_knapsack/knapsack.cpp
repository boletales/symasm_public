#include <bits/stdc++.h>
using namespace std;

const int INF = INT_MAX;







int main() {
  int cnt, weightmax;                                    //  cnt       <- l2NewLocalVar
                                                        //  weightmax <- l2NewLocalVar
                                                        //  wmp1      <- l2NewLocalVar
  cin >> cnt >> weightmax;                              //  cnt       <<- (f stdio.getInt :: ML2E Int)
                                                        //  weightmax <<- (f stdio.getInt :: ML2E Int)
                                                        //  wmp1      <<- weightmax +^ 1
                                                        //  
  vector<int> weights = vector<int>(cnt);                 //  weights <- l2NewLocalVar
  vector<int> values  = vector<int>(cnt);                 //  values  <- l2NewLocalVar
                                                        //  weights <<- (l2PrimGetHeap cnt :: ML2E (L2Arr Int))
                                                        //  values  <<- (l2PrimGetHeap cnt :: ML2E (L2Arr Int))
                                                        //  
  vector<vector<int>> dp  = vector<vector<int>>(cnt + 1); //  dp  <- l2NewLocalVar
                                                        //  dp <<- (l2PrimGetHeap (cnt +^ 1) :: ML2E (L2Arr (L2Arr Int)))
                                                        //  
                                                        //  i <- l2NewLocalVar
                                                        //  i <<- 0
  for(int i=0; i<cnt; i++){                              //  l2While (i <^ cnt) (do
    cin >> values[i] >> weights[i];                     //    weights ##^ i <<- (f stdio.getInt :: ML2E Int)
                                                        //    values  ##^ i <<- (f stdio.getInt :: ML2E Int)
    dp[i] = vector<int>(weightmax + 1);                  //    dp      ##^ i <<- (l2PrimGetHeap wmp1 :: ML2E (L2Arr Int))
                                                        //    i <<- i +^ 1
  }                                                     //  )
  dp[cnt] = vector<int>(weightmax + 1);                  //  dp ##^ cnt <<- (l2PrimGetHeap wmp1 :: ML2E (L2Arr Int))
                                                        //  
                                                        //  w <- l2NewLocalVar
                                                        //  wnow <- l2NewLocalVar
                                                        //  vnow <- l2NewLocalVar
                                                        //  dpi  <- l2NewLocalVar
                                                        //  dpi1 <- l2NewLocalVar
                                                        //  v1 <- l2NewLocalVar
                                                        //  v2 <- l2NewLocalVar
                                                        //  i <<- 0
  for(int i=0; i<cnt; i++){                              //  l2While (i <^ cnt) (do
                                                        //    w <<- 0
                                                        //    vnow <<- values #^ i
                                                        //    dpi  <<- dp #^ i
                                                        //    dpi1 <<- dp #^ (i+^1)
    for(int w=0; w<=weightmax; w++){                     //    l2While (w <^ wmp1) (do
                                                        //      wnow <<- weights #^ i
      if(w < weights[i]){                               //      l2IfElse (w <^ wnow) (do
        dp[i+1][w] = dp[i][w];                          //        dpi1 ##^ w <<- dpi #^ w
      }else{                                            //      )(do  
        int v1 = dp[i][w];                               //        v1 <- dpi #^ w
        int v2 = dp[i][w-weights[i]] + values[i];        //        v2 <- (dpi #^ (w -^ wnow)) +^ vnow
        if(v1 < v2){                                    //        l2IfElse (v1 <^ v2) (do
          dp[i+1][w] = v2;                              //          dpi1 ##^ w <<- v2
        }else{                                          //        ) (do
          dp[i+1][w] = v1;                              //          dpi1##^ w <<- v1
        }                                               //        )
      }                                                 //      )
                                                        //      w <<- w +^ 1
    }                                                   //    )
                                                        //    i <<- i +^ 1
  }                                                     //  )
  cout << dp[cnt][weightmax];                           //  u $ f stdio.printInt (dp #^ cnt #^ weightmax)

  return 0;
}