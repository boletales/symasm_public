#include <bits/stdc++.h>
using namespace std;

#define ll long long
const ll INF = LLONG_MAX;

int main() {
  ll towns, roads, start, goal, tmp_from, tmp_to, tmp_cost;
  cin >> towns >> roads >> start >> goal;
  map<pair<ll,ll>,ll>  data ;
  for(ll i=0; i<roads; i++){
    cin >> tmp_from >> tmp_to >> tmp_cost;
    if(data.count(pair(tmp_from,tmp_to)) > 0){
      data.insert_or_assign(pair(tmp_from,tmp_to), min(tmp_cost, data.at(pair(tmp_from,tmp_to))));
    }else{
      data.insert(pair(pair(tmp_from,tmp_to), tmp_cost));
    }
  }

  priority_queue<pair<int, int>, vector<pair<int, int>>, greater<pair<int, int>>> queue;
  queue.push(pair(0, start));

  vector<bool> seen = vector<bool>(towns, false);
  while(true){
    if(queue.empty()){
      cout << "no";
      return 0;
    }else{
      pair<int, int> e = queue.top();
      ll cost = e.first;
      ll town = e.second;
      queue.pop();
      if(seen[town]){

      }else{
        seen[town] = true;
        if(e.second == goal) {
          cout << e.first;
          return 0;
        }else{
          for(ll i=0; i<towns; i++){
            if(data.count(pair(town,i)) > 0){
              queue.push(pair(cost+data.at(pair(town,i)), i));
            }
          }
        }
      }
    }
  }
  return 0;
}