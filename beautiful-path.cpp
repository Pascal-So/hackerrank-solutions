#include <bits/stdc++.h>
using namespace std;


int main() {
    
    int n, m;
    cin >> n >> m;
    vector<vector<pair<int, int> > > out_edges (n); // dest, cost
    vector<bool> visited (n, false);
    for(int i = 0; i < m; ++i){
        int u, v, c;

        cin >> u >> v >> c;
	--u;
	--v;
        out_edges[u].push_back({v, c});
        out_edges[v].push_back({u, c});
    }

    int s, e;
    cin >> s >> e;
    --s;
    --e;
    
    priority_queue<pair<int, int> > q;
    q.push({0, s});
    
    int out;
    
    while(! q.empty()){
        pair<int,int> tmp = q.top(); q.pop();
        int curr = tmp.second;
        int cost = -tmp.first;
        if(visited[curr]){
            continue;
        }
        
        if(curr==e){
            out = cost;
            break;
        }
        
        visited[curr] = true;
        for(auto e:out_edges[curr]){
            q.push({-(cost | e.second), e.first});
        }
    }
    
    cout<<out<<endl;
    
    return 0;
}
