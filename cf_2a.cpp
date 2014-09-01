#include <iostream>
#include <string>
#include <map>
#include <vector>
#include <climits>

using namespace std;

int cf_2a_main() {
    int n;
    cin >> n;
    vector<pair<string, int>> entries(n);
    map<string, long int> final_scores;
    for (int i = 0; i < n; ++i) {
        string name;
        int score;
        cin >> name;
        cin >> score;
        entries.push_back(make_pair(name, score));
        final_scores[name] += score;
    }
    long int max_score = LONG_MIN;
    for (auto& kv : final_scores)
    if (kv.second > max_score)
        max_score = kv.second;
    map<string, long int> scores;
    for (auto& kv : entries)
    if ((scores[kv.first] += kv.second) >= max_score && final_scores[kv.first] == max_score) {
        cout << kv.first;
        return 0;
    }
    return 0;
}