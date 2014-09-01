#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int cf_3b_main() {
    int n;
    long int w;
    cin >> n >> w;
    vector<pair<int,int>> boats1, boats2, boats3;
    for (int i = 0; i < n; ++i) {
        int t, p;
        cin >> t >> p;
        (t == 1 ? boats1 : boats2).push_back(make_pair(i+1, p));
    }
    auto cmp = [](const pair<int, int>& a, const pair<int, int>& b) { return a.second < b.second; };
    sort(boats1.begin(), boats1.end(), cmp);
    sort(boats2.begin(), boats2.end(), cmp);
    int i1 = boats1.size() - 1, i2 = boats2.size() - 1;
    while (w > 0) {
        if (i1 < 0) {
            while (i2 >= 0 && w > 1) {
                boats3.push_back(boats2[i2]);
                i2--;
                w -= 2;
            }
            break;
        }
        if (i2 < 0) {
            while (i1 >= 0 && w > 0) {
                boats3.push_back(boats1[i1]);
                i1--;
                w--;
            }
            break;
        }
        if (w == 1 || boats1[i1].second + (i1 == 0 ? 0 : boats1[i1 - 1].second) > boats2[i2].second) {
            boats3.push_back(boats1[i1]);
            boats1.pop_back();
            i1--;
            w--;
        }
        else {
            boats3.push_back(boats2[i2]);
            boats2.pop_back();
            i2--;
            w -= 2;
        }
    }
    long int v = 0;
    for (auto& boat : boats3)
        v += boat.second;
    cout << v << endl;
    for (auto& boat : boats3)
        cout << boat.first << " ";
    return 0;
}

/*
if (i1 < 0) {
while (i2 >= 0 && w > 0) {
boats3.push_back(boats2[i2]);
i2--;
w -= 2;
}
break;
}
if (i2 < 0) {
while (i1 >= 0 && w > 0) {
boats3.push_back(boats1[i1]);
i1--;
w--;
}
break;
}
*/

/*
if (w == 1) {
if (i1 >= 0)
boats3.push_back(boats1[i1]);
break;
}
int x = 0;
if (i1 >= 0) {
x = boats1[i1].second;
if (i1 != 0)
x += boats1[i1 - 1].second;
}
else if (i2 < 0)
break;
if (i1 >= 0 && (i2 < 0 || x > boats2[i2].second)) {
boats3.push_back(boats1[i1]);
boats1.pop_back();
i1--;
if (i1 >= 0) {
boats3.push_back(boats1[i1]);
boats1.pop_back();
i1--;
}
}
else {
boats3.push_back(boats2[i2]);
boats2.pop_back();
i2 -= 1;
}
w -= 2;
*/