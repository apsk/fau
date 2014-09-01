#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int cf_4b_main() {
    int d, s, minsum = 0, maxsum = 0;
    cin >> d >> s;
    vector<int> mins(d), maxs(d);
    for (int i = 0; i < d; ++i) {
        int min, max;
        cin >> min >> max;
        mins[i] = min;
        maxs[i] = max;
        minsum += min;
        maxsum += max;
    }
    if (s < minsum || s > maxsum) {
        cout << "NO";        
    }
    else {
        s -= minsum;
        cout << "YES" << endl;
        for (int i = 0; i < d; ++i) {
            if (s == 0) {
                cout << mins[i] << " ";
            }
            else {
                int diff = min(s, maxs[i] - mins[i]);
                cout << mins[i] + diff << " ";
                s -= diff;
            }
        }
    }
    return 0;
}