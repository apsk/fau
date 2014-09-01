#include <iostream>
#include <string>
#include <algorithm>

using namespace std;

int cf_3a_main() {
    string s, t;
    cin >> s >> t;
    int xS = s[0] - 'a', yS = s[1] - '0' - 1,
        xT = t[0] - 'a', yT = t[1] - '0' - 1;
    cout << max(abs(xS-xT), abs(yS-yT)) << endl;
    while (xS != xT || yS != yT) {
        if (xS < xT) {
            cout << 'R';
            xS++;
        }
        else if (xS > xT) {
            cout << 'L';
            xS--;
        }
        if (yS < yT) {
            cout << 'U';
            yS++;
        }
        else if (yS > yT) {
            cout << 'D';
            yS--;
        }
        cout << endl;
    }
    return 0;
}