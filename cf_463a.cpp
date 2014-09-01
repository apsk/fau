#include <iostream>

using namespace std;

int cf_463a_main() {
    int n, s, x = -1;
    cin >> n >> s;
    for (int i = 0; i < n; ++i) {
        int d, c;
        cin >> d >> c;
        if (d < s && c != 0 && 100 - c > x)
            x = 100 - c;
        else if (d <= s && c == 0 && x == -1)
            x = 0;
    }
    if (x >= 0) cout << x;
    else cout << -1;
    return 0;
}