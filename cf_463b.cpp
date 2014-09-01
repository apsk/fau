#include <iostream>

using namespace std;

int cf_463b_main() {
    int n, x = 0, y;
    cin >> n;
    for (int i = 0; i < n; ++i) {
        cin >> y;
        if (y > x)
            x = y;
    }
    cout << x;
    return 0;
}