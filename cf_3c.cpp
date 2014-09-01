#include <iostream>
#include <vector>
#include <string>

using namespace std;

int cf_3c_main() {
    string m, s;
    for (int i = 0; i < 3; ++i) {
        cin >> s;
        m += s;
    }
    int nX = 0, n0 = 0;
    for (int i = 0; i < 9; ++i)
    if (m[i] == 'X') nX++;
    else if (m[i] == '0') n0++;
    if (nX != n0 && nX != n0 + 1) {
        cout << "illegal";
        return 0;
    }
    int rX = 0, r0 = 0;
    for (int i = 0; i < 3; ++i) {
        char c1, c2, c3;
        c1 = m[i * 3 + 0];
        c2 = m[i * 3 + 1];
        c3 = m[i * 3 + 2];
        if (c1 == c2 && c2 == c3 && c3 == 'X') rX++;
        if (c1 == c2 && c2 == c3 && c3 == '0') r0++;
        c1 = m[i];
        c2 = m[3 + i];
        c3 = m[6 + i];
        if (c1 == c2 && c2 == c3 && c3 == 'X') rX++;
        if (c1 == c2 && c2 == c3 && c3 == '0') r0++;
    }
    if (m[0] == m[4] && m[4] == m[8] && m[8] == 'X') rX++;
    if (m[0] == m[4] && m[4] == m[8] && m[8] == '0') r0++;
    if (m[2] == m[4] && m[4] == m[6] && m[6] == 'X') rX++;
    if (m[2] == m[4] && m[4] == m[6] && m[6] == '0') r0++;
    if ((rX > 0 && (r0 > 0 || n0 >= nX)) || (r0 > 0 && nX > n0)) { cout << "illegal"; return 0; }
    else if (rX > 0) goto firstwon;
    else if (r0 > 0) goto secondwon;
    if (nX == 5 && n0 == 4) cout << "draw";
    else if (nX > n0) cout << "second";
    else cout << "first";
    return 0;
firstwon:
    cout << "the first player won";
    return 0;
secondwon:
    cout << "the second player won";
    return 0;
}