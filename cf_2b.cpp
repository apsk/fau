#include <iostream>
#include <string>
#include <vector>
#include <algorithm>

#define UP   true
#define LEFT false

typedef unsigned int uint;

using namespace std;

uint times_divisible_by(uint x, uint y) {
    if (x == 0) return 0;
    uint n = 0;
    while (x % y == 0) {
        n += 1;
        x /= y;
    }
    return n;
}

pair<uint, string> min_powers_path(vector<uint>& inputs, int n, int x) {
    vector<bool> steps(n*n);
    vector<uint> counts(n*n);
    counts[0] = times_divisible_by(inputs[0], x);
    for (int r = 0; r < n; ++r)
    for (int c = 0; c < n; ++c) {
        int i = r * n + c;
        int u = r == 0 ? INT_MAX : counts[(r - 1)*n + c];
        int l = c == 0 ? INT_MAX : counts[i - 1];
        int d = times_divisible_by(inputs[i], x);
        if (u < l) {
            steps[i] = UP;
            counts[i] = u + d;
        }
        else if (l != INT_MAX) {
            steps[i] = LEFT;
            counts[i] = l + d;
        }
    }
    string path = "";
    for (int r = n - 1, c = n - 1;;) {
        int i = r*n + c;
        if (steps[i] == UP) {
            path += 'D';
            r -= 1;
        }
        else {
            path += 'R';
            c -= 1;
        }
        if (r == 0 && c == 0)
            break;
    }
    reverse(path.begin(), path.end());
    return make_pair(counts[n*n - 1], path);
}

void cf_2b_main() {
    int z_i = -1;
    int n;
    cin >> n;
    vector<uint> inputs(n*n);
    for (int r = 0; r < n; ++r)
    for (int c = 0; c < n; ++c) {
        uint x;
        cin >> x;
        int i = r*n + c;
        if (x == 0) z_i = i;
        inputs[i] = x;
    }
    auto a = min_powers_path(inputs, n, 2);
    auto b = min_powers_path(inputs, n, 5);
    if (z_i != -1 && a.first > 1 && b.first > 1) {
        cout << 1 << endl;
        for (int i = 0; i < z_i / n; ++i) cout << 'D';
        for (int i = 0; i < n - 1; ++i) cout << 'R';
        for (int i = z_i / n; i < n - 1; ++i) cout << 'D';
    }
    else if (a.first < b.first)
        cout << a.first << endl << a.second;
    else
        cout << b.first << endl << b.second;
}