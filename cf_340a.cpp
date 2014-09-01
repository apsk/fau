#include <iostream>
#include <complex>

using namespace std;

typedef unsigned int uint;

uint gcd(uint a, uint b) {
    return b == 0 ? a : gcd(b, a % b);
}

uint lcm(uint a, uint b) {
    return (a * b) / gcd(a, b);
}

int cf_340a_main() {
    uint x, y, a, b, d;
    cin >> x >> y >> a >> b;
    d = lcm(x, y);
    cout << (b / d - (a - 1) / d);
    return 0;
}