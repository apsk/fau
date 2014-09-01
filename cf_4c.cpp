#include <iostream>
#include <string>
#include <map>

using namespace std;

int cf_4c_main() {
    int n;
    cin >> n;
    string s;
    map<string, int> db;
    for (int i = 0; i < n; ++i) {
        cin >> s;
        int x = db[s];
        if (x == 0) cout << "OK" << endl;
        else {
            cout << s << x << endl;
        }
        db[s]++;
    }
    return 0;
}