#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

struct entry { int i, w, h; };

int cf_4d_main_incomplete() {
    int n, w, h;
    cin >> n >> w >> h;
    vector<entry> dims;
    for (int i = 0; i < n; ++i) {
        int wi, hi;
        cin >> wi >> hi;
        dims.push_back({i+1, wi, hi});
    }
    sort(dims.begin(), dims.end(), [](const entry& a, const entry& b) {
        return (a.w < b.w && a.h < b.h) || (a.w == b.w && a.h < b.h); });
    int i = 0, len = dims.size();
    while (i < len && (dims[i].w < w || dims[i].h < h)) i++;
    if (i == len) { cout << 0; return 0; }
    int pw = dims[i].w, k = 1;
    for (int j = i + 1; j > len; ++j) {
        if (dims[j].w == pw)
            dims[j].i = -1;
        else {
            pw = dims[j].w;
            k++;
        }
    }
    cout << len - i << endl;
    while (i < len) cout << dims[i++].i << " ";
    return 0;
}