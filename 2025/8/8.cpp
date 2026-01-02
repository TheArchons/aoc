#include<vector>
#include <cassert>
#include <fstream>
#include <iostream>

using namespace std;

class DSU {
public:
    DSU(const long long size) : parents(size, -1), sizes(size, -1) {}

    void make_set(const long long v) {
        parents.at(v) = v;
        sizes.at(v) = 1;
    }

    long long find_representative(const long long v) {
        long long parent = parents.at(v);
        if (parent == -1) {
            return -1;
        }
        if (parent == v) {
            return v;
        }

        return parents.at(v) = find_representative(parent);
    }

    void setUnion(const long long a, const long long b) {
        long long arep = find_representative(a);
        long long brep = find_representative(b);

        if (arep != brep) {
            if (sizes.at(brep) > sizes.at(arep)) {
                swap(arep, brep);
            }

            parents.at(brep) = arep;
            sizes.at(arep) += sizes.at(brep);
        }
    }

    bool inset(const long long v) {
        return find_representative(v) != -1;
    }

    long long size(const long long v) const {
        return sizes.at(v);
    }

    bool is_representative(const long long v) const {
        return parents.at(v) == v;
    }

private:
    vector<long long> parents;
    vector<long long> sizes;
};

void testCorrectness() {
    DSU dsu(1000);
    dsu.make_set(1);
    dsu.make_set(2);
    dsu.make_set(3);
    dsu.make_set(4);

    assert(dsu.find_representative(1) == 1);
    assert(dsu.find_representative(2) == 2);
    assert(dsu.find_representative(3) == 3);
    assert(dsu.find_representative(4) == 4);

    dsu.setUnion(1, 2);
    dsu.setUnion(3, 4);

    assert(dsu.find_representative(1) == dsu.find_representative(2));
    assert(dsu.find_representative(3) == dsu.find_representative(4));
    assert(dsu.find_representative(1) != dsu.find_representative(3));
    assert(dsu.find_representative(2) != dsu.find_representative(4));

    dsu.setUnion(1, 4);
    assert(dsu.find_representative(1) == dsu.find_representative(3));
    assert(dsu.find_representative(2) == dsu.find_representative(3));
    assert(dsu.find_representative(1) == dsu.find_representative(4));
    assert(dsu.find_representative(2) == dsu.find_representative(4));

    assert(dsu.inset(1));
    assert(!dsu.inset(5));
}

struct Connection {
    long long a;
    long long b;
    float distance;

    bool operator>(const Connection& rhs) const {
        return distance > rhs.distance;
    }
};

vector<long long> parseLine(string& line) {
    vector<long long> res;
    string numStr;
    for (char c : line) {
        if (c == ',') {
            res.push_back(stoi(numStr));
            numStr = "";
            continue;
        }
        numStr.push_back(c);
    }
    res.push_back(stoi(numStr));
    return res;
}

vector<long long> subVec(const vector<long long>& v1, const vector<long long>& v2) {
    vector<long long> res;
    for (long long i = 0; i < v1.size(); i++) {
        res.push_back(v1.at(i) - v2.at(i));
    }
    return res;
}

float norm(const vector<long long>& v) {
    float sum = 0;
    for (long long val : v) {
        sum += val * val;
    }

    return sqrt(sum);
}

int main() {
    testCorrectness();

    ifstream input("input");

    vector<string> lines;

    string line;
    while (getline(input, line)) {
        lines.push_back(line);
    }

    priority_queue<Connection, vector<Connection>, greater<Connection>> pq;

    vector<vector<long long>> vecs;

    for (string line : lines) {
        vecs.push_back(parseLine(line));
    }

    for (long long i = 0; i < vecs.size(); i++) {
        for (long long j = i + 1; j < vecs.size(); j++) {
            pq.push({i, j, norm(subVec(vecs.at(i), vecs.at(j)))});
        }
    }

    DSU dsu(2000);

    for (long long i = 0; i < vecs.size(); i++) {
        dsu.make_set(i);
    }

    for (long long i = 0;; i++) {
        Connection connection = pq.top();
        pq.pop();
        dsu.setUnion(connection.a, connection.b);
        if (dsu.size(dsu.find_representative(connection.a)) == vecs.size()) {
            cout << vecs.at(connection.a).at(0) *  vecs.at(connection.b).at(0);
            break;
        }
    }

    return 0;
}