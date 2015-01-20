#ifndef __LIBGRAPH_H_
#define __LIBGRAPH_H_

#include <map>
#include <string>
#include <iostream>
#include <cstdlib>
#include <algorithm>
#include <cstdarg>
#include <set>

using namespace std;

void raiseError(const string &msg) {
	cerr << msg << endl;
	exit(1);
}

class graph;
struct node;

class edge {
public:
	string src, dst; //symbols of source and destination node
	int weight;
	const graph *g; //ptr to graph to which this node belongs

	edge() {}

	edge(const edge &o) : src(o.src), dst(o.dst), weight(o.weight), g(o.g) {}

	edge(const string &src, const string &dst, int weight, const graph *g) : src(src), dst(dst), weight(weight), g(g) {}

	int getWeight() const {
		return weight;
	}

	bool operator<(const edge &o) const {
		return weight < o.weight;
	}

};

struct edge_decl {
	string src, dst;
	int weight;
	edge_decl(const string &src, const string &dst, int weight) : src(src), dst(dst), weight(weight) {}
};

struct node {
	string symbol;
	const graph *g; //ptr to graph to which this node belongs

	node() {}

	node(const string &symbol, const graph *g) : symbol(symbol), g(g) {}

	bool operator==(const node &o) const {
		return symbol == o.symbol && g == o.g;
	}
};

class graph {
	int nextNodeIndex;
	map<string, int> nodeIndex;
	map< pair<string, string>, int > weight;
	vector<string> nodes;
public:
	graph() : nextNodeIndex(0) {}

	node getNode(const string &symbol) const {
		if(nodeIndex.find(symbol) == nodeIndex.end()) {
			raiseError("Node " + symbol + " does not exist.");
		}
		return node(symbol, this);
	}

	node getNode(int id) const {
		return node(nodes[id], this);
	}

	int getNodeIndex(const string &symbol) const {
		map<string, int>::const_iterator it = nodeIndex.find(symbol);
		if(it == nodeIndex.end()) {
			raiseError("Node " + symbol + " does not exist.");
		}
		return it->second;
	}

	int getNodeCount() const {
		return int(nodes.size());
	}

	int getEdgeCount() const {
		return int(weight.size());
	}

	vector<node> getInNeighbours(const string &symbol) const {
		vector<node> ret;
		for(map<pair<string, string>, int>::const_iterator it = weight.begin(); it != weight.end(); it++) {
			if((it->first).second == symbol) ret.push_back(node((it->first).first, this));
		}
		return ret;
	}

	vector<node> getOutNeighbours(const string &symbol) const {
		vector<node> ret;
		for(map<pair<string, string>, int>::const_iterator it = weight.begin(); it != weight.end(); it++) {
			if((it->first).first == symbol) ret.push_back(node((it->first).second, this));
		}
		return ret;
	}

	edge getEdge(const string &a, const string &b) const {
		map<pair<string, string>, int>::const_iterator it = weight.find(make_pair(a, b));
		if(it == weight.end()) raiseError("Edge <" + a + ", " + b + "> does not exist.");
		return edge((it->first).first, (it->first).second, it->second, this);
	}

	vector<edge> getAllEdges() const {
		vector<edge> edges;
		for(map<pair<string, string>, int>::const_iterator it = weight.begin(); it != weight.end(); it++) {
			edges.push_back(edge((it->first).first, (it->first).second, it->second, this));
		}
		return edges;
	}

	void addEdge(const string &src, const string &dst, int w = 0) {
		if(nodeIndex.find(src) == nodeIndex.end()) {
			nodeIndex[src] = nextNodeIndex++;
			nodes.push_back(src);
		}
		if(nodeIndex.find(dst) == nodeIndex.end()) {
			nodeIndex[dst] = nextNodeIndex++;
			nodes.push_back(dst);
		}
		weight[make_pair(src, dst)] = w;
	}

	void deleteEdge(const string &src, const string &dst) {
		weight.erase(make_pair(src, dst));
	}

};

graph newGraph(int numEdges, ...) {
	va_list edges;
	va_start(edges, numEdges);
	graph g;
	for(int i=0; i<numEdges; ++i) {
		edge_decl *e = va_arg(edges, edge_decl*);
		g.addEdge(e->src, e->dst, e->weight);
		delete e;
	}
	va_end(edges);
	return g;
}

vector<node> _getInNeighbours(const graph &g, const node &n) {
	return g.getInNeighbours(n.symbol);
}

vector<node> _getOutNeighbours(const graph &g, const node &n) {
	return g.getOutNeighbours(n.symbol);
}

int _getID(const node &n) {
	return (n.g)->getNodeIndex(n.symbol);
}

node _getNode(const graph &g, string symbol) {
	return g.getNode(symbol);
}

node _getNode(const graph &g, int id) {
	return g.getNode(id);
}

edge _getEdge(const graph &g, const string &src, const string &dst) {
	return g.getEdge(src, dst);
}

edge _getEdge(const graph &g, const node &src, const node &dst) {
	return g.getEdge(src.symbol, dst.symbol);
}

int _getWeight(const edge &e) {
	return e.getWeight();
}

int _getWeight(const graph &g, const node &src, const node &dst) {
	return g.getEdge(src.symbol, dst.symbol).getWeight();
}

void _addEdge(graph &g, const string &src, const string &dst, int weight = 0) {
	g.addEdge(src, dst, weight);
}

vector<edge> _getAllEdges(const graph &g) {
	return g.getAllEdges();
}

int _getNodeCount(const graph &g) {
	return g.getNodeCount();
}

int _getEdgeCount(const graph &g) {
	return g.getEdgeCount();
}

node _getSrc(const edge &e) {
	return node(e.src, e.g);
}

node _getDst(const edge &e) {
	return node(e.dst, e.g);
}

void _sort(vector<int> &v) {
	sort(v.begin(), v.end());
}

void _sort(vector<char> &v) {
	sort(v.begin(), v.end());
}

void _sort(vector<string> &v) {
	sort(v.begin(), v.end());
}

void _sort(vector<edge> &v) {
	sort(v.begin(), v.end());
}

#endif
