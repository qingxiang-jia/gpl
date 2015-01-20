#ifndef __LIBPRINT_H_
#define __LIBPRINT_H_

#include <iostream>
#include <string>
#include "libgraph.h"

using namespace std;

void _print(int n) {
	cout << n << endl;
}

void _print(double n) {
	cout << n << endl;
}

void _print(string n) {
	cout << n << endl;
}

void _print(char c) {
	cout << c << endl;
}

void _print(const char *s) {
	cout << string(s) << endl;
}

void _print(bool v) {
	cout << (v ? "true" : "false") << endl;
}

void _print(const node &n) {
	cout << n.symbol.substr(1) << endl;
}

#endif
