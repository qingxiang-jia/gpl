int _len(const string &s) {
	return s.size();
}

bool _empty(const string &s) {
	return s.empty();
}

char _at(const string &s, int i) {
	return s.at(i);
}

void _append(string &a, const string &b) {
	a += b;
}

void _append(string &s, char c) {
	s += c;
}

string _substr(const string &s, int pos, int len) {
	return s.substr(pos, len);
}
