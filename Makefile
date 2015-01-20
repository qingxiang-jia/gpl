test: gpl
	./runtests.sh

gpl:
	make -C src/ gpl

all: clean test

clean:
	make -C src/ clean
