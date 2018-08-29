all: ldc

run: ldc
	./ldc

game.c: *.fut lib
	futhark-opencl --library game.fut

lib: futhark.pkg
	futhark-pkg sync

clean:
	rm -rf lib ldc *.c

ldc: game.c ldc.go game.go
	go build
