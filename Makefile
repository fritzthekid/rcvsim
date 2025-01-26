.PHONY: subdirs

subdirs:
	mkdir -p _build/log
	mkdir -p _build/obj
	mkdir -p _build/tmp
	mkdir -p _build/libs
	gcc -fPIC --shared -o _build/libs/rvsops.so nifs/rvsops.c
	cd csrc; make

