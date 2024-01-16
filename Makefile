
.PHONY: subdirs

subdirs:
	mkdir -p _build/log
	mkdir -p _build/obj
	mkdir -p _build/tmp
	cd csrc; make

