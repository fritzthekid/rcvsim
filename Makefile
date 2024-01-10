
.PHONY: subdirs

subdirs:
	mkdir -p _build/log
	mkdir -p _build/obj
	cd csrc; make

