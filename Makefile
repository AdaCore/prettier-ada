BUILD_MODE ?= dev
LIBRARY_TYPE ?= relocatable
PROCESSORS ?= 0

ALL_LIBRARY_TYPES = static static-pic relocatable

LIB_PROJECT = gnat/prettier_ada.gpr

TEST_DRIVER = testsuite/drivers/document_formatter/gnat/document_formatter.gpr

.PHONY: lib
lib:
	gprbuild \
		-v \
		-k \
		-XPRETTIER_ADA_LIBRARY_TYPE=$(LIBRARY_TYPE) \
		-XPRETTIER_ADA_BUILD_MODE=$(BUILD_MODE) \
		-P $(LIB_PROJECT) \
		-p \
		-j$(PROCESSORS) ; \

.PHONY: all
all: all-libs

.PHONY: all-libs
all-libs:
	for library_type in $(ALL_LIBRARY_TYPES) ; do \
		gprbuild \
			-v \
			-k \
			-XPRETTIER_ADA_LIBRARY_TYPE=$$library_type \
			-XLIBRARY_TYPE=$$library_type \
			-XPRETTIER_ADA_BUILD_MODE=$(BUILD_MODE) \
			-P $(LIB_PROJECT) \
			-p \
			-j$(PROCESSORS) ; \
	done;

.PHONY: clean
clean:
	rm -rf lib;
	rm -rf obj;

.PHONY: install
install: install-lib

.PHONY: install-lib
install-lib:
	for library_type in $(ALL_LIBRARY_TYPES) ; do \
		gprinstall \
			-XPRETTIER_LIBRARY_LIBRARY_TYPE=$$library_type \
			-XPRETTIER_LIBRARY_BUILD_MODE=$(BUILD_MODE) \
			--prefix="$(PREFIX)" \
			--sources-subdir=include/ada_prettier \
			--build-name=$$library_type \
			--build-var=LIBRARY_TYPE \
			-P $(LIB_PROJECT) -p -f ; \
	done ;

.PHONY: test-driver
test-driver:
	gprbuild \
		-v \
		-k \
		-XPRETTIER_LIBRARY_LIBRARY_TYPE=$(LIBRARY_TYPE) \
		-XPRETTIER_LIBRARY_BUILD_MODE=$(BUILD_MODE) \
		-P$(TEST_DRIVER) \
		-p \
		-j$(PROCESSORS);

.PHONY: install-test-driver
install-test-driver:
	gprinstall \
		-XPRETTIER_LIBRARY_LIBRARY_TYPE=$(LIBRARY_TYPE) \
		-XPRETTIER_LIBRARY_BUILD_MODE=$(BUILD_MODE) \
		--prefix="$(PREFIX)" \
		--mode=usage \
		-P$(TEST_DRIVER) \
		-p \
		-f

.PHONY: test
test: test-driver
	python3 testsuite/testsuite.py

