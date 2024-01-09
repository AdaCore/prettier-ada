BUILD_MODE ?= dev
LIBRARY_TYPE ?= relocatable
PROCESSORS ?= 0

ALL_LIBRARY_TYPES = static static-pic relocatable

LIB_PROJECT = prettier_ada.gpr

TEST_PROGRAMS = testsuite/test_programs/test_programs.gpr

.PHONY: lib
lib:
	gprbuild \
		-v \
		-k \
		-XLIBRARY_TYPE=$(LIBRARY_TYPE) \
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
			-XPRETTIER_ADA_LIBRARY_TYPE=$$library_type \
			-XLIBRARY_TYPE=$$library_type \
			-XPRETTIER_ADA_BUILD_MODE=$(BUILD_MODE) \
			--prefix="$(PREFIX)" \
			--install-name=prettier_ada \
			--sources-subdir=include/prettier_ada \
			--build-name=$$library_type \
			--build-var=LIBRARY_TYPE \
			-P $(LIB_PROJECT) -p -f ; \
	done ;

.PHONY: test-programs
test-programs:
	gprbuild \
		-v \
		-k \
		-XLIBRARY_TYPE=static \
		-XPRETTIER_ADA_LIBRARY_TYPE=static \
		-XPRETTIER_ADA_BUILD_MODE=$(BUILD_MODE) \
		-P$(TEST_PROGRAMS) \
		-p \
		-j$(PROCESSORS); \

.PHONY: install-test-programs
install-test-programs:
	gprinstall \
		-XLIBRARY_TYPE=static \
		-XPRETTIER_ADA_LIBRARY_TYPE=static \
		-XPRETTIER_ADA_BUILD_MODE=$(BUILD_MODE) \
		--prefix="$(PREFIX)" \
		--install-name=test_programs \
		--mode=usage \
		-P$(TEST_PROGRAMS) \
		-p \
		-f

.PHONY: test
test: test-programs
	python3 testsuite/testsuite.py

