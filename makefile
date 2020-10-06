#!/usr/bin/make
MAKEFLAGS = -j 1

#main building variables
DSRC = src

ifdef gnu
   FC = gfortran
   FORT = $(FC)
   CC  = gcc
   LDFLAGS = -fopenmp -foffload=nvptx-none -c
   ifdef debug
      LDFLAGS = -fopenmp -foffload=nvptx-none -g -Wall -ftracer -c
   endif
   FCFLAGS = $(LDFLAGS) -J$(DMOD)
   FORTFLAGS = -g -c -Wall -J$(DMOD)
   EXEFLAGS = -fopenmp -foffload=nvptx-none -g -Wall -ftracer -J$(DMOD)
endif

ifdef intel
   FC = ifx
   FORT = ifort
   CC = icx
   LDFLAGS = -fiopenmp -fopenmp-targets=spir64 -c
   ifdef debug
      LDFLAGS = -fiopenmp -fopenmp-targets=spir64 -g -c
   endif
   FCFLAGS  = $(LDFLAGS) -warn all -check all -traceback -check bounds -debug all -module $(DMOD)
   FORTFLAGS = -c -warn all -check all -traceback -check bounds -debug all -module $(DMOD)
   EXEFLAGS = -fiopenmp -fopenmp-targets=spir64 -g -warn all -check all -traceback -check bounds -debug all -module $(DMOD)
   DFLAGS = -D_R16P
endif

ifdef ibm
   FC = xlf2008_r
   FORT = xlf2008_r
   CC = xlc
   LDFLAGS = -qsmp=omp -qoffload -c
   ifdef debug
      LDFLAGS = -g -qtbtable=full -qcheck -qsmp=omp -qoffload -c
   endif
   FCFLAGS = $(LDFLAGS) -qsigtrap -qmaxmem=-1 -qmoddir=$(DMOD) -I$(DMOD)
   FORTFLAGS = -g -qtbtable=full -qcheck -qsigtrap -qsmp=omp -qoffload -qmaxmem=-1 -qtgtarch=sm_70 -qcuda -c -qmoddir=$(DMOD) -I$(DMOD)
   EXEFLAGS = -g -qtbtable=full -qcheck -qsigtrap -qsmp=omp -qoffload -qmaxmem=-1 -qtgtarch=sm_70 -qcuda -qmoddir=$(DMOD) -I$(DMOD)
   DFLAGS =
   OBJECTS = exe/obj/falco.o exe/obj/falco_c_functions.o exe/obj/init_device_pointers.o exe/obj/matmul_device_pointers.o exe/obj/penf_b_size.o exe/obj/penf_global_parameters_variables.o exe/obj/penf.o exe/obj/penf_stringify.o exe/obj/test_falco.o
endif

TEST = no
ifeq "$(TEST)" "yes"
  DOBJ = exe/obj/
  DMOD = exe/mod/
  DEXE = exe/
  RULE = TESTS
else
  DOBJ = lib/obj/
  DMOD = lib/mod/
  DEXE = lib/
  RULE = FALCO
endif
LIBS    =
VPATH   = $(DSRC) $(DOBJ) $(DMOD)
MKDIRS  = $(DOBJ) $(DMOD) $(DEXE)
LCEXES  = $(shell echo $(EXES) | tr '[:upper:]' '[:lower:]')
EXESPO  = $(addsuffix .o,$(LCEXES))
EXESOBJ = $(addprefix $(DOBJ),$(EXESPO))
MAKELIB = ar -rcs $(DEXE)libfalco.a $(DOBJ)*.o ; ranlib $(DEXE)libfalco.a

#auxiliary variables
COTEXT = "Compile $(<F)"
LITEXT = "Assemble $@"

firstrule: $(RULE)

#building rules
#the library
FALCO: $(MKDIRS) $(DOBJ)falco.o
	@echo $(LITEXT)
	@$(MAKELIB)

#tests
TESTS: $(DEXE)TEST_ALL

$(DEXE)TEST_ALL: $(MKDIRS) $(DOBJ)test_falco.o
	@rm -f $(filter-out $(DOBJ)test_falco.o,$(EXESOBJ))
	@echo $(LITEXT)
	#@$(FC) $(FCFLAGS) $(DOBJ)*.o $(LIBS) -o $@
	@$(FC) $(EXEFLAGS) $(DOBJ)*.o $(LIBS) -o $@
EXES := $(EXES) TEST_ALL

#compiling rules
$(DOBJ)falco_c_functions.o: src/lib/falco_c_functions_c.c \
	$(DOBJ)penf.o
	@echo $(COTEXT)
	@$(CC) $(LDFLAGS) $(CFLAGS)  $< -o $@

$(DOBJ)falco.o: src/lib/falco.F90 \
	$(DOBJ)penf.o \
	$(DOBJ)falco_c_functions.o
	@echo $(COTEXT)
	@$(FORT) $(FORTFLAGS) $(DFLAGS)  $< -o $@

$(DOBJ)penf.o: src/third_party/PENF/src/lib/penf.F90 \
	$(DOBJ)penf_global_parameters_variables.o \
	$(DOBJ)penf_b_size.o \
	$(DOBJ)penf_stringify.o
	@echo $(COTEXT)
	@$(FORT) $(FORTFLAGS) $(DFLAGS)  $< -o $@

$(DOBJ)penf_stringify.o: src/third_party/PENF/src/lib/penf_stringify.F90 \
	$(DOBJ)penf_b_size.o \
	$(DOBJ)penf_global_parameters_variables.o
	@echo $(COTEXT)
	@$(FORT) $(FORTFLAGS) $(DFLAGS)  $< -o $@

$(DOBJ)penf_b_size.o: src/third_party/PENF/src/lib/penf_b_size.F90 \
	$(DOBJ)penf_global_parameters_variables.o
	@echo $(COTEXT)
	@$(FORT) $(FORTFLAGS) $(DFLAGS)  $< -o $@

$(DOBJ)penf_global_parameters_variables.o: src/third_party/PENF/src/lib/penf_global_parameters_variables.F90
	@echo $(COTEXT)
	@$(FORT) $(FORTFLAGS) $(DFLAGS)  $< -o $@

$(DOBJ)test_falco.o: src/tests/test_falco.F90 \
	$(DOBJ)penf.o \
	$(DOBJ)falco.o \
	$(DOBJ)init_device_pointers.o \
	$(DOBJ)matmul_device_pointers.o
	@echo $(COTEXT)
	@$(FC) $(FCFLAGS) $(DFLAGS)  $< -o $@

$(DOBJ)init_device_pointers.o: src/tests/init_device_pointers.F90 \
	$(DOBJ)penf.o
	@echo $(COTEXT)
	@$(FC) $(FCFLAGS) $(DFLAGS)  $< -o $@

$(DOBJ)matmul_device_pointers.o: src/tests/matmul_device_pointers.F90 \
	$(DOBJ)penf.o
	@echo $(COTEXT)
	@$(FC) $(FCFLAGS) $(DFLAGS)  $< -o $@

#phony auxiliary rules
.PHONY : $(MKDIRS)
$(MKDIRS):
	@mkdir -p $@
.PHONY : cleanobj
cleanobj:
	@echo deleting objects
	@rm -fr $(DOBJ)
.PHONY : cleanmod
cleanmod:
	@echo deleting mods
	@rm -fr $(DMOD)
.PHONY : cleanexe
cleanexe:
	@echo deleting exes
	@rm -rf $(addprefix $(DEXE),$(EXES))
	#@rm -fr $(DEXE)
.PHONY : clean
clean: cleanobj cleanmod
.PHONY : cleanall
cleanall: clean cleanexe
