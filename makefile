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
   FCFLAGS  = $(LDFLAGS) -traceback -module $(DMOD)
   FORTFLAGS = -c -warn all -check all -traceback -check bounds -debug all -module $(DMOD)
   EXEFLAGS = -fiopenmp -fopenmp-targets=spir64 -g -warn all -check all -traceback -check bounds -debug all -module $(DMOD)
   DFLAGS = -D_real128 -D_OpenMP_TR9
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
   OBJECTS = exe/obj/dmr.o exe/obj/dmr_c_functions.o exe/obj/init_device_pointers.o exe/obj/matmul_device_pointers.o exe/obj/penf_b_size.o exe/obj/penf_global_parameters_variables.o exe/obj/penf.o exe/obj/penf_stringify.o exe/obj/test_dmr.o
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
  RULE = DMR
endif
LIBS    =
VPATH   = $(DSRC) $(DOBJ) $(DMOD)
MKDIRS  = $(DOBJ) $(DMOD) $(DEXE)
LCEXES  = $(shell echo $(EXES) | tr '[:upper:]' '[:lower:]')
EXESPO  = $(addsuffix .o,$(LCEXES))
EXESOBJ = $(addprefix $(DOBJ),$(EXESPO))
MAKELIB = ar -rcs $(DEXE)libdmr.a $(DOBJ)*.o ; ranlib $(DEXE)libdmr.a

#auxiliary variables
COTEXT = "Compile $(<F)"
LITEXT = "Assemble $@"

firstrule: $(RULE)

#building rules
#the library
DMR: $(MKDIRS) $(DOBJ)dmr.o
	@echo $(LITEXT)
	@$(MAKELIB)

#tests
TESTS: $(DEXE)TEST_ALL

$(DEXE)TEST_ALL: $(MKDIRS) $(DOBJ)test_dmr.o
	@rm -f $(filter-out $(DOBJ)test_dmr.o,$(EXESOBJ))
	@echo $(LITEXT)
	#@$(FC) $(FCFLAGS) $(DOBJ)*.o $(LIBS) -o $@
	@$(FC) $(EXEFLAGS) $(DOBJ)*.o $(LIBS) -o $@
EXES := $(EXES) TEST_ALL

#compiling rules
$(DOBJ)dmr_c_functions_c.o: src/lib/dmr_c_functions_c.c
	@echo $(COTEXT)
	@$(CC) $(LDFLAGS) $(DFLAGS)  $< -o $@

$(DOBJ)dmr_c_functions.o: src/lib/dmr_c_functions.F90 \
	$(DOBJ)dmr_c_functions_c.o
	@echo $(COTEXT)
	@$(FC) $(FCFLAGS) $(DFLAGS)  $< -o $@

$(DOBJ)dmr.o: src/lib/dmr.F90 \
	$(DOBJ)dmr_environment.o \
	$(DOBJ)dmr_c_functions.o
	@echo $(COTEXT)
	@$(FC) $(FCFLAGS) $(DFLAGS)  $< -o $@

$(DOBJ)dmr_target_is_present.o: src/lib/dmr_target_is_present.F90 \
	$(DOBJ)dmr_environment.o \
	$(DOBJ)dmr_c_functions.o \
	$(DOBJ)dmr.o
	@echo $(COTEXT)
	@$(FC) $(FCFLAGS) $(DFLAGS)  $< -o $@

$(DOBJ)dmr_target_free.o: src/lib/dmr_target_free.F90 \
	$(DOBJ)dmr_environment.o \
	$(DOBJ)dmr_c_functions.o \
	$(DOBJ)dmr.o
	@echo $(COTEXT)
	@$(FC) $(FCFLAGS) $(DFLAGS)  $< -o $@

$(DOBJ)dmr_target_alloc.o: src/lib/dmr_target_alloc.F90 \
	$(DOBJ)dmr_environment.o \
	$(DOBJ)dmr_c_functions.o \
	$(DOBJ)dmr.o
	@echo $(COTEXT)
	@$(FC) $(FCFLAGS) $(DFLAGS)  $< -o $@

$(DOBJ)dmr_target_memcpy.o: src/lib/dmr_target_memcpy.F90 \
	$(DOBJ)dmr_environment.o \
	$(DOBJ)dmr_c_functions.o \
	$(DOBJ)dmr.o
	@echo $(COTEXT)
	@$(FC) $(FCFLAGS) $(DFLAGS)  $< -o $@

$(DOBJ)dmr_target_memcpy_rect.o: src/lib/dmr_target_memcpy_rect.F90 \
	$(DOBJ)dmr_environment.o \
	$(DOBJ)dmr_c_functions.o \
	$(DOBJ)dmr.o
	@echo $(COTEXT)
	@$(FC) $(FCFLAGS) $(DFLAGS)  $< -o $@

$(DOBJ)dmr_get_mapped_ptr.o: src/lib/dmr_get_mapped_ptr.F90 \
	$(DOBJ)dmr_environment.o \
	$(DOBJ)dmr_c_functions.o \
	$(DOBJ)dmr.o
	@echo $(COTEXT)
	@$(FC) $(FCFLAGS) $(DFLAGS)  $< -o $@

$(DOBJ)dmr_correctly_mapped.o: src/lib/dmr_correctly_mapped.F90 \
	$(DOBJ)dmr_environment.o \
	$(DOBJ)dmr_c_functions.o \
	$(DOBJ)dmr.o
	@echo $(COTEXT)
	@$(FC) $(FCFLAGS) $(DFLAGS)  $< -o $@

$(DOBJ)dmr_target_init.o: src/lib/dmr_target_init.F90 \
	$(DOBJ)dmr_environment.o \
	$(DOBJ)dmr.o
	@echo $(COTEXT)
	@$(FC) $(FCFLAGS) $(DFLAGS)  $< -o $@

$(DOBJ)dmr_environment.o: src/lib/dmr_environment.F90
	@echo $(COTEXT)
	@$(FORT) $(FORTFLAGS) $(DFLAGS)  $< -o $@

$(DOBJ)test_dmr.o: src/tests/test_dmr.F90 \
	$(DOBJ)dmr_environment.o \
	$(DOBJ)dmr.o \
	$(DOBJ)dmr_target_is_present.o \
	$(DOBJ)dmr_target_free.o \
	$(DOBJ)dmr_target_alloc.o \
	$(DOBJ)dmr_target_memcpy.o \
	$(DOBJ)dmr_target_memcpy_rect.o \
	$(DOBJ)dmr_get_mapped_ptr.o \
	$(DOBJ)dmr_correctly_mapped.o \
	$(DOBJ)dmr_target_init.o \
	$(DOBJ)init_device_pointers.o \
	$(DOBJ)matmul_device_pointers.o
	@echo $(COTEXT)
	@$(FC) $(FCFLAGS) $(DFLAGS)  $< -o $@

$(DOBJ)init_device_pointers.o: src/tests/init_device_pointers.F90 \
	$(DOBJ)dmr_environment.o
	@echo $(COTEXT)
	@$(FC) $(FCFLAGS) $(DFLAGS)  $< -o $@

$(DOBJ)matmul_device_pointers.o: src/tests/matmul_device_pointers.F90 \
	$(DOBJ)dmr_environment.o
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
