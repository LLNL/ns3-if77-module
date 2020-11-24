
MAIN1  = src/if77ata.for

SRCS1 = src/ata.for \
	src/aalos.for \
	src/amsc.for \
        src/asorp.for

MAIN2  = cpp/if77-example.cc

INCS2  = cpp/if77.h \
	 cpp/if77ata.h

SRCS2  = cpp/if77.cc

EXE1   = $(MAIN1:.for=)
EXE2   = $(MAIN2:.cc=)

OBJS1  = $(SRCS1:.for=.o)
OBJM1  = $(MAIN1:.for=.o)

OBJS2  = $(SRCS2:.cc=.o)
	 
OBJM2  = $(MAIN2:.cc=.o)

F77    = gfortran
CXX    = g++

FFLAGS = --std=legacy -fno-align-commons -g
CXXFLAGS = --std=c++11 -g

FORTLIB = /sw/lib/gcc9/lib
LINKFLAGS = -L$(FORTLIB) -lgfortran

FLIB = libif77.a
CPPLIB = libif77cpp.a

LIBS = $(FLIB) $(CPPLIB) 

all : $(EXE1) $(EXE2) 
.PHONY: all

$(OBJS1) $(OBJM1): %.o : %.for Makefile
	@echo "Compiling $<"
	$(F77) $(FFLAGS) -c $< -o $@

$(EXE1) : $(OBJS1) $(OBJM1) Makefile
	@echo "Linking $@"
	$(F77) $(FFLAGS) $(OBJS1) $(OBJM1) -o $@

$(OBJS2) : $(SRCS2) $(INCS2) 
	@echo "Compiling $<"
	$(CXX) $(CXXFLAGS) -c $< -o $@

$(EXE2) : $(OBJM2) $(CPPLIB) $(FLIB)
	@echo "Linking $@"
	$(CXX) $(CXXFLAGS) $(LINKFLAGS) $^ $* -o $@

clean:
	@echo "Removing object files and executable"
	rm -f $(OBJS1) $(OBJM1) $(OBJS2) $(EXE1) $(EXE2) $(LIBS)
.PHONY: clean

$(FLIB): $(OBJS1)
	ar crv $@ $^

$(CPPLIB): $(OBJS2)
	ar crv $@ $^


# Function to pretty-print a list
empty :=
indent := $(empty)    $(empty)
define print_list
  $(info $(1): $($(1)))
  $(foreach item,$($(1)), $(info $(indent) $(item)))
endef

# print_list = $(shell echo $(1); for f in $($(1)) ; do echo "$(indent) \$f" ; done)

info :
	@echo "MAIN1:     $(MAIN1)"
	@echo "EXE1:      $(EXE1)"
	@echo "SRCS1:     $(SRCS1)"
	@echo "OBJS1:     $(OBJS1)"
	@echo "OBJM1:     $(OBJM1)"
	@echo "F77:       $(F77)"
	@echo "FFLAGS:    $(FFLAGS)"
	@echo ""
	@echo "MAIN2:     $(MAIN2)"
	@echo "EXE2:      $(EXE2)"
	@echo "INCS2:     $(INCS2)"
	@echo "SRCS2:     $(SRCS2)"
	@echo "OBJS2:     $(OBJS2)"
	@echo "CXX:       $(CXX)"
	@echo "CXXFLAGS:  $(CXXFLAGS)"
	@echo ""
	@echo "F77:       $(F77)"
	@echo "FFLAGS:    $(FFLAGS)"
	@echo "FORTLIB:   $(FORTLIB)"
	@echo "CXX:       $(CXX)"
	@echo "CXXFLAGS:  $(CXXFLAGS)"
	@echo "LINKFLAGS: $(LINKFLAGS)"




