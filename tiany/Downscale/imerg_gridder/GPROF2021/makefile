#************************************************
# makefile for L2AGPROF2021_nngrid
#************************************************

#F90 = ifort -O2 -shared-intel -mcmodel=medium -fpp -D$(MACHINE) \

F90 = gfortran -cpp -O2 -shared-libgcc -fdec-math -fdec-structure -mcmodel=medium -D$(MACHINE) \
      -DLANGUAGE_FORTRAN  -DWRITEPIAINFO \
#     -check all -warn all -warn nounused -ftrapuv

MACHINE = LINUX

LIB1 = -L$(HDF5_LIB)
LIB2 = -L/usr/lib64  -L$(TKIO)/lib
LIBFLAGS = $(LIB1) $(LIB2)

LIB = -ltkc -ltkcselect \
      -ltkchdf5 -ltkchdf5algs \
      -ltkchdf5 -ltkchdf5 -ltkc \
      -ltkc -lm -lhdf5 -lhdf5_hl -ljpeg \
      -lz -lxml2
#	-lmfhdf -ldf

INC1 = -I$(HDF5_INC) 
INC2 = -I$(TKIO)/inc/fortcode -I/$(TKIO)/inc/ccode
INCFLAGS = $(INC1) $(INC2)

FLAGS= $(INCFLAGS) $(LIBFLAGS) 

OUT= L2AGPROF2021_nngrid 

#************************************************

SOURCE=	L2AGPROF2021_nngrid.f90 \
	abterm_e.f90 \
	addmscan.f90 \
	char2int.f90 \
	char2real.f90 \
	cgprfile.f90 \
	getfst3.f90  \
	getmscan.f90 \
	getgprofg.f90  \
	getwindow.f90 \
	ogprfile.f90 \
	parsekp.f90 \
	readargs.f90 \
	t2sec2k.f90 \
	zones2globe.f90 \
	gprof2021out.f90   \
	fov2gc.f90    \
	fov2gns.f90    \
	fov2gc_timeonly.f90 \
	fov2gns_timeonly.f90 \
	fov_dim.f90  \
	fov_orc.f90   \
	fov_orns.f90   \
	mk_oname.f90  \
	write_emptygrid.f90
#test
#	fov_or.f90   \
#	dmp.f90
#test

#************************************************

L2AGPROF2021_nngrid:  makefile
	$(F90) -o $(OUT) $(FLAGS) $(SOURCE) $(LIB)
	rm -f *.o
	rm -f *__*

#************************************************

clean:	
	rm -f *.o
	rm -f $(OUT)
	rm -f *__*

#************************************************
