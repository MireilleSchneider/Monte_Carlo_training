# -----------------------------
# OPTIONS FOR GFORTRAN COMPLER
# -----------------------------
F90 = gfortran
F90FLAGS = -fPIC -g -cpp -ffixed-line-length-none -ffree-line-length-none \
-Wuninitialized -pedantic -fdefault-real-8 -DGFORTRAN

# -----------------------
# FORTRAN FILES FOR ALPHA
# -----------------------
OBJS_ALPHA = mod_io_management.o mod_constants.o mod_randomsp.o mod_particle.o \
mod_input_scalars.o mod_input_1d.o mod_input_2d.o mod_output_1d.o mod_source.o \
mod_collision.o interp1d.o interp2d.o position_source.o energy_source.o init_particle.o \
input.o output.o fusion_reaction.o ion_trajectory.o source.o \
bfield_components.o orbit_increments.o anotransport.o \
bfield_modulation.o collision.o icrh_heating.o \
new_born.o .dummy_generation.o

#mod_input_scalars.o mod_input_1d.o mod_input_2d.o mod_source.o \
#interp1d.o interp2d.o position_source.o energy_source.o init_particle.o \
#new_born.o input.o output.o fusion_reaction.o ion_trajectory.o source.o \
#bfield_components.o bfield_modulation.o orbit_increments.o anotransport.o \
#.dummy_generation.o

# ------------
# COMPILATION
# ------------
all: alpha.exe

alpha.exe: $(OBJS_ALPHA) alpha.f90
	$(F90) -o alpha.exe alpha.f90 ${F90FLAGS} $(OBJS_ALPHA)

%.o:%.f90
	$(F90) ${F90FLAGS} -c $<

# ---------------
# CLEAN DIRECTORY
# ---------------
clean:
	rm -f *.o .*.o alpha.exe *.mod
