design_name = ooocpu
fsdb_name = wave_$(design_name).fsdb
vcs:
	vcs \
	-f flist.f \
	-LDFLAGS -Wl,--no-as-needed \
	-timescale=1ns/1ps \
	-fsdb  -full64  -R  +vc  +v2k \
	-sverilog  -debug_all
sim:
	./simv -debug-region
verdi: 
	verdi  -nologo +v2k  -sverilog  -f flist.f \
	-ssf $(fsdb_name)  &
