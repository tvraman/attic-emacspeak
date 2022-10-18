#$Id$
#
LABEL=1.1
TCL_VERSION=8.6
CFLAGS+=-O3 -fpic -pedantic -ansi -Wall -I/usr/include/tcl$(TCL_VERSION)
CFLAGS+= -g    -O4 -fPIC  -DPIC  -pedantic -ansi -Wall  -Wno-long-long -I$(TCL_INCLUDE)
LIBS=-ltts -ltts_us -ltcl$(TCL_VERSION) 

tcldtk.so: tcldtk.o
	$(CC) $(LDFLAGS) -shared -o $@ $< $(LIBS)

clean:
	rm tcldtk.so tcldtk.o
