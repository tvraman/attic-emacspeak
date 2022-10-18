#$Id$
#
LABEL=1.1
DIST=software-dtk-$(LABEL)
TCL_VERSION=8.6
CFLAGS+=-O3 -fpic -pedantic -ansi -Wall -I/usr/include/tcl$(TCL_VERSION)
CFLAGS+= -g    -O4 -fPIC  -DPIC  -pedantic -ansi -Wall  -Wno-long-long -std=c++11 -I$(TCL_INCLUDE)
INSTALL=install
LIBS=-ltts -ltts_us -ltcl$(TCL_VERSION)

tcldtk.so: tcldtk.o
	$(CC) $(LDFLAGS) -shared -o $@ $< $(LIBS)

clean:
	rm tcldtk.so tcldtk.o
