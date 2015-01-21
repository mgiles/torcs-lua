ROBOT       = lua_sample
MODULE      = ${ROBOT}.so
MODULEDIR   = drivers/${ROBOT}
SOURCES     = ${ROBOT}.cpp lua_bridge.cpp dispatch.cpp

SHIPDIR     = drivers/${ROBOT}
SHIP        = ${ROBOT}.xml car1-stock1.rgb logo.rgb
SHIPSUBDIRS =

PKGSUBDIRS  = ${SHIPSUBDIRS}
src-robots-base_PKGFILES = $(shell find * -maxdepth 0 -type f -print)
src-robots-base_PKGDIR   = ${PACKAGE}-${VERSION}/$(subst ${TORCS_BASE},,$(shell pwd))

LIBS        = -L/usr/include -llua5.1
COMPILFLAGS = -I/usr/include/lua5.1

include ${MAKE_DEFAULT}
