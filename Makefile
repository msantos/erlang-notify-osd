
ERL=erl
APP=notify

CC=gcc

# Use "-m64" for 64-bit Erlang installs
ARCH=-m32

# Mac OS X
#FLAGS=$(ARCH) -O3 -fPIC -bundle -flat_namespace -undefined suppress -fno-common

# Linux
FLAGS=-fPIC -shared

ERL_ROOT=/usr/local/lib/erlang
CFLAGS=-g -Wall

LIB=-pthread -D_REENTRANT -I/usr/include/gtk-2.0 -I/usr/lib/gtk-2.0/include -I/usr/include/atk-1.0 -I/usr/include/cairo -I/usr/include/pango-1.0 -I/usr/include/gio-unix-2.0/ -I/usr/include/glib-2.0 -I/usr/lib/glib-2.0/include -I/usr/include/pixman-1 -I/usr/include/freetype2 -I/usr/include/directfb -I/usr/include/libpng12 -I/usr/include/dbus-1.0 -I/usr/lib/dbus-1.0/include -L/lib -lgtk-x11-2.0 -lgdk-x11-2.0 -latk-1.0 -lgio-2.0 -lpangoft2-1.0 -lgdk_pixbuf-2.0 -lm -lpangocairo-1.0 -lcairo -lpango-1.0 -lfreetype -lfontconfig -lgmodule-2.0 -ldbus-glib-1 -ldbus-1 -lpthread -lgobject-2.0 -lgthread-2.0 -lrt -lglib-2.0 -lnotify

all: dir erl nif

dir:
	-@mkdir -p ebin

erl:
	@$(ERL) -noinput +B \
		-eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

nif:
	(cd c_src && \
	$(CC) $(ARCH) -g -Wall $(LIB) $(FLAGS) -o ../priv/notify.so  \
		notify.c -I $(ERL_ROOT)/usr/include/ )


clean:  
	@rm -fv ebin/*.beam priv/$(APP) priv/$(APP).so c_src/*.a c_src/*.o


