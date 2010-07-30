
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

LIB=$(shell pkg-config --libs --cflags libnotify)

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


