CC?=/opt/TileraMDE-2.1.0.98943/bin/tile-cc

BASE_DIR ?= $(shell pwd)/..
SRC_DIR  ?= $(BASE_DIR)/core

SRC_FILES = $(wildcard $(SRC_DIR)/*.c) \
            $(wildcard $(SRC_DIR)/fact/*.c)
OBJ_FILES = $(SRC_FILES:.c=.o)

APR_LIB_DIR = $(BASE_DIR)/libs/apr
APR_LIB = $(BASE_DIR)/libs/apr/.libs
APR_DISABLED = $(APR_LIB_DIR)/_disabled
APR_DISABLED_LIBS = \
	$(APR_LIB_DIR)/buckets \
	$(APR_LIB_DIR)/dbd \
	$(APR_LIB_DIR)/dbm \
	$(APR_LIB_DIR)/dso \
	$(APR_LIB_DIR)/hooks \
	$(APR_LIB_DIR)/ldap \
	$(APR_LIB_DIR)/memcache \
	$(APR_LIB_DIR)/mmap \
	$(APR_LIB_DIR)/passwd \
	$(APR_LIB_DIR)/shmem \
	$(APR_LIB_DIR)/strmatch \
	$(APR_LIB_DIR)/uri \
	$(APR_LIB_DIR)/xml

INCLUDES += -I$(SRC_DIR) -I$(APR_LIB_DIR)/include

CFLAGS += $(INCLUDES)
LDFLAGS += -lm -L$(APR_LIB) -lapr-2

all: clips

$(APR_DISABLED):
	mkdir $(APR_DISABLED)
	mv $(APR_DISABLED_LIBS) $(APR_DISABLED)

$(APR_LIB): $(APR_DISABLED)	
	cd $(APR_LIB_DIR) && ./buildconf && LDFLAGS="" ./configure --enable-debug --disable-dso  --without-crypto --without-ldap
	$(MAKE) -C $(APR_LIB_DIR)

clips: $(APR_LIB) $(OBJ_FILES)
	$(CC) $(LDFLAGS) $(OBJ_FILES) -o clips

clean:
	rm $(OBJ_FILES) clips

.PHONY: all clean
