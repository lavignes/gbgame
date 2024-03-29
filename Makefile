rwildcard = $(foreach d,$(wildcard $(1:=/*)),$(call rwildcard,$d,$2) $(filter $(subst *,%,$2),$d))

ASM_PATH := tools/asm
ASM := $(ASM_PATH)/target/release/basm
LD := $(ASM_PATH)/target/release/blink

SRCS := $(call rwildcard,src,*.asm)
OBJS := $(SRCS:.asm=.o)

LOG_LEVEL := ERROR
ASM_FLAGS := -l $(LOG_LEVEL) -I include
LD_FLAGS := -c link.toml -l $(LOG_LEVEL) -g game.sym --tags game.tags

all: toolchain game.gbc

toolchain: $(ASM) $(LD)

$(ASM) $(LD):
	cd $(ASM_PATH) && cargo build --release

game.gbc: $(OBJS)
	$(LD) $(LD_FLAGS) -o $@ $^

%.o: %.asm
	$(ASM) $(ASM_FLAGS) -o $@ $<

deepclean: clean
	cd tools/asm && cargo clean

clean:
	rm -f $(call rwildcard,src,*.o)
	rm -f game.gbc
	rm -f game.sym
	rm -f game.tags


