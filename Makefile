MAKEFLAGS += -j
rwildcard = $(foreach d,$(wildcard $(1:=/*)),$(call rwildcard,$d,$2) $(filter $(subst *,%,$2),$d))

ASM_PATH := tools/asm
ASM := $(ASM_PATH)/target/release/basm
LD := $(ASM_PATH)/target/release/blink
FIX := $(ASM_PATH)/target/release/bfix

SRCS := $(call rwildcard,src,*.asm)
OBJS := $(SRCS:.asm=.o)
DEPS := $(SRCS:.asm=.d)

LOG_LEVEL := ERROR
ASM_FLAGS := -l $(LOG_LEVEL) -I include
LD_FLAGS := -c link.toml -l $(LOG_LEVEL) -g game.sym --tags game.tags
FIX_FLAGS := -l $(LOG_LEVEL)

game.gbc: $(DEPS) $(OBJS) $(LD) $(FIX)
	$(LD) $(LD_FLAGS) -o $@ $(OBJS)
	$(FIX) $(FIX_FLAGS) -o $@ $@

%.o: %.asm $(ASM)
	$(ASM) $(ASM_FLAGS) -o $@ $<

%.d: %.asm $(ASM)
	$(ASM) $(ASM_FLAGS) -M -o $@ $<

$(ASM):
	cd $(ASM_PATH) && cargo build --release --bin basm

$(LD):
	cd $(ASM_PATH) && cargo build --release --bin blink

$(FIX):
	cd $(ASM_PATH) && cargo build --release --bin bfix

.PHONY: deepclean
deepclean: clean
	cd tools/asm && cargo clean

.PHONY: clean
clean:
	rm -f $(call rwildcard,src,*.o)
	rm -f $(call rwildcard,src,*.d)
	rm -f game.gbc
	rm -f game.sym
	rm -f game.tags

ifneq (,$(filter-out clean deepclean,$(MAKECMDGOALS)))
include $(DEPS)
endif
