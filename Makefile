PROJECT	:= UFM
MKDIR	:= mkdir -p
RMDIR	:= rmdir -p
AS = cl65
ASFLAGS += -t cx16
ASFLAGS += --asm-include-dir ../inc --asm-include-dir ../x16-rom/inc

default: all
all: ram rom incbin

incbin: bin/launch.prg
ram: bin/fm.prg
rom: bin/fmrom.prg

# Compile launcher to incbin in ram version of UFM
bin/launch.prg: launch.s | bin map
	$(AS) $(ASFLAGS) -C cfg/cx16fm.cfg -m map/launch.map -Ln map/launch.sym -o bin/launch.prg -Wa -Dram=1 launch.s

#Compile ram-version of UFM, with incbin launcher.prg
bin/fm.prg: fm.s | bin map incbin
	$(AS) $(ASFLAGS) -C cfg/cx16fm.cfg -u __EXEHDR__ -m map/ram.map -Ln map/ram.sym -o bin/fm.prg -Wa -Duselauncher=1 fm.s

#Compile ROM-version as a 16kb file to add to rombank #16/$10
bin/fmrom.prg: fm.s | bin map
	$(AS) $(ASFLAGS) -C cfg/cx16rom.cfg -m map/rom.map -Ln map/rom.sym -o bin/fmrom.prg fm.s

bin:
	$(MKDIR) bin

map:
	$(MKDIR) map

.PHONY: clean
clean:
	$(RM) bin/*.prg map/*.map map/*.sym
