@REM Compile launcher for RAM version of UFM
cl65 -t cx16 -C c:/x16/ufm/cfg/cx16fm.cfg -o bin/launch.prg -Wa -Dram=1 launch.s

@REM Compile RAM version of ufm. This will incbin launch.prg
cl65 -t cx16 -C c:/x16/ufm/cfg/cx16fm.cfg -u __EXEHDR__ -o bin/fm.prg -Wa -Duselauncher=1 fm.s 

@REM Compile rom version of ufm
cl65 -t cx16 -C c:/x16/ufm/cfg/cx16rom.cfg -o bin/fmrom.prg fm.s


@REM Copy fmrom.prg into rombank 16 of rom-file
python scripts/copyfmrom.py		x

@REM Run the emulator with the custom rom image and fm.prg loaded into memory
cd..
x16emu -debug -startin c:\x16 -rom romfm.bin -prg ufm/bin/fm.prg 
cd ufm


@REM -hostfsdev 9 -sdcard 2part.img



