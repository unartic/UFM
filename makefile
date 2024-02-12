
#Compile launcher to incbin in ram version of UFM
cl65 -t cx16 -C c:/x16/ufm/cfg/cx16fm.cfg -o bin/launch.prg -Wa -Dram=1 launch.s

#Compile ram-version of UFM, whicn incbin launcher.prg
cl65 -t cx16 -C c:/x16/ufm/cfg/cx16fm.cfg -u __EXEHDR__ -o bin/fm.prg -Wa -Duselauncher=1 fm.s 

#Compile ROM-version as a 16kb file to add to rombank #16/$10
cl65 -t cx16 -C c:/x16/ufm/cfg/cx16rom.cfg -o bin/fmrom.prg fm.s


