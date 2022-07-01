ca65 main.s -v -g -l nes_reset_state_detect.lst
ld65 -o nes_reset_state_detect.nes -C config.cfg -v -m nes_reset_state_detect.map -vm --dbgfile nes_reset_state_detect.dbg main.o
