all:	bootmenu.bin

bootmenu.bin:	bootmenu.mac Makefile
	macro11 -o bootmenu.obj -l bootmenu.lst bootmenu.mac
	../tools/obj2bin.pl --raw --outfile bootmenu.bin bootmenu.obj
	../tools/obj2bin.pl --binary --outfile bootmenu.bld bootmenu.obj
	./splitter.py bootmenu.bin bootmenu.evn bootmenu.odd
	od -ov bootmenu.bin > bootmenu.dmp

clean:
	rm -f *.obj *.lst *.bin *.bld *.dmp *.evn *.odd
