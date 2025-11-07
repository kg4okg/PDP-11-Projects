AS	:= macro11
DMPOBJ	:= dumpobj
B2L	:= bin2abs

boot.pdp:	boot.bin
	$(B2L) boot.bin 167744 > boot.pdp

boot.bin:	boot.obj
	$(DMPOBJ) boot.obj boot.bin 2>&1 > boot.dmp

boot.obj:	23-765B9.mac
	$(AS) -o boot.obj -l boot.lst 23-765B9.mac

clean:
	rm -f *.bin *.obj *.lst *.dmp *.pdp
