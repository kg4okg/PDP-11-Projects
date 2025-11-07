#!/usr/bin/perl -w

# Copyright (c) 2005 Don North
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 
# o Redistributions of source code must retain the above copyright
#   notice, this list of conditions and the following disclaimer.
# 
# o Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the following disclaimer in the
#   documentation and/or other materials provided with the distribution.
# 
# o Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
# TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

require 5.008;

=head1 NAME

obj2hex.pl - Convert a Macro-11 program image to PROM/load format

=head1 SYNOPSIS

obj2hex.pl
S<[--help]>
S<[--debug]>
S<[--verbose]>
S<[--boot]>
S<[--console]>
S<[--binary]>
S<[--ascii]>
S<[--bytes=N]>
S<[--nocrc]>
OBJFILE
>HEXFILE

=head1 DESCRIPTION

Converts a Macro-11 object file to various output formats,
including M9312 boot and console PROM, straight binary records,
and ASCII format for M9312 console load commands.

The object file must be suitable for translation into an
absolute binary program image. No linking or complex object
file translations are allowed. Simple PC-relative and absolute
addressing object file relocation are supported, but not much
else.

=head1 OPTIONS

The following options are available:

=over

=item B<--help>

Output this manpage and exit the program.

=item B<--debug>

Enable debug mode; print input file records as parsed.

=item B<--verbose>

Verbose status; output status messages during processing.

=item B<--boot>

Generate a hex PROM file image suitable for programming into
an M9312 boot prom (512x4 geometry, only low half used).

=item B<--console>

Generate a hex PROM file image suitable for programming into
an M9312 console/diagnostic prom (1024x4 geometry).

=item B<--binary>

Generate binary format load records of the program image (paper
tape format) for loading into SIMH or compatible simulators.
These files can also be copied onto XXDP filesystems to generate
runnable program images (used to write custom diaqnostics).

=item B<--ascii>

Generate a a sequence of 'L addr' / 'D data' commands for downloading
a program via a terminal emulator thru the M9312 user command interface.
Suitable only for really small test programs.

Exactly ONE of B<--boot>, B<--console>, B<--binary>, or B<--ascii>
must be specified.

=item B<--bytes=N>

For hex format output files, output N bytes per line (default 16).

=item B<--nocrc>

For hex format output files, don't automatically stuff the computed
CRC-16 as the last word in the ROM.

=back

=head1 ERRORS

The following diagnostic error messages can be produced on STDERR.
The meaning should be fairly self explanatory.

C<Aborted due to command line errors> -- bad option or missing file(s)

C<Can't open input file '$file'> -- bad filename or unreadable file

C<Error: Improper object file format (1)> -- valid record must start with 0x01

C<Error: Improper object file format (2)> -- second byte must be 0x00

C<Error: Improper object file format (3)> -- third byte is low byte of record length

C<Error: Improper object file format (4)> -- fourth byte is high byte of record length

C<Error: Improper object file format (5)> -- bytes five thru end-1 are data bytes

C<Error: Improper object file format (6)> -- last byte is checksum

C<Error: Bad checksum exp=0x%02X rcv=0x%02X> -- compare rcv'ed checksum vs exp'ed checksum

=head1 EXAMPLES

Some examples of common usage:

  obj2hex.pl --help

  obj2hex.pl --verbose --boot 23-751A9.obj >23-751A9.hex

  obj2hex.pl --verbose --binary memtest.obj >memtest.bin

=head1 AUTHOR

Don North - donorth <ak6dn _at_ mindspring _dot_ com>

=head1 HISTORY

Modification history:

  2005-05-05 v1.0 donorth - Initial version.

=cut

# options
use strict;
	
# external standard modules
use Getopt::Long;
use Pod::Text;
use FindBin;

# external local modules search path
BEGIN { unshift(@INC, $FindBin::Bin);
        unshift(@INC, $ENV{PERL5LIB}) if defined($ENV{PERL5LIB}); # cygwin bugfix
        unshift(@INC, '.'); }

# external local modules

# generic defaults
my $VERSION = 'v1.0'; # version of code
my $HELP = 0; # set to 1 for man page output
my $DEBUG = 0; # set to 1 for debug messages
my $VERBOSE = 0; # set to 1 for verbose messages

# specific defaults
my $crctype = 'CRC-16'; # type of crc calc to do
my $memsize; # number of instruction bytes allowed
my %excaddr; # words to be skipped in rom crc calc
my $romsize; # number of rom addresses
my $romfill; # rom fill pattern
my $romtype = 'NONE'; # default rom type
my $bytesper = -1; # bytes per block in output file
my $nocrc = 0; # output CRC16 as last word unless set

# process command line arguments
my $NOERROR = GetOptions( "help"        => \$HELP,
			  "debug"       => \$DEBUG,
			  "verbose"     => \$VERBOSE,
			  "boot"        => sub { $romtype = 'BOOT'; },
			  "console"     => sub { $romtype = 'DIAG'; },
			  "binary"      => sub { $romtype = 'BINA'; },
			  "ascii"       => sub { $romtype = 'ASC9'; },
			  "bytes=i"     => \$bytesper,
			  "nocrc"       => \$nocrc,
			  );

# init
$VERBOSE = 1 if $DEBUG; # debug implies verbose messages

# say hello
printf STDERR "obj2hex.pl %s by Don North (perl %g)\n", $VERSION, $] if $VERBOSE;

# output the documentation
if ($HELP) {
    # output a man page if we can
    if (ref(Pod::Text->can('new')) eq 'CODE') {
	# try the new way if appears to exist
	my $parser = Pod::Text->new(sentence=>0, width=>78);
	printf STDOUT "\n"; $parser->parse_from_file($0);
    } else {
	# else must use the old way
	printf STDOUT "\n"; Pod::Text::pod2text(-78, $0);
    };
    exit(1);
}

# check for correct arguments present, print usage if errors
unless ($NOERROR && scalar(@ARGV) == 1 && $romtype ne 'NONE') {
    print STDERR "Usage: $0 [options...] arguments\n";
    print STDERR <<"EOF";
       --help                  output manpage and exit
       --debug                 enable debug mode
       --verbose               verbose status reporting
       --boot                  M9312 boot prom
       --console               M9312 console/diagnostic prom
       --binary                binary program load image
       --ascii                 ascii m9312 program load image
       --bytes=N               bytes per block on output
       --nocrc                 inhibit output of CRC-16 in hex format
       OBJFILE                 macro11 object .obj file
     > OUTFILE                 output .hex/.txt/.bin file
EOF
    # exit if errors...
    die "Aborted due to command line errors.\n";
}

#----------------------------------------------------------------------------------------------------

# fill in the parameters of the device
if ($romtype eq 'BOOT') {
    # M9312 512x4 boot prom
    %excaddr = ( 024=>1, 025=>1 ); # bytes to be skipped in rom crc calc
    $memsize = 128; # number of instruction bytes allowed
    $romsize = 512; # number of rom addresses
    $romfill = 0x00; # rom fill pattern
} elsif ($romtype eq 'DIAG') {
    # M9312 1024x4 diagnostic/console prom
    %excaddr = ( ); # bytes to be skipped in rom crc calc
    $memsize = 512; # number of instruction bytes allowed
    $romsize = 1024; # number of rom addresses
    $romfill = 0x00; # rom fill pattern
} elsif ($romtype eq 'BINA' || $romtype eq 'ASC9') {
    # program load image
    %excaddr = ( ); # bytes to be skipped in rom crc calc
    $memsize = 65536; # number of instruction bytes allowed
    $romsize = 65536; # number of rom addresses
    $romfill = 0x00; # rom fill pattern
} else {
    die "ROM type '$romtype' is not supported!\n";
}

if ($VERBOSE) {
    printf STDERR "ROM type is '%s'\n", $romtype;
    printf STDERR "ROM space is %d. bytes\n", $memsize;
    printf STDERR "ROM length is %d. addresses\n", $romsize;
}
    
#----------------------------------------------------------------------------------------------------

# crc computation routine

sub crc (%) {

    # pass all args by name
    my %args = @_;

    # all the crcs we know how to compute
    my %crcdat = ( 'CRC-16' => [ 0xA001,     2, 0x0000,     0x0000     ],
		   'CRC-32' => [ 0xEDB88320, 4, 0xFFFFFFFF, 0xFFFFFFFF ] );

    # run next byte thru crc computation, return updated crc
    return $args{-table}[($args{-crc}^$args{-byte}) & 0xFF]^($args{-crc}>>8) if exists($args{-byte});

    # return initial crc value
    return $crcdat{$args{-name}}->[2] if exists($args{-init});

    # return final crc value xored with xorout
    return $args{-crc} ^ $crcdat{$args{-name}}->[3] if exists($args{-last});

    # compute the crc lookup table, return a pointer to it
    if (exists($args{-new})) {
	my $crctab = [];
	my $poly = $crcdat{$args{-name}}->[0];
	foreach my $byte (0..255) {
	    my $data = $byte;
	    foreach (1..8) { $data = ($data>>1) ^ ($data&1 ? $poly : 0); }
	    $$crctab[$byte] = $data;
	}
	return $crctab;
    }
}

#----------------------------------------------------------------------------------------------------

# compute checksum (twos complement of the sum of bytes)

sub chksum (@) { my $sum = 0; map($sum += $_, @_); (-$sum) & 0xFF; }

#----------------------------------------------------------------------------------------------------

# read a record from the object file

sub read_rec ($) {

    local (*OBJ) = @_;

    my ($buf, $cnt, $len);

    # skip over strings of 0x00; exit OK if hit EOF
    do { return () unless $cnt = read(OBJ, $buf, 1); } while (ord($buf) == 0x00);

    # valid record starts with 0x01
    die "Error: Improper object file format (1)" unless $cnt == 1 && ord($buf) == 0x01;

    # second byte must be 0x00
    $cnt = read(OBJ, $buf, 1);
    die "Error: Improper object file format (2)" unless $cnt == 1 && ord($buf) == 0x00;

    # third byte is low byte of record length
    $cnt = read(OBJ, $buf, 1);
    die "Error: Improper object file format (3)" unless $cnt == 1;
    $len = ord($buf);

    # fourth byte is high byte of record length
    $cnt = read(OBJ, $buf, 1);
    die "Error: Improper object file format (4)" unless $cnt == 1;
    $len += ord($buf)<<8;

    # bytes five thru end-1 are data bytes
    $cnt = read(OBJ, $buf, $len-4);
    die "Error: Improper object file format (5)" unless $cnt == $len-4 && $len >= 4;
    my @dat = unpack("C*", $buf);

    # last byte is checksum
    $cnt = read(OBJ, $buf, 1);
    die "Error: Improper object file format (6)" unless $cnt == 1;
    my $rcv = ord($buf);

    # compare rcv'ed checksum vs exp'ed checksum
    my $exp = &chksum(0x01, $len>>0, $len>>8, @dat);
    die sprintf("Error: Bad checksum exp=0x%02X rcv=0x%02X", $exp, $rcv)
	unless $exp == $rcv;

    # all is well, return the record
    return @dat;
}

#----------------------------------------------------------------------------------------------------

my @mem = (); # real pdp11 memory data words in boot prom
for (my $adr = 0; $adr < $memsize; $adr += 1) { $mem[$adr] = 0; }

my ($minadr,$maxadr,$lasttextaddr) = ('','',0);
my $msk = ($romsize-1)>>1;
    
# open the input .obj file, die if error
open(OBJ, "< ".$ARGV[0]) || die "Can't open input object file '$ARGV[0]'\n";
while (my @rec = &read_rec(*OBJ)) {
    printf STDERR "%s\n", join(" ",map(sprintf("%02X",$_),@rec)) if $DEBUG;
    my $key = $rec[0];
    if ($key == 0x01) { # GSD
	# ignore
    } elsif ($key == 0x02) { # ENDGSD
	# ignore
    } elsif ($key == 0x03) { # TEXT
	# process text record
	my $adr = ($lasttextaddr = (($rec[3]<<8)|($rec[2]<<0))) & $msk;
	my $len = @rec-4;
	$minadr = $adr        if $minadr eq '' || $adr        < $minadr;
	$maxadr = $adr+$len-1 if $maxadr eq '' || $adr+$len-1 > $maxadr;
	for (my $i = 0; $i < $len; $i += 1) {
	    $mem[$adr+$i] = $rec[4+$i];
	    printf STDERR "mem[%04o] = %03o\n", $adr+$i, $mem[$adr+$i] if $DEBUG;
	}
    } elsif ($key == 0x04) { # RLD
	# iterate over RLD subrecords
	for (my $i = 2; $i < @rec; ) {
	    if ($rec[$i] == 0x03) {
		# internal displaced
		my $adr = $lasttextaddr + $rec[$i+1] - 4;
		my $off = ($rec[$i+3]<<8)|($rec[$i+2]<<0);
		my $loc = $off-$adr-2;
		printf STDERR "adr=%o off=%o loc=%o\n", $adr, $off, $loc if $DEBUG;
		$mem[($adr+0)&$msk] = ($loc>>0)&0xFF;
		$mem[($adr+1)&$msk] = ($loc>>8)&0xFF;
		$i += 4;
	    } elsif ($rec[$i] == 0x07) {
		# location counter definition
		my $loc = ($rec[$i+7]<<8)|($rec[$i+6]<<0);
		printf STDERR "loc=%o\n", $loc if $DEBUG;
		$lasttextaddr = $loc;
		$i += 8;
	    } elsif ($rec[$i] == 0x08) {
		# location counter modification
		my $loc = ($rec[$i+3]<<8)|($rec[$i+2]<<0);
		printf STDERR "loc=%o\n", $loc if $DEBUG;
		$lasttextaddr = $loc;
		$i += 4;
	    } else {
		die sprintf("Unknown RLD entry 0x%02X\n", $rec[$i]);
	    }
	}
    } elsif ($key == 0x05) { # ISD
	# ignore
    } elsif ($key == 0x06) { # ENDMOD
	# ignore
    } elsif ($key == 0x07) { # LIBHDR
	# ignore
    } elsif ($key == 0x08) { # LIBEND
	# ignore
    } else                 { # unknown
	die sprintf("Error: unknown record type '0x%02X'\n",$key);
    }
}
close(OBJ);

#----------------------------------------------------------------------------------------------------

my @buf = ($romfill) x $romsize; # physical PROM data bytes, filled background pattern

# only compute CRC on M9312 ROMs
if ($romtype eq 'BOOT' || $romtype eq 'DIAG') {

    # compute CRC-16 of the prom contents (except exception words) and store at last location
    my $crctab = &crc(-name=>$crctype, -new=>1);
    my $crc = &crc(-name=>$crctype, -init=>1);
    for (my $adr = 0; $adr < $memsize-2; $adr += 1) {
	next if exists($excaddr{$adr}); # skip these addresses
	$crc = &crc(-name=>$crctype, -table=>$crctab, -crc=>$crc, -byte=>$mem[$adr]);
    }
    $crc = &crc(-name=>$crctype, -crc=>$crc, -last=>1);
    unless ($nocrc) {
	# output computed CRC-16 as last word in the ROM file
	$mem[$memsize-2] = ($crc>>0)&0xFF;
	$mem[$memsize-1] = ($crc>>8)&0xFF;
    }
    printf STDERR "ROM %s is %06o (0x%04X)\n", $crctype, ($crc) x 2 if $VERBOSE;

    # process data words to actual PROM byte data
    # put 4bit nibble in low 4b of each 8b data byte, zero the upper 4b
    # only copy the above instruction portion over
    for (my $idx = 0; $idx < $memsize<<1; $idx += 4) {
	my $dat = ($mem[($idx>>1)+1]<<8) | ($mem[($idx>>1)+0]<<0);
	$buf[$idx+0] = ($dat&0xE)|(($dat>>8)&0x1);       # bits   3   2   1   8
	$buf[$idx+1] = ($dat>>4)&0xF;                    # bits   7   6   5   4
	$buf[$idx+2] = ((($dat>>8)&0xE)|($dat&0x1))^0xC; # bits ~11 ~10   9   0
	$buf[$idx+3] = (($dat>>12)&0xF)^0x1;             # bits  15  14  13 ~12
    }

} elsif ($romtype eq 'BINA' || $romtype eq 'ASC9') {

    # only copy the above instruction portion over
    for (my $idx = 0; $idx < $memsize; $idx += 1) { $buf[$idx] = $mem[$idx]; }

}

if ($VERBOSE) {

    # print checksum of entire device
    my $chksum = 0; map($chksum += $_, @buf);
    printf STDERR "ROM checksum is %06o (0x%04X)\n", $chksum, $chksum;

}

#----------------------------------------------------------------------------------------------------

if ($romtype eq 'BOOT' || $romtype eq 'DIAG') {
    
    # output the entire PROM buffer as an intel hex file

    $bytesper = 16 if $bytesper <= 0;

    for (my $idx = 0; $idx < $romsize; $idx += $bytesper) {
	my $cnt = $idx+$bytesper <= $romsize ? $bytesper : $romsize-$idx; # N bytes or whatever is left
	my @dat = @buf[$idx..($idx+$cnt-1)]; # get the data
	my $dat = join('', map(sprintf("%02X",$_),@dat)); # map to ascii text
	printf ":%02X%04X%02X%s%02X\n", $cnt, $idx, 0x00, $dat, &chksum($cnt, $idx>>0, $idx>>8, 0x00, @dat);
    }

    printf ":%02X%04X%02X%s%02X\n", 0x00, 0x0000, 0x01, '', &chksum(0x0, 0x0000>>0, 0x0000>>8, 0x01);

} elsif ($romtype eq 'BINA') {

    # Loader format consists of blocks, optionally preceded, separated, and
    # followed by zeroes.  Each block consists of:
    #
    #   001		---
    #   000		 |
    #   lo_count	 |
    #   hi_count	 |
    #   lo_origin	 > count bytes
    #   hi_origin	 |
    #   data byte	 |
    #   :		 |
    #   data byte	---
    #   checksum
    #
    # If the byte count is exactly six, the block is the last on the tape, and
    # there is no checksum.  If the origin is not 000001, then the origin is
    # the PC at which to start the program.

    binmode(STDOUT);

    $bytesper = 128 if $bytesper <= 0;

    my $start = 000001;

    sub m ($) { $_[0] & 0xFF; }

    # output the entire PROM buffer as a binary loader file
    for (my $idx = $minadr; $idx < $maxadr+1; $idx += $bytesper) {
	my $cnt = $idx+$bytesper <= $maxadr+1 ? $bytesper : $maxadr+1-$idx; # N bytes or whatever is left
	my @dat = @buf[$idx..($idx+$cnt-1)]; # get the data
	my $len = $cnt+6;
	my @rec = (0x01, 0x00, &m($len>>0), &m($len>>8), &m($idx>>0), &m($idx>>8), @dat);
	print pack("C*", @rec, &chksum(@rec));
    }
    my @end = (0x01, 0x00, 0x06, 0x00, &m($start>>0), &m($start>>8));
    print pack("C*", @end, &chksum(@end));

} elsif ($romtype eq 'ASC9') {

    # ascii interface to M9312 console emulator

    binmode(STDOUT);

    sub n ($) { $_[0] & 0xFF; }

    # start program load here
    printf "L %o\r\n", $minadr;

    # output the PROM buffer as an ascii load file
    for (my $idx = $minadr; $idx < $maxadr+1; $idx += 2) {
	printf "D %06o\r\n", (&n($buf[$idx+1])<<8) | &n($buf[$idx+0]);
    }

    # start program exec here
    printf "L %o\r\nS\r\n", $minadr;

}

#----------------------------------------------------------------------------------------------------

exit;

# the end
