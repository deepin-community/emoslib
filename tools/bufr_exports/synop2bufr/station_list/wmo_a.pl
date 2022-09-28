#!/usr/bin/perl -w
#
# Copyright 1981-2016 ECMWF
#
# Licensed under the GNU Lesser General Public License which
# incorporates the terms and conditions of version 3 of the GNU
# General Public License.
# See LICENSE and gpl-3.0.txt for details.
#
# Needs Perl5, no special modules.
# Script to read the WMO A station lists that's ftp'ed
# from WMO. File is processed, and a formatted output
# file is produced.
# Pressure level field is converted to numeric values
# using the table below, and same thing is done for
# upper air observing practices.
#
# Two arguments: inputfile and outputfile

use strict;

my  %pressure_table = (
		       "" => "0" ,
		       "STATION" => "1", "850 HPA" => "2",
		       "700 HPA" => "3", "1000 HPA" => "4",
		       "1000 GPM" => "5" , "2000 GPM" => "6",
		       "3000 GPM" => "7", "4000 GPM" => "8",
		       "925 HPA"  => "9" );

my %upper_air_table = (
		       "." =>"0","P" =>"1","R" =>"2", "W" =>"3",
		       "RW" =>"4","WR" => "4", "PR" => "5","PW" => "6","WP" =>"7",
		       "" => "8", "X" =>"9" );

# Format WMO A stations file

$#ARGV == 1 || die "Usage : $0 infile outfile.\n";

my $in = $ARGV[0];
my $out = $ARGV[1];


open(IN, $in) || die "ERROR: Can't open input file $in for reading.\n";
open(OUT,">$out" ) || die "ERROR: Can't open output file $out for writing.\n";


my @fields;

my ($skip,,$cid,$stationid,$index,$subindex,$name,$lat,$lon,$hp,$hpflag,$hha,$hhaflag);
my ($pdefid,$so1,$so2,$so3,$so4,$so5,$so6,$so7,$so8,$obshs,$ua1,$ua2,$ua3,$ua4,$comments);
my($block, $station);

# Skip header lines.
$skip = <IN>; $skip = <IN>;

# Header
print OUT "Fields:country blk station lat lon pres_elev ground_elev plevel SO1-8 UA1-4\n";

while ( <IN> )
  {
    # Skip empty lines.
    next if length($_) < 10; 

    ($skip,$skip,$skip,$cid,$skip,$index,$subindex,$name,$lat,$lon,$hp,$hpflag,$hha,$hhaflag,
     $pdefid,$so1,$so2,$so3,$so4,$so5,$so6,$so7,$so8,$obshs,$ua1,$ua2,$ua3,$ua4,$comments)= split(/\t/);
    
    # Check if fixed ship, if so don't print
    if ( $index ne '*****' )
      {
	# Convert/fix lat/lon
	$lat = convertLatLon($lat,'S');
	$lon = convertLatLon($lon,'W');
	
	# Convert pressure level values.
	$pdefid = $pressure_table{$pdefid};
	
	my ($i, $ref);

	# Strip off '#+*' from synoptic values
	for ( $i =1; $i <= 8; $i++ )
	  {
	    eval "\$ref = \\\${so$i} ";
	    $$ref =~ s/[\#\+\*]//;
	    $$ref = "" if $$ref eq '.'; # Blank it if it's just a dot.
	  }
	
	# Strip off '*' from upper air values, convert to table values.
	for (  $i =1; $i <= 4; $i++ )
	  {
	    eval " \$ref = \\\${ua$i}";
	    if ( $$ref =~ /(\S+) /) { $$ref = $1; }
	    $$ref =~ s/\*//;
	    if ( $$ref && exists $upper_air_table{$$ref} ) 
	      {  
		$$ref = $upper_air_table{$$ref}; 
	      }
	    else 
	      {
		$$ref = 0;
	      }
	  }
	# Split index into block and station.
	$block = substr($index,0,2); $station = substr($index,2,3);
	
	
	# Print order for Fortran program.
	printf OUT "%4d %2s %3s %7.2f %7.2f %4s %4s ",
	$cid,$block,$station,$lat,$lon,$hp,$hha;
	# Presssure level and Observing practice.
	printf OUT "%1s %2s %2s %2s %2s %2s %2s %2s %2s %1d %1d %1d %1d ",
	$pdefid,$so1,$so2,$so3,$so4,$so5,$so6,$so7,$so8,$ua1,$ua2,$ua3,$ua4 ;
	# Print name for identification purposes.
	print OUT "$name\n";
      }
    else
      {
	print STDERR "Skipping, fixed ship station\n";
      }
  }
close(IN);
close(OUT);

sub convertLatLon
  {
    my ($val,$what) = @_;

    if  ( ! $val ) 
      {
	print STDERR "ERROR: MISSING VALUE, set to 0 !!";
	$val = "000 00";
      }
    
    my $newVal = $val;
    # Add minus for S/W
    $newVal = "-$val" if ( $val =~ /$what/ );
    
    # Strip off character
    $newVal =~ s/[NSEW]//;
    
    # Convert middle space to dot
    $newVal =~ s/ /./;
    
    # Convert to degrees. A few values has a missing space, and consequently a missing '.'
    # Try to fix this also.
    my $dot = index($newVal,'.');
    if ( $dot < 0 )
      {
	my $firstPart = substr($newVal,0,-2); my $lastPart = substr($newVal,-2);
	$newVal = "${firstPart}.$lastPart";
	$dot = index($newVal,'.');
	print STDERR "Fixed missing space in lat/lon $newVal at line $.\n";	
      }
    
    my $temp = substr($newVal, $dot +1);
    my $ll = int(($temp/60.0+0.005)*100.00);
    $ll = "0$ll" if $ll < 10;

    substr($newVal, $dot+1) = $ll;

    return $newVal;
  }

