#!/usr/local/bin/perl56
use strict;

die("Usage:loadReducedGaussian.pl <input> <number> \n") if $#ARGV < 1;

my $input  = $ARGV[0];
my $number = $ARGV[1];

print "\t  REAL QG$number($number) \n \t  DATA QG$number/";

my @gaussian;
my $count = 0;
open(IN, "<$input");
while (<IN>) {
    if (/(\d+)\,$/) {
        $count++;
        if($count > $number){
            last;
        }
        push @gaussian, $1;
    }
}

close(IN);

my $g = 5;
my @s;
foreach my $n(@gaussian)
{
    if ($g == 5) {
        print "\n","     X";
        $g = 0;
    }
    print " $n,";
    $g++;
}

