#!/usr/bin/perl

use Modern::Perl;
use File::Slurp;

my @digits = split //, read_file('day01.in');
$#digits -= 1; 

sub make_strider {
    my $stride = shift;
    return sub {
        my $i = shift; 
        return ($i + $stride) % @digits;
    }
}


sub calc_sum {
    my $stride = shift;
    my $strider =  make_strider($stride);

    my $sum = 0;
    for my $i (0 .. $#digits){

        my $j = $strider->($i);

        if ($digits[$i] eq $digits[$j]){
            $sum += $digits[$i];
        }
    }

    return $sum;
}

my $sum1 = calc_sum(1);
say $sum1;
my $sum2 = calc_sum(@digits/2);
say $sum2;
  
