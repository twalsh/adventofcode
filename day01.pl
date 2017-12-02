#!/usr/bin/perl
use Modern::Perl;
use File::Slurp;

my @digits = split //, read_file('day01.in');
$#digits -= 1; 

sub calc_sum {
    my $stride = shift;
    my $strider = sub { return ($_[0] + $stride) % @digits };

    my $sum = 0;
    for my $i (0 .. $#digits){

        my $j = $strider->($i);

        if ($digits[$i] eq $digits[$j]){
            $sum += $digits[$i];
        }
    }

    return $sum;
}

say calc_sum(1);
say calc_sum(@digits/2);
