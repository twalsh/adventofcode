#!/usr/bin/perl
use Modern::Perl;
use File::Slurp;
use List::Util qw/min max sum/;

my @rows = read_file('day02.in'); 
my @cells = map { [ split q{ }, $_ ] } @rows;

sub checksum {
    my $func = shift;
    my @row_values = map { $func->($_) } @cells;
    sum(@row_values);
}

say checksum(sub { max(@{$_}) - min(@{$_}) }); 

my $find_divisors = sub {
    my @row = @{$_};
    for my $i (0 .. $#row-1){
        my $m = $row[$i];
        for my $j ($i+1 .. $#row){
            my $n = $row[$j];
            if ($m % $n == 0){
                return $m / $n;
            }
            elsif ($n % $m == 0){
                return $n / $m;
            }
        }
    }
};

say checksum($find_divisors); 
