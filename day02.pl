#!/usr/bin/perl
use Modern::Perl;
use File::Slurp;
use List::Util qw/min max sum/;

my @rows = read_file('day02.in'); 

my @diffs = map {
    my @cells = split q{ }, $_;
    max(@cells) - min(@cells);
} @rows;

say sum(@diffs);
