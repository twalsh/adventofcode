#!/usr/bin/perl

my @digits = 'day01.in'.IO.slurp.chomp.split('',:skip-empty);

sub calc_sum($stride) {
    my $strider = sub ($i) { return ($i + $stride) % @digits };

    my $sum = 0;
    for 0 .. @digits-1 -> $i {
        my $j = $strider.($i);
        $sum += @digits[$i] if @digits[$i] == @digits[$j];
    }
    return $sum;
}

say calc_sum(1);
say calc_sum(@digits/2);
