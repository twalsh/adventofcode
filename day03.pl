#!/usr/bin/perl

use Modern::Perl;

sub distance {
    my $addr = shift;

    my $ptr = 1;
    my ($x,$y) = (0,0);
    my $stride = 1;
    my $dir = 1;

    while ($ptr != $addr){
        $x += $stride * $dir;
        $ptr += $stride;
        if ($ptr == $addr){
            last;
        }
        $y += $stride * $dir;
        $ptr += $stride;
        if ($ptr == $addr){
            last;
        }
        $stride++;
        $dir *= -1;
        say "$ptr $stride $dir";
    }
    say "ADDR $addr $x $y";
    return $x+$y;
}

for my $test_address (qw/1 12 23 1024/){
    say "$test_address ",distance($test_address);
}
