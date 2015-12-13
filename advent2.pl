

sub min (@dimensions) {
	return @dimensions.sort({$^a <=> $^b })[0..1];
}

sub get_ribbon (@dimensions) {
	return 2 * ([+] min(@dimensions)) + [*] @dimensions;
}

sub get_paper (@dimensions){
	my ($l,$w,$h) = @dimensions;
	return 2 * ( $l*$w + $w*$h + $h*$l) + [*] min(@dimensions);
}
my $total_paper = 0;
my $total_ribbon = 0;

for 'input2.txt'.IO.lines -> $line {
	my @dimensions = $line.split('x');
	$total_paper += get_paper(@dimensions);
	$total_ribbon += get_ribbon(@dimensions);
}

say $total_paper;
say $total_ribbon;
