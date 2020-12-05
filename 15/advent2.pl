

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

my @dimensions = 'input2.txt'.IO.lines.map({ .split('x') }); 

my $total_paper = [+] @dimensions.map(&get_paper);
my $total_ribbon = [+] @dimensions.map(&get_ribbon);

say $total_paper;
say $total_ribbon;
