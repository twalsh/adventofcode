
#(define input
#  (string->list
#   (port->string (open-input-file "input1.txt"))))
my @input = split("",slurp "input1.txt");

my $floor = 0;
my $pos = 0;
my $first_entry;

for @input -> $order {
	if $order eq '(' {
		$floor++;
	} 
	elsif $order eq ')' {
		$floor--;
	}
	if $floor == -1 && $first_entry == 0 {
		$first_entry = $pos;
	}
	$pos++;
}

say $floor,' ',$first_entry;

