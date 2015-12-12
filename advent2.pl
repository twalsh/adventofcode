#%#lang racket
#
#(define (get-ribbon dimensions)
#  (+ (* 2 (apply + (take (sort dimensions <) 2)))
#     (apply * dimensions)))
#
sub dim_min (@dimensions){
	return @dimensions.sort[0..1];
}

sub get_ribbon (@dimensions) {
	return 2 * [+] @dimensions.sort[0..1] + [*] @dimensions;
}

sub get_paper (@dimensions){
	my ($l,$w,$h) = @dimensions;
	return 2 * ( $l*$w + $w*$h + $h*$l) + [*] @dimensions.sort[0..1];
}
#(define (get-paper dimensions)
#  (match-let (((list l w h) dimensions))
#    (+ (* 2
#          (+
#           (* l w)
#           (* w h)
#           (* h l)))
#       ; Extra paper
#       (apply * (take (sort dimensions <) 2)))))
#
my $total_paper = 0;
my $total_ribbon = 0;

for 'input2.txt'.IO.lines -> $line {
	my @dimensions = $line.split('x');
	$total_paper += get_paper(@dimensions);
	$total_ribbon += get_ribbon(@dimensions);
}
#(define (read-input in)
#  (let loop ((line (read-line in)) (total-paper 0) (total-ribbon 0))
#    (if (eof-object? line)
#        (cons total-paper total-ribbon)
#        (let ((dimensions (map string->number (string-split line "x"))))
#          (let ((paper (get-paper dimensions))
#                (ribbon (get-ribbon dimensions)))
#            (loop (read-line in) (+ total-paper paper) (+ total-ribbon ribbon))
#            )))))
#
#
#(call-with-input-file "input2.txt" read-input)
#
