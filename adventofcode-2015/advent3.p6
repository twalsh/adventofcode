
my @input = 'input3.txt'.IO.slurp.comb;

sub visit_map (@orders,%visits) {
    my $x = 0;
    my $y = 0;
    for @orders -> $o {
        given $o {
            when '<' { $x -= 1 }
            when '>' { $x += 1 }
            when '^' { $y -= 1 }
            when 'v' { $y += 1 }
        }
        %visits{"$x.$y"}++;
    }
}
my %visits;
visit_map(@input,%visits);
# 3a answer
say %visits.elems;

my @santa_orders = @input[0,2 ... @input.elems-2];
my @robo_orders  = @input[1,3 ... @input.elems-1];

my %all_visits;
visit_map(@santa_orders,%all_visits);
visit_map(@robo_orders,%all_visits);

# 3b answer
say %all_visits.elems;
