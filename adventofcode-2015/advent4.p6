
use Digest::MD5;

my $key = "bgvyzdsv";

sub lpn ($prefix) {
    for (1 ... *) -> $i  { 
        say $i;
        my $string = "$key$i";
        my $hash = Digest::MD5.new.md5_hex($string);
        if substr($hash,0,$prefix.chars) eq $prefix {
            say "$i $hash";
            return;
        }
    }
}

lpn('00000');
lpn('000000');
