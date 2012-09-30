#!/opt/local/bin/perl

use strict;
use warnings;

# declarations
sub check_defined;
sub myname;		# a list operator

my $a;			# $a is declared but undefined
check_defined($a);

undef $a;		# $a is undef ("" or 0)
check_defined($a);

undef $a;
$a = "*" . $a . "*";	# warning: uninitialized value as string
			# now $a is **
check_defined($a);

undef $a;
$a = $a + 1;		# warning: uninitialized value as number
			# now $a is 1
check_defined($a);

# TODO: when used as a reference that isn't being assigned to, it is treated as an error

my $me;
$me = myname $0 or die "can't get myname";

# definitions
sub check_defined {
	my ($p) = @_;
	if ($p) {	# if $p is defined
		print "The value is defined and the value is $p\n"
	} else {
		print "The value is undefined\n"
	}
}

sub myname {
	return "Fabio"
}
