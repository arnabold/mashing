#!/opt/local/bin/perl

#use strict;
#use warnings;

# declarations
sub declarations;
sub check_defined;
sub myname;		# a list operator

&declarations();


# definitions
sub declarations {
	$a;			# $a is declared but undefined
	check_defined($a);
	use warnings;
	$a = "*" . $a . "*";	# warning: uninitialized value as a string ("")
	no warnings;
	check_defined($a);
	undef $a;		# now $a is undefined agin
	use warnings;
	$a = $a + 1;		# warning: uninitialized value as a number (0)
	no warnings;
	check_defined($a);

	# TODO: when used as a reference that isn't being assigned to, it is treated as an error

	my $me;
	$me = myname $0 or die "can't get myname";
}
sub check_defined {
	my ($p) = @_;
	if ($p) {	# if $p is defined
		print "The scalar variable is defined and the value is $p\n"
	} else {
		print "The scalar variable is undefined\n"
		# the scalar variable remains undefined
	}
}

sub myname {
	return "Fabio"
}
