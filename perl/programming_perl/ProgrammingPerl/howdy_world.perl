#!/opt/local/bin/perl

print "Howdy, world!\n";

my $phrase = "Howdy, world!\n"; # create a scalar variable
print $phrase;                  # print the variable

my $cents;      # declare a scalar variable
my @large;      # declare an array variable
my %interest;   # decalare an hash variable
#&how;    
#my *struck;


# scalar variable
my $answer = 42;
my $pi = 3.14159265;
my $avocados = 6.02e23;
my $pet = "Camel";
my $sign = "I love my $pet";    # double quotes do scalar variable interpolation
my $cost = 'It costs $100';     # single quotes suppress interpolation
my $thence = $whence;
my $salsa = $moles * $avocados; 
my $file = '/tmp';
my $exit = system("ls $file");  
my $cwd = `pwd`;                
