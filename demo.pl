#!/usr/bin/perl

use CGI qw(:standard);
$cgi = new CGI();

$default_events = "DEFAULT EVENTS";
$default_scene= "((bleh (0 10)))";

sub print_event_form {
    print
	h3('Event Forumla'),
	start_form(),
	textarea(-name => 'events',
		 -default => $default_events,
		 -rows=>30,
		 -columns=>100),
	hidden(-name => 'action',
	       -value => 'event_formula'),
	p,
	submit(),
	end_form();
}
sub print_scene_form {
    my $events = shift;
    print 
	h3('Scene descriptions'),
	start_form(),
	textarea(-name => 'scene',
		 -default => $default_scene,
		 -rows => 30,
		 -cols => 100),
	hidden(-name => 'events',
	       -value => $events),
	hidden(-name => 'action',
	       -value => 'scene_desc',
	       -override => 1),
	p,
	submit(),
	end_form();
}

sub print_results {
    my $events = shift;
    my $scene = shift;
    my $fh;
    open($fh, "| ./demo |") or print "Couldn't run demo: $!";
    print $fh $events;
    print $fh $scene;
    {
	$/ = undef;
	print <$fh>;
    }
}

print header;
print start_html(-title => 'FYP Demo');
print start_body;

$action = $cgi->param('action');

print_results('abc','123');

# if ($action eq "event_formula"){
#      print_scene_form($cgi->param('events'));
# }
# elsif ($action eq "scene_desc"){
#     print_results($cgi->param('events'), $cgi->param('scene'));
# }
# else {
#     print_event_form();
# }

print end_body;
print end_html;
