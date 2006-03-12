#!/usr/bin/perl
# $Id: Makefile.PL 6 2005-06-22 04:31:41Z rcaputo $

use ExtUtils::MakeMaker;

# Touch CHANGES so it exists.
open(CHANGES, ">>CHANGES") and close CHANGES;

WriteMakefile(
	NAME         => 'Filter::Inline',
	AUTHOR       => 'Rocco Caputo <rcaputo@cpan.org>',
	ABSTRACT     => 'Source filter for inline code templating.',
	VERSION_FROM => 'lib/Filter/Inline.pm',

	PREREQ_PM    => {
		'Filter::Util::Call'  => 1.0601,
		'Test::More'          => 0,
	},
	dist         => {
		COMPRESS   => 'gzip -9f',
		SUFFIX     => 'gz',
		PREOP      => (
			'svn-log.perl tee ./$(DISTNAME)-$(VERSION)/CHANGES > ./CHANGES'
		),
	},
);
