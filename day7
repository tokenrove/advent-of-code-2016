#!/usr/bin/env perl
#
# I was hoping to use perl re's notorious backrefs to cheat and do
# everything in terrible exponential-time regexps, but it was too
# confusing at 2am.

use strict;

sub abba {
  return 0 if (/\[[a-z]*([a-z])((?!\1)[a-z])\2\1[a-z]*\]/);
  for (split(/\[[^\[\]]*\]/)) {
    return 1 if (/([a-z])((?!\1)[a-z])\2\1/)
  }
  return 0
}

sub ababab {
  my $orig = $_;
  for (split(/\[[a-z]*\]/)) {
    while (/(?=([a-z])((?!\1)[a-z])\1)/g) {
        return 1 if $orig =~ /\[[a-z]*$2$1$2[a-z]*\]/
    }
  }
  return 0
}

my $f = \&ababab;
if (grep('--initial', @ARGV)) { shift @ARGV; $f = \&abba }

my $count = 0;
while (<>) { ++$count if &$f($_) }
print $count . "\n"
