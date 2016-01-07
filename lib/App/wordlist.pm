package App::wordlist;

# DATE
# VERSION

use 5.010001;
use strict;
use warnings;

our %SPEC;

sub _list_installed {
    require Module::List;
    my $mods = Module::List::list_modules(
        "WordList::",
        {
            list_modules  => 1,
            list_pod      => 0,
            recurse       => 1,
        });
    [map {s/\AWordList:://; $_} sort keys %$mods];
}

$SPEC{wordlist} = {
    v => 1.1,
    summary => 'Grep words from WordList::*',
    args => {
        arg => {
            schema => ['array*' => of => 'str*'],
            pos => 0,
            greedy => 1,
        },
        ignore_case => {
            schema  => 'bool',
            default => 1,
        },
        len => {
            schema  => 'int*',
        },
        min_len => {
            schema  => 'int*',
        },
        max_len => {
            schema  => 'int*',
        },
        wordlist => {
            schema => ['array*' => of => 'str*'],
            summary => 'Select one or more wordlist modules',
            cmdline_aliases => {w=>{}},
            element_completion => sub {
                require Complete::Util;

                my %args = @_;
                Complete::Util::complete_array_elem(
                    word  => $args{word},
                    array => _list_installed(),
                    ci    => 1,
                );
            },
        },
        or => {
            summary => 'Use OR logic instead of the default AND',
            schema  => 'bool',
        },
        action => {
            schema  => ['str*', in=>[
                'list_cpan', 'list_installed',
                'grep',
            ]],
            default => 'grep',
            cmdline_aliases => {
                l => {
                    summary=>'List installed WordList::* modules',
                    is_flag => 1,
                    code => sub { my $args=shift; $args->{action} = 'list_installed' },
                },
                L => {
                    summary=>'List WordList::* modules on CPAN',
                    is_flag => 1,
                    code => sub { my $args=shift; $args->{action} = 'list_cpan' },
                },
            },
        },
        lcpan => {
            schema => 'bool',
            summary => 'Use local CPAN mirror first when available',
        },
        detail => {
            summary => 'Display more information when listing modules',
            schema  => 'bool',
        },
    },
    examples => [
        {
            argv => [],
            summary => 'By default print all words from all wordlists',
            test => 0,
        },
        {
            argv => [qw/foo bar/],
            summary => 'Print all words matching /foo/ and /bar/',
            test => 0,
        },
        {
            argv => [qw/--or foo bar/],
            summary => 'Print all words matching /foo/ or /bar/',
            test => 0,
        },
        {
            argv => [qw/-w KBBI foo/],
            summary => 'Select a specific wordlist (multiple -w allowed)',
            test => 0,
        },
        {
            argv => [qw|/fof[aeiou]/|],
            summary => 'Filter by regex',
            test => 0,
        },
    ],
    'cmdline.default_format' => 'text-simple',
};
sub wordlist {
    my %args = @_;

    my $action = $args{action} // 'grep';
    my $list_installed = _list_installed();
    my $ci = $args{ignore_case} // 1;
    my $or = $args{or};
    my $arg = $args{arg} // [];

    if ($action eq 'grep') {

        # convert /.../ in arg to regex
        for (@$arg) {
            if (m!\A/(.*)/\z!) {
                $_ = $ci ? qr/$1/i : qr/$1/;
            } else {
                $_ = lc($_) if $ci;
            }
        }

        my @res;
        my $wordlists = $args{wordlist};
        if (!$wordlists || !@$wordlists) {
            $wordlists = $list_installed;
        }
        for my $wl (@$wordlists) {
            my $mod = "WordList::$wl";
            (my $modpm = $mod . ".pm") =~ s!::!/!g;
            require $modpm;
            my $obj = $mod->new;
            $obj->each_word(
                sub {
                    my $word = shift;

                    return if defined($args{len}) &&
                        length($word) != $args{len};
                    return if defined($args{min_len}) &&
                        length($word) < $args{min_len};
                    return if defined($args{max_len}) &&
                        length($word) > $args{max_len};

                    my $cmpword = $ci ? lc($word) : $word;
                    for (@$arg) {
                        my $match =
                            ref($_) eq 'Regexp' ? $cmpword =~ $_ :
                                index($cmpword, $_) >= 0;
                        if ($or) {
                            # succeed early when --or
                            if ($match) {
                                push @res, $word;
                                return;
                            }
                        } else {
                            # fail early when and (the default)
                            if (!$match) {
                                return;
                            }
                        }
                    }
                    if (!$or || !@$arg) {
                        push @res, $word;
                    }
                }
            );
        }
        [200, "OK", \@res];

    } elsif ($action eq 'list_installed') {

        my @res;
        for (@$list_installed) {
            if ($args{detail}) {
                push @res, {
                    name   => $_,
                };
            } else {
                push @res, $_;
            }
        }
        [200, "OK", \@res,
         {('cmdline.default_format' => 'text') x !!$args{detail}}];

    } elsif ($action eq 'list_cpan') {

        my @methods = $args{lcpan} ?
            ('lcpan', 'metacpan') : ('metacpan', 'lcpan');

      METHOD:
        for my $method (@methods) {
            if ($method eq 'lcpan') {
                unless (eval { require App::lcpan::Call; 1 }) {
                    warn "App::lcpan::Call is not installed, skipped listing ".
                        "modules from local CPAN mirror\n";
                    next METHOD;
                }
                my $res = App::lcpan::Call::call_lcpan_script(
                    argv => [qw/mods --namespace WordList/],
                );
                return [200, "OK", [grep {/WordList::/} sort @$res]];
            } elsif ($method eq 'metacpan') {
                unless (eval { require MetaCPAN::Client; 1 }) {
                    warn "MetaCPAN::Client is not installed, skipped listing ".
                        "modules from MetaCPAN\n";
                    next METHOD;
                }
                my $mcpan = MetaCPAN::Client->new;
                my $rs = $mcpan->module({
                        'module.name'=>'WordList::*',
                    });
                my @res;
                while (my $row = $rs->next) {
                    my $mod = $row->module->[0]{name};
                    push @res, $mod unless grep {$mod eq $_} @res;
                }
                return [200, "OK", [sort @res]];
            }
        }
        return [412, "Can't find a way to list CPAN mirrors"];

    } else {

        [400, "Unknown action '$action'"];

    }
}

1;
# ABSTRACT:

=head1 SYNOPSIS

See the included script L<wordlist>.


=head1 SEE ALSO

L<App::GamesWordlist> (L<games-wordlist>) which greps from
C<Games::Word::Wordlist::*> instead.

L<WordList> and C<WordList::*> modules.
