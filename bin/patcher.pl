#!/usr/bin/perl
# A Perl script that patches a bunch of files
#
#
#
# Copyright (c) 2001 by
#  George C. Necula	necula@cs.berkeley.edu
#  Scott McPeak        smcpeak@cs.berkeley.edu
#  Wes Weimer          weimer@cs.berkeley.edu
#   
# All rights reserved.  Permission to use, copy, modify and distribute
# this software for research purposes only is hereby granted, 
# provided that the following conditions are met: 
# 1. Redistributions of source code must retain the above copyright notice, 
# this list of conditions and the following disclaimer. 
# 2. Redistributions in binary form must reproduce the above copyright notice, 
# this list of conditions and the following disclaimer in the documentation 
# and/or other materials provided with the distribution. 
# 3. The name of the authors may not be used to endorse or promote products 
# derived from  this software without specific prior written permission. 
#
# DISCLAIMER:
# THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR 
# IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES 
# OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
# IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS 
# OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON 
# ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF 
# THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
use strict;
use File::Basename;
use File::Copy;
use FindBin;
use Getopt::Long;           # Command-line option processing
use Data::Dumper;

if($^O eq 'MSWin32') {
    require Win32;
}
# Set filename parsing according to current operating system.
File::Basename::fileparse_set_fstype($^O);

sub printHelp {
    print <<EOL;
Patch include files
Usage: patcher [options] args

options: 
  --help       Prints this help message
  --verbose    Prints a lot of information about what is being done
  --mode=xxx   What tool to emulate: 
                GNUCC   - GNU CC
                MSVC    - MS VC cl compiler

  --dest=xxx   The destination directory. Will make one if it does not exist
  --patch=xxx  Patch file (can be specified multiple times)
  --ppargs=xxx An argument to be passed to the preprocessor (can be specified
               multiple times)

  --ufile=xxx  A user-include file to be patched (treated as \#include "xxx")
  --sfile=xxx  A system-include file to be patched (treated as \#include <xxx>)
 
  --clean       Remove all files in the destination directory
  --dumpversion Print the version name used for the current compiler

 All of the other arguments are passed to the preprocessor.

Send bugs to necula\@cs.berkeley.edu.
EOL
}


my %option;
&Getopt::Long::Configure("pass_through");
&Getopt::Long::GetOptions
    (\%option,
     "--help",            # Display help information
     "--verbose|v",       # Display information about programs invoked
     "--mode=s",          # The mode (GNUCC, MSVC)
     "--dest=s",          # The destination directory
     "--patch=s@",         # Patch files
     "--ufile=s@",         # User include files
     "--sfile=s@",         # System include files
     "--dumpversion",
     "--clean",
     );

if($option{help}) {
    &printHelp();
    exit 0;
}

# print Dumper({"option" => \%option, "ARGV" => \@ARGV});

my $cversion; # Compiler version
my $cname; # Compiler name
my @patches; # A list of the patches to apply

my $ppargs = join(' ', @ARGV);

my %groups;

&findCompilerVersion();

if($option{dumpversion}) {
    print $cversion;
    exit 0;
}

# Find the destination directory
if(!defined($option{dest})) {
    die "Must give a --dest directory\n";
}
if(! -d $option{dest}) {
    die "The destination directory $option{dest} does not exist\n";
}

if($option{clean}) {
    # Find the destination directory for a dummy file
    my $dest = &destinationFileName("");
    chop $dest; # The final /
    print "Cleaning all files in $dest\n";
    (!system("rm -rf $dest")) || die "Cannot remove directory\n";
    exit 0;
}

print "Patching files for $cname version $cversion\n";

# Prepare the patches
if(defined($option{patch})) {
    my $pFile;
    foreach $pFile (@{$option{patch}}) {
        &preparePatchFile($pFile);
    }
}

# print Dumper(\@patches);

my $file;
foreach $file (@{$option{ufile}}) {
    &patchOneFile($file, 0);
}
foreach $file (@{$option{sfile}}) {
    &patchOneFile($file, 1);
} 

# Now check whether we have used all the patches
my $hadError = 0;
foreach my $patch (@patches) {
    # It was optional
    if(defined $patch->{FLAGS}->{optional}) { next; }
    # It was for another system
    if(defined $patch->{FLAGS}->{system} &&
       $patch->{FLAGS}->{system} ne $^O) { next; }
    # Its group was done
    if(defined $patch->{FLAGS}->{group}) {
        if(! defined $groups{$patch->{FLAGS}->{group}}) {
            $hadError = 1;
            print "None of the following patch from group $patch->{FLAGS}->{group} were used:\n";
            foreach my $gp (@patches) {
                if($gp->{FLAGS}->{group} eq $patch->{FLAGS}->{group}) {
                    print "\tfrom $gp->{PATCHFILE} at $gp->{PATCHLINENO}\n";
                }
            }
            $groups{$patch->{FLAGS}->{group}} = 1; # We're done with it
        }
        next;
    }
    # It was not in a group and was not optional
    if(! defined $patch->{USED}) { 
        $hadError = 1;
        print "Non-optional patch was not used:\n\tfrom $patch->{PATCHFILE} at $patch->{PATCHLINENO}\n";
        next;
    }
}
exit $hadError;


############# SUBROUTINES 
sub findCompilerVersion {
    $cname = "";
    $cversion = 0;
    if($option{mode} eq "GNUCC") {
        $cname = "GNU CC";
        open(VER, "gcc -dumpversion $ppargs|") || die "Cannot start $cname";
        while(<VER>) {
            # sm: had to modify this to match "egcs-2.91.66", which is
            # how egcs responds to the -dumpversion request
            if($_ =~ m|^(\d+\S+)|  ||
               $_ =~ m|^(egcs-\d+\S+)|) {
                $cversion = "gcc_$1";
                close(VER) || die "Cannot start $cname\n";
                return;
            }
        }
        die "Cannot find the version for GCC\n";
    }
    if($option{mode} eq "MSVC") {
        $cname = "Microsoft cl";
        $ppargs =~ s|/nologo||g;
        open(VER, "cl $ppargs 2>&1|") || die "Cannot start $cname: cl $ppargs\n";
        while(<VER>) {
            if($_ =~ m|Compiler Version (\S+) |) {
                $cversion = "cl_$1";
                close(VER);
                return;
            }
        }
        die "Cannot find the version for Microsoft CL\n";
    }
    die "You must specify a --mode (either GNUCC or MSVC)";
}

sub lineDirective {
    my ($fileName, $lineno) = @_;
    if($^O eq 'MSWin32') {
        $fileName =~ s|\\|/|g;
    }
    if($option{mode} eq "MSVC") {
        return "#line $lineno \"$fileName\"\n";
    }
    if($option{mode} eq "GNUCC") {
        return "# $lineno \"$fileName\"\n";
    }
}

# Find the absolute name for a file
sub patchOneFile {
    my ($fname, $issys) = @_;
    my $fname1 = $issys ? "<$fname>" : "\"$fname\"";
    print "Patching $fname1\n";
    my $preprocfile = "__topreproc";
    unlink "$preprocfile.i";
    open(TOPREPROC, ">$preprocfile.c") || die "Cannot open preprocessor file";
    print TOPREPROC "#include $fname1\n";
    close(TOPREPROC);
    if($option{mode} eq "GNUCC") {
        (!system("gcc -E $ppargs $preprocfile.c >$preprocfile.i")) 
            || die "Cannot run the GCC preprocessor";
    } elsif($option{mode} eq "MSVC") {
        (!system("cl /nologo /P $ppargs $preprocfile.c"))
            || die "Cannot run the CL preprocessor";
    } else { die "Invalid --mode"; }
    
    # Now scan the resulting file and get the real name of the file
    my $absname = "";
    open(PPOUT, "<$preprocfile.i") || die "Cannot find $preprocfile.i";
    while(<PPOUT>) {
        if($_ =~ m|^\#.+\"(.+$fname)\"|) {
            $absname = $1;
            last;
        }
    }
    close(PPOUT);
    unlink "$preprocfile.c";
    unlink "$preprocfile.i";
    if($absname eq "") {
        die "Cannot find the absolute name of $fname1\n";
    }
    # If we fail then maybe we are using cygwin paths in a Win32 system
    if($option{mode} eq "GNUCC" && $^O eq 'MSWin32') {
        open(WINNAME, "cygpath -w $absname|")
            || die "Cannot run cygpath to convert $absname to a Windows name";
        $absname = <WINNAME>;
        if($absname =~ m|\n$|) {
            chop $absname;
        }
        # print "Converted $fileName to $newName\n";
        close(WINNAME) || die "Cannot run cygpath to convert $absname";
    }
    print "   Absolute name is $absname\n";
    # Decide where to put the result
    my $dest = &destinationFileName($fname);
    print "   Destination is $dest\n";
    &applyPatches($absname, $dest);
}

# Is absolute path name?
sub isAbsolute {
    my($name) = @_;
    if($^O eq "MSWin32") {
        return ($name =~ m%^([a-zA-Z]:)?[/\\]%);
    } else {
        return ($name =~ m%^[/\\]%);
    }
}

# Compute the destination file name and create all necessary directories
sub destinationFileName {
    my ($fname) = @_;
    if(&isAbsolute($fname)) {
        die "Cannot process files that have absolute names\n";
    }
    my $dest = $option{dest} . "/" . $cversion;
    # Break the file name into components
    my @fnamecomp = split(m%[/\\]%, $fname);
    # Add one component at a time
    do {
        if(! -d $dest) {
            (mkdir $dest, 0777) || die "Cannot create directory $dest\n";
        }
        my $comp = shift @fnamecomp;
        $dest .= ('/' . $comp);
    } while($#fnamecomp >= 0);
    return $dest;
}
#####################################################################
# Patching of files
#
sub preparePatchFile {
    my ($pFile) = @_;
    open(PFILE, "<$pFile") ||
        die "Cannot read patch file $pFile\n";
    my $patchLineNo = 0;
    my $patchStartLine = 0;
  NextPattern:
    while(<PFILE>) {
        $patchLineNo ++;
        if($_ !~ m|^<<<(.*)$|) {
            next;
        }
        # Process the flags
        my @patchflags = split(/\s*,\s*/, $1);
        my %valueflags;
        foreach my $flg (@patchflags) {
            $flg = &trimSpaces($flg);
            if($flg =~ m|^(.+)\s*=\s*(.+)$|) {
                $valueflags{$1} = $2;
            } else {
                $valueflags{$flg} = 1;
            }
        }
        # Now we have found the start
        my $pattern = <PFILE>;
        $patchLineNo ++;
        if($pattern eq "") {
            die "A pattern is missing in $pFile";
        }
        my @all_patterns = ();
        my $current_pattern = [$pattern];

        while(<PFILE>) {
            $patchLineNo ++;
            if($_ =~ m|^===|) {
                last;
            }
            if($_ =~ m%^\|\|\|%) {
                # This is an alternate pattern
                push @all_patterns, $current_pattern;
                $current_pattern = [];
                next;
            }
            push @{$current_pattern}, $_;
        }
        # Finish off the last pattern
        push @all_patterns, $current_pattern;
        if($_ !~ m|^===|) {
            die "No separator found after pattern $pattern in $pFile";
        }
        $patchStartLine = $patchLineNo + 1;
        my $replacement = "";
        # If we have more than one non-optional pattern with no group
        # specified, then create a group
        if(@all_patterns > 1 && 
           ! defined $valueflags{group} && 
           ! defined $valueflags{optional}) {
            $valueflags{group} = $pFile . "_$patchStartLine";
        }
        while(<PFILE>) {
            $patchLineNo ++;
            if($_ =~ m|^>>>|) {
                # For each alternate pattern
                my $patt;
                foreach $patt (@all_patterns) {
                    # Maybe the @__pattern__@ string appears in the replacement
                    my $pattern_repl = join('', @{$patt});
                    my $nrlines = int(@{$patt});
                    my $local_repl = $replacement;
                    $local_repl =~ s/\@__pattern__\@/$pattern_repl/g;
                    # Strip the spaces from patterns
                    my @pattern_no_space = ();
                    my $i;
                    foreach $i (@{$patt}) {
                        $i =~ s/\s+//g;
                        push @pattern_no_space, $i;
                    }
                    push @patches, { HEAD => $pattern_no_space[0],
                                     FLAGS => \%valueflags,
                                     NRLINES   => $nrlines,
                                     PATTERNS => \@pattern_no_space,
                                     REPLACE => $local_repl,
                                     PATCHFILE => $pFile,
                                     PATCHLINENO => $patchStartLine,
                                 };
                }
                next NextPattern;
            }
            $replacement .= $_;
        }
        die "Replacement for $pattern not ended in $pFile";
    }
    close(PFILE) ||
        die "Cannot close patch file $pFile\n";
    print "Loaded patches from $pFile\n";
    # print Dumper(\@patches); die "Here\n";
    
}

sub trimSpaces {
    my($str) = @_;
    if($str =~ m|^\s+(\S.*)$|) {
        $str = $1;
    }
    if($str =~ m|^(.*\S)\s+$|) {
        $str = $1;
    }
    return $str;
}


my @includeReadAhead = ();
sub readIncludeLine {
    my($infile) = @_;
    if($#includeReadAhead < 0) {
        my $newLine = <$infile>;
        return $newLine;
    } else {
        return shift @includeReadAhead;
    }
}

sub undoReadIncludeLine {
    my($line) = @_;
    push @includeReadAhead, $line;
}

sub applyPatches {
    my($in, $out) = @_;
    # Initialize all the patches
    my $patch;
    foreach $patch (@patches) {
        $patch->{USE} = 1;
        my $infile = $patch->{FLAGS}->{file};
        if(defined $infile && $in !~ m|$infile$|) {
#            print "Will not use patch ", 
#                  &lineDirective($patch->{PATCHFILE},$patch->{PATCHLINENO});
            $patch->{USE} = 0;
        }
        # Disable the system specific patterns
        if(defined $patch->{FLAGS}->{system} &&
           $patch->{FLAGS}->{system} ne $^O) {
            $patch->{USE} = 0;
        }
        
    }
        
    open(OUT, ">$out") || die "Cannot open patch output file $out";
    open(IN, "<$in") || die "Cannot open patch input file $in";

    @includeReadAhead = ();

    my $lineno = 0;
    my $line; # The current line
    while($line = &readIncludeLine(\*IN)) {
        $lineno ++;
            # Now we have a line to print out. See if it needs patching
        my $patch;
        my @lines = ($line); # A number of lines
        my $nrLines = 1;     # How many lines
        my $toundo  = 0;
      NextPatch:
        foreach $patch (@patches) {
            if(! $patch->{USE}) { next; } # We are not using this patch
            my $line_no_spaces = $line;
            $line_no_spaces =~ s/\s+//g;
            if($line_no_spaces eq $patch->{HEAD}) {
                # Now see if all the lines match
                my $patNrLines = $patch->{NRLINES};
                if($patNrLines > 1) {
                    # Make sure we have enough lines
                    while($nrLines < $patNrLines) {
                        push @lines, &readIncludeLine(\*IN);
                        $nrLines ++;
                        $toundo ++;
                    }
                    my @checkLines = @{$patch->{PATTERNS}};
                    my $i;
                    # print "check: ", join(":", @checkLines);
                    # print "with $nrLines lines: ", join("+", @lines);
                    for($i=0;$i<$patNrLines;$i++) {
                        $line_no_spaces = $lines[$i];
                        $line_no_spaces =~ s/\s+//g;
                        if($checkLines[$i] ne $line_no_spaces) {
                            # print "No match for $patch->{HEAD}\n";
                            next NextPatch;
                        }
                    }
                }
                # print "Using patch from $patch->{PATCHFILE}:$patch->{PATCHLINENO} at $in:$lineno\n";
                # Now replace
                $lineno += ($patNrLines - 1);
                $toundo -= ($patNrLines - 1);
                $line  = &lineDirective($patch->{PATCHFILE},
                                        $patch->{PATCHLINENO});
                $line .= $patch->{REPLACE};
                $line .= &lineDirective($in, $lineno + 1);
                # Mark that we have used this group
                $patch->{USED} = 1;
                if(defined $patch->{FLAGS}->{group}) {
                    $groups{$patch->{FLAGS}->{group}} = 1;
                }
                last;
            }
        }
        print OUT $line;
        # Now undo all but the first line
        my $i;
        for($i=$nrLines - $toundo;$i<$nrLines;$i++) {
            &undoReadIncludeLine($lines[$i]);
        }
    }
    close(IN) || die "Cannot close file $in";
    close(OUT);
    return 1;
}

