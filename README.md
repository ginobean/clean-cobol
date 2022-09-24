# clean-cobol.py

clean-cobol.py (and verify-results.sh, a test driver for clean-cobol.py)

MIT License
Copyright (c) 2022 UChin Kim

[recursive reference to this page](<https://github.com/ginobean/clean-cobol>)

## SUMMARY

*clean-cobol.py* is a filter program that *refactors* sentence structured
COBOL code into code block structured COBOL code, thus improving
maintainability of existing COBOL code going forwards. It
heuristically examines indent levels to figure out where code blocks
start and end, inserting code block terminators (such as ‘end-if’,
‘end-start’, ‘end-search’, etc), as needed. Also, annotates code block
terminators, to make it easier to visually identify the extents of each
code block. Additionally, it can convert code to all lowercase or all
uppercase, as per house coding requirements.

## CAVEATS

Note that your indent levels MUST BE ACCURATE, +/- a fudge factor
of 1 space. This program determines the correct insertion points,
for code block terminators (e.g. 'end-if', 'end-compute', 'end-string', etc.)
heuristically, based on your indent levels. So, if your indent levels are
screwy/wrong, you're likely to get UNUSABLE results.

Also, due to this fudge factor of +/- 1 space, you must use AT LEAST 2
spaces (or more) to signify an indentation change.
e.g. Here is an example of a BAD indent level change. Notice how there
is ONLY a 1 space indentation change from the 'then' line to the
'perform do-something' line.

### Example of a "bad" indent level

-----
    if flag-1 = 'Z'
    then
     perform do-something
     perform something-else
    end-if
-----

Also, if you have 'next sentence' statements in your source code, it's probably
a good idea to manually resolve it, possibly by replacing it with 'continue',
'exit perform cycle', 'exit perform', 'exit paragraph', etc.,
depending on what is contextually correct for the case at hand. COBOL source
code containing 'next sentence' will generally NOT
 work correctly, after being processed by my *clean-cobol.py* script, since
'next sentence' implicitly assumes a paragraph is comprised of multiple
sentences. Whereas, *clean-cobol.py* will convert each paragraph in the
procedure division into a single sentence, comprised of one or more statements
and/or code blocks.

## HOW WELL DOES IT WORK?

I tested my *clean-cobol,py* script on a couple dozen COBOL source code files,
that I found on the Web, plus a number of examples from
*Murach's Structured Cobol* (written by Mike Murach, Anne Prince,
Raul Menendez). In general, it seemed to work pretty well.

As a specific example, for listmods.cbl (from *Murach's Structured Cobol*),
which was about 100K bytes in size, there were two lines containing
the statement 'next sentence', which I manually changed to 'exit paragraph'.
After running my *clean-cobol.py* script on listmods.cbl, the resulting
compiled COBOL program generated exactly the same results as before (except
for the report generation date/time field, which changes each time the
report is regenerated).

But, of course, be sure to do regression testing on the code, both
before and after you run *clean-cobol.py*, to ensure that NO issues were
introduced during the cleaning/filtering process.

## DETAILS

In the 'procedure division' section, converts the COBOL source code to
use the more robust style, which is to use a single solitary '.' at the end
of each paragraph (procedure).

* strips out trailing '.' from every sentence/statement that occurs
    within the procedure division.
* inserts a solitary '.' at the end of each paragraph/procedure,
    if one is not already there.

Heuristically inserts code block terminators ((e.g. end-if, end-call,
end-evaluate, et al.), if needed, by examing the indent levels of each
line in the source code. If a code block terminator is already present,
this script is "smart" enough to figure it out.

If you run this script that already uses code blocks terminators and a
single solitary '.' at the end of each procedure paragraph, you will
likely see few/no changes in the resulting generated output file.

Inserts the following code block terminators, as needed:

* end-if

* end-evaluate
* end-start
* end-search

* end-call
* end-read
* end-write
* end-rewrite

* end-compute
* end-string
* end-unstring

Note that, in general, block terminators will only be added if they
are not already present and the associated start token (e.g. 'call',
'read', 'write', etc) has at least one substatement under it,
as determined by indentation levels.

This means, that if you have a one line 'call' statement, it will
NOT have an 'end-call' added to it.

A notable exception to this are the 'if', 'else if' start tokens,
which will ALWAYS be terminated by an 'end-if', even if they have
NO substatements under it.

Annotates code block terminators (e.g. end-if, 'end-search', 'end-start'),
by appending the text of the correlated 'start' statement as a comment.
e.g.

-----
    main-proc.
        if flag-1 = 'N'
            display 'foobar'
        <more statements go here>
        stop run.

is transformed into:

-----
    main-proc.
        if flag-1 = 'N'
            display 'foobar'
        end-if *> flag-1 = 'N'
        <more statements go here>
        stop run
    . *> end main-proc
-----

* [annotations can be disabled via the --suppress-annotations flag ]
* note that if an existing terminator already has a non-zero length
comment next to it, this program will NOT attempt to add an
annotation to it.

-----

Optionally, transforms code to all uppercase or all lowercase, leaving
quoted strings untouched.

* --lowercase
* --uppercase

Other cosmetic aesthetics:

* --clear-right-margin
  * blanks out right margin (cols >= 73)

* --clear-left-margin
  * blanks out left margin (cols 1 through 6, inclusive)

* --clear-empty-asterisk-comments
  * if line has only a single asterisk, at column 7, will convert it
to a blank line, to help reduce the appearance of 'clutter'.

-----

Code block terminators insertion example:

-----
    if foobar
        if foobar2
            if foobar3
                perform proc-3
            else
                perform proc-4
    perform proc-2

is transformed into

-----
    if foobar
        if foobar2
            if foobar3
                perform proc-3
            else
                perform proc-4
            end-if *> foobar3
        end-if *> foobar2
    end-if *> foobar
    perform proc-2

-----

Note that the program was able to figure out, where to insert the end-if's
based on examining the indentation levels of the original code.

Also note that linear nested if statements are supported.

Terminators insertion example, for linear nested if's:

-----
    if foobar
        dosomething-0
    else if foobar2
        dosomething
    else if foobar3
        dosomething-2
    else if foobar4
        dosomething-3
    perform proc-7

is transformed into:

-----
    if foobar
        dosomething-0
    else if foobar2
        dosomething
    else if foobar3
        dosomething-2
    else if foobar4
        dosomething-3
    end-if *> foobar4
    end-if *> foobar3
    end-if *> foobar2
    end-if *> foobar
    perform proc-7
-----

## IDEAL USE CASE

The ideal use case for this script, is when you are looking to maintain a
COBOL code base which already has a regression test suite associated
with it, to verify continued correct operation of the code.

Once you've verified correctness, via regression testing, the resulting code,
cleaned/filtered by *clean-cobol.py* should be notably easier to maintain
going forwards.

### USAGE EXAMPLES

    # removes trailing dots, from each statement in procedure division.
    # inserts and annotates code block terminators.
    # places 'cleaned' cobol source file into tests/test-set-1-new.cbl
    ./clean-cobol.py tests/test-set-1.cbl

    # removes trailing dots, from each statement in procedure division.
    # inserts and annotates code block terminators.
    # plus, converts everything, except comments and quoted strings, to lowercase.
    # places 'cleaned' cobol source file into tests/test-set-1-new.cbl
    ./clean-cobol.py --lowercase tests/test-set-1.cbl

    # removes trailing dots, from each statement in procedure division.
    # inserts and annotates code block terminators.
    # plus, clears left and right margins, clears empty asterisk comments.
    # places 'cleaned' cobol source file into ./test-set-1-cleaned.cbl
    ./clean-cobol.py --clear-all tests/test-set-1.cbl -o test-set-1-cleaned.cbl

-----

    positional arguments:
    filename              name of the cobol source file

    options:
    -h, --help            show this help message and exit
    -l, --lowercase       convert to all lowercase
    -u, --uppercase       convert to all uppercase.
    -cl, --clear-left-margin
                            blank out left margin (cols 1-6, inclusive)
    -cr, --clear-right-margin
                            blank out right margin (cols >= 73)
    -cs, --clear-empty-asterisk-comments
                            blank out empty asterisk comment lines
    -ca, --clear-all      blank out left and right margins and blank out empty
                            asterisk comment lines
    -A, --suppress-annotations
                            suppress annotation of terminators
    -b, --batch-mode      suppress printing of final line indicating the output file
                            that the changes were written to
    -d, --debug           enable verbose debugging output
    -o OUTPUT_FILENAME, --output-filename OUTPUT_FILENAME
                            output to specified filename

-----

## CONTACT ME / DONATE

[![Donate](<./paypal-donate-button.png>)](<https://paypal.me/uchinkim>)

-----
