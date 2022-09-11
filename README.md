
SUMMARY:

To make it easier to maintain an existing COBOL code base going forwards,
which uses the older style of terminating most statements with a '.',
this script will convert the COBOL source code to use the more robust style,
which is to use a single solitary '.' at the end of each paragraph (procedure),
along with code block terminators. In order to make this feasible, it will
heuristically insert block terminators (e.g. 'end-if', 'end-start',
'end-search', etc) as needed, by examining the indent structure of the code,
to ascertain where the block terminators should be inserted, if they are not
already present.

In addition, this script will also annotate select code block terminators,
such as 'end-if', 'end-search', 'end-start', etc. to make it easier to
visually identify the extents of each code block.

CAVEATS:

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

Bad indent level example:
---
    if flag-1 = 'Z'
    then
     perform do-something
     perform something-else
    end-if
-----

DETAILS:

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
+ end-if

+ end-evaluate
+ end-start
+ end-search

+ end-call
+ end-read
+ end-write
+ end-rewrite

+ end-compute
+ end-string
+ end-unstring

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

Before:
---
    main-proc.
        if flag-1 = 'N'
            display 'foobar'
        <more statements go here>
        stop run.

After:
---
    main-proc.
        if flag-1 = 'N'
            display 'foobar'
        end-if *> flag-1 = 'N'
        <more statements go here>
        stop run
    . *> end main-proc
----------

- [annotations can be disabled via the --suppress-annotations flag ]
- note that if an existing terminator already has a non-zero length
    comment next to it, this program will NOT attempt to add an
    annotation to it.
---

Optionally, transforms code to all uppercase or all lowercase, leaving
quoted strings untouched.
* --lowercase
* --uppercase

Other cosmetic aesthetics:
* --clear-right-margin
    + blanks out right margin (cols >= 73)

* --clear-left-margin
    + blanks out left margin (cols 1 through 6, inclusive)

* --clear-empty-asterisk-comments
    + if line has only a single asterisk, at column 7, will convert it
    to a blank line, to help reduce the appearance of 'clutter'.

---

Code block terminators insertion example:

Before:
---
    if foobar
        if foobar2
            if foobar3
                perform proc-3
            else
                perform proc-4
    perform proc-2


is transformed into

After:
---
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
----
Note that the program was able to figure out, where to insert the end-if's
based on examining the indentation levels of the original code.

Also note that linear nested if statements are supported.

Terminators insertion example, for linear nested if's:

Before:
----
    if foobar
        dosomething-0
    else if foobar2
        dosomething
    else if foobar3
        dosomething-2
    else if foobar4
        dosomething-3
    perform proc-7

After:
----
    is transformed into:
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
---

IDEAL USE CASE:

The ideal use case for this script, is when you are looking to maintain a
COBOL code base which already has a regression test suite associated
with it, to verify continued correct operation of the code.

USAGE EXAMPLES:

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

----

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
