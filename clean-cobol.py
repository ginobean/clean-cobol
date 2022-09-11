#!/usr/bin/env python3
#
# MIT License
#
# Copyright (c) 2022 UChin Kim
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#
#

import sys
import argparse
import argcomplete
import re
import os.path
import pathlib

USAGE_DOCS = """

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
---bad indent level example---
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

    - strips out trailing '.' from every sentence/statement that occurs
        within the procedure division.
    - inserts a solitary '.' at the end of each paragraph/procedure,
        if one is not already there.

Heuristically inserts code block terminators ((e.g. end-if, end-call,
end-evaluate, et al.), if needed, by examing the indent levels of each
line in the source code. If a code block terminator is already present,
this script is "smart" enough to figure it out.

If you run this script that already uses code blocks terminators and a
single solitary '.' at the end of each procedure paragraph, you will
likely see few/no changes in the resulting generated output file.

    - inserts the following code block terminators, as needed:
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
---before--
main-proc.
    if flag-1 = 'N'
        display 'foobar'
    <more statements go here>
    stop run.
---after--
main-proc.
    if flag-1 = 'N'
        display 'foobar'
    end-if *> flag-1 = 'N'
    <more statements go here>
    stop run
. *> end main-proc
-----------
- [annotations can be disabled via the --suppress-annotations flag ]
- note that if an existing terminator already has a non-zero length
    comment next to it, this program will NOT attempt to add an
    annotation to it.
---

Optionally, transforms code to all uppercase or all lowercase, leaving
quoted strings untouched.
    --lowercase
    --uppercase

Other cosmetic aesthetics:
    --clear-right-margin
        blanks out right margin (cols >= 73)

    --clear-left-margin
        blanks out left margin (cols 1 through 6, inclusive)

    --clear-empty-asterisk-comments
        if line has only a single asterisk, at column 7, will convert it
        to a blank line, to help reduce the appearance of 'clutter'.

---

Code block terminators insertion example:
---
if foobar
    if foobar2
        if foobar3
            perform proc-3
        else
            perform proc-4
perform proc-2
---
is transformed into:
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
"""

fudge_factor = 1
current_indent = 0
tokens = []


paragraph_tokens = []
paragraph_statement_count = {}
end_paragraph_token = '.'

area_a_num_spaces = 1
area_b_num_spaces = 5
implicit_first_paragraph_name = '<main>'
end_paragraph_or_section_annotation = 'end'

# assume that paragraph, section names, and 'end program' statements
# all start at either column 8 or column 9 (1-based).
# left_column holds columns 1-6, inclusive. So we can expect
# 1 or 2 spaces here, meaning the first token starts at either
# column 8 or column 9.
section_tokenizer_re = r'^\s{1,2}([^.\s]+)\s*([^.\s]+)?\s*(.*)\.\s*$'

input_filename = ''
use_lowercase = False
use_uppercase = False
clear_left_margin = False
clear_right_margin = False
add_annotations = True
debug = False
output_filename = ''
output_file = None

procedure_division_line_num = -1
current_line_number = 0
mismatched_terminator_found = False


# must be specified in all lowercase!
IndentTokens = {
    'if': 'end-if',
    'else if': 'end-if',
    'evaluate': 'end-evaluate',
    'start': 'end-start',
    'search': 'end-search',

    'call': 'end-call',
    'read': 'end-read',
    'write': 'end-write',
    'rewrite': 'end-rewrite',

    'compute': 'end-compute',
    'string': 'end-string',
    'unstring': 'end-unstring',
}

IndentTerminators = set(IndentTokens.values())

AnnotateTokens = set(('if', 'else if', 'search', 'start'))

# always emit corresponding terminator tokens irrespective of
# substatement counts..
AlwaysEmitTerminator = set(('if', 'else if'))

# must be specified in all lowercase!
# 'else', 'else if', 'then' are outdented indent continuations.
# they do NOT signify
# an end to the current indentation level.
OutdentedContinuations = ('else', 'else if', 'then',
                          'until', 'with', 'varying',
                          'or', 'not', 'and',
                          '<', '<=', '=', '>=', '>')


# These are, potentially, single word 'sentences' that
# DO NOT denote the start of a new paragraph or paragraph section.
NonParagraphTokens = set(('goback', 'exit'))

comments_queue = []


generated_file_marker = '-new'


def perror(*objs):
    """
    prints to stderr
    """
    print(*objs, file=sys.stderr)


def fatal(*objs):
    perror(*objs)
    perror("(cannot safely continue..)")
    sys.exit(1)


def split_into_basename_and_ext(filename):

    m = re.search(r'^(.+)[.]([^.]+)$', filename)
    if m is not None:
        return (m.group(1), m.group(2))

    return (filename, '')


def split_at_comment(line):
    for i in range(1, len(line) - 1):
        if line[i] == '*' and line[i + 1] == '>':
            return (line[:i], line[i + 2:])

    return (line, '')


def pop_trailing_whitespace(line_chars):
    while len(line_chars) > 1 and line_chars[-1] == ' ':
        line_chars.pop()


def clear_out_margins(line, clear_left_margin, clear_right_margin):

    left_column = ''
    right_column = ''

    if not clear_left_margin:
        left_column = line[:6]

    if not clear_right_margin:
        right_column = line[72:]

    line = line[6:72]

    return (line, left_column, right_column)


def clear_empty_comment(line_chars):
    if len(line_chars) >= 1 and line_chars[0] == '*':
        for i in range(len(line_chars)):
            if i == 0:
                continue

            if line_chars[i] != ' ':
                return

        line_chars[0] = ' '


def push_masked_chars(line_chars, masked_chars,
                      start_quote_idx, end_quote_idx, dummy_char='@'):
    end_quote_idx = min(end_quote_idx, len(line_chars))
    for i in range(start_quote_idx, end_quote_idx):
        masked_chars[i] = line_chars[i]
        line_chars[i] = dummy_char


def mask_quoted(line_chars, masked_chars):
    start_quote_idx = -1
    i = 0

    while i < len(line_chars):
        if start_quote_idx == -1:
            # look for a start quote char
            if line_chars[i] == '"' or line_chars[i] == "'":
                start_quote_char = line_chars[i]
                start_quote_idx = i

        # we're inside a quote. look for matching quote char
        elif line_chars[i] == start_quote_char:
            if i + 1 < len(line_chars) \
               and line_chars[i + 1] == start_quote_char:
                # doubled quote char encountered
                i += 2
                continue

            push_masked_chars(line_chars, masked_chars, start_quote_idx, i + 1)
            start_quote_idx = -1

        i += 1

    if start_quote_idx > -1:
        push_masked_chars(line_chars, masked_chars,
                          start_quote_idx, len(line_chars))
        start_quote_idx = -1


def unmask_masked(line_chars, masked_chars):
    for i in range(len(masked_chars)):
        if masked_chars[i] != '\x00':
            line_chars[i] = masked_chars[i]


def unmask_masked_substr(line, masked_chars, sp):
    result = []
    for i in range(sp[0], sp[1]):
        if masked_chars[i] != '\x00':
            result.append(masked_chars[i])
        else:
            if use_uppercase:
                result.append(line[i].upper())
            elif use_lowercase:
                result.append(line[i].casefold())
            else:
                result.append(line[i])

    return "".join(result).strip()


def lowercase_line(line_chars):
    for i in range(len(line_chars)):
        line_chars[i] = line_chars[i].casefold()


def uppercase_line(line_chars):
    for i in range(len(line_chars)):
        line_chars[i] = line_chars[i].upper()


def is_asterisk_comment_line(line):
    return len(line) >= 7 and line[6] == '*'


def is_empty_asterisk_comment_line(line):
    m = re.search(r'^\s{6}\*\s*$', line)
    return m is not None


def is_continuation_line(line_chars):
    return len(line_chars) >= 1 and line_chars[0] == '-'


def same_indent(v1, v2):
    return abs(v1 - v2) <= fudge_factor


def greater_indent(v1, v2):
    return (v1 - v2 > fudge_factor)


def lesser_indent(v1, v2):
    return (v2 - v1 > fudge_factor)


def print_output(line, left_column, right_column):
    right_column = right_column.rstrip()
    middle_padding = max(66 - len(line), 0)
    left_padding = max(6 - len(left_column), 0)

    output_line = []

    output_line.append(left_column)
    output_line.append(' ' * left_padding)

    if len(right_column) == 0:
        output_line.append(line[:74])
    else:
        output_line.append(line[:66])

    output_line.append(' ' * middle_padding)
    output_line.append(right_column)
    print("".join(output_line).rstrip(), file=output_file)


def print_output_verbatim(line):
    print(line, file=output_file)


def print_multiline_comment(comment):
    lines = []
    stride = 62
    prefix = "**> "

    for i in range(0, len(comment), stride):
        lines.append(prefix + comment[i:i + stride])

    for line in lines:
        print_output(line, '', '')


def flush_line(line_chars, masked_chars, line_comments, left_column,
               right_column):
    flush_queued_comments()

    unmask_masked(line_chars, masked_chars)
    pop_trailing_whitespace(line_chars)
    if len(line_comments) > 0:
        if len(line_chars) == 0 or line_chars[-1] != ' ':
            line_chars.append(' ')

        line = "*>".join(["".join(line_chars), line_comments])
    else:
        line = "".join(line_chars)

    print_output(line, left_column, right_column)


def flush_dot_line(line):
    print_output(line, '', '')
    flush_queued_comments()


def queue_comments(line_chars, masked_chars, line_comments, left_column,
                   right_column):
    unmask_masked(line_chars, masked_chars)

    if len(line_comments) > 0:
        comments_queue.append((("*>"
                                .join(["".join(line_chars), line_comments])),
                               left_column,
                               right_column))
    else:
        comments_queue.append((("".join(line_chars)),
                               left_column,
                               right_column))


def queue_asterisk_comments(line):
    comments_queue.append((line, ))


def flush_queued_comments():
    if len(comments_queue) == 0:
        return

    for item in comments_queue:
        if len(item) == 1:
            print_output_verbatim(item[0])
        else:
            (line, left_column, right_column) = item
            print_output(line, left_column, right_column)

    comments_queue.clear()


def is_end_program(token1, token2):
    return (token1 == 'end' and token2 == 'program')


def is_paragraph_or_section(token1, token2):
    is_para_name = token2 == '' \
        and token1 not in NonParagraphTokens
    is_section_name = token2 == 'section'

    return is_para_name or is_section_name


# remove all trailing dots, except for single word units (considering a word to
# be a token that is delimited by one or more spaces) that have a trailing dot
# should also be ignored. This will effectively skip dots that are on a line
# by themselves and procedure names that have a trailing dot.
def remove_trailing_dot(line_chars):

    if is_asterisk_comment_line(line_chars):
        return

    line = "".join(line_chars).casefold()
    m = re.search(section_tokenizer_re, line)

    if m is not None:
        token1 = m.group(1)
        token2 = m.group(2)
        if token2 is None:
            token2 = ""

        end_of_program = is_end_program(token1, token2)
        if is_paragraph_or_section(token1, token2) \
                or end_of_program:
            return

    m = re.search(r'^( {1,})\.\s*$', line)
    # single solitary dot
    if m is not None:
        return

    for i in reversed(range(len(line_chars))):
        if line_chars[i] == ' ':
            continue

        if line_chars[i] == '.':
            line_chars[i] = ' '
            return

        # the first non-space char was NOT a '.'. we're done here.
        return


def insert_terminator_into_output(indents, terminator, description):
    if use_uppercase:
        terminator = terminator.upper()
    elif use_lowercase:
        terminator = terminator.casefold()

    line = "{}{}".format(" " * indents, terminator)
    if len(description) > 0:
        line += " *> {}".format(description)

    print_output(line, '', '')


def process_terminator_insertions(current_token, non_matching_only=False):
    # outdent occurred, that went left of the last known if-indent level.
    # we need to close off the if-indent levels that it just went past.
    while (len(tokens) > 0) \
            and lesser_indent(current_indent, tokens[-1]['indent']):
        token = tokens[-1]['token']
        if non_matching_only:
            if IndentTokens[token] == current_token:
                return

        if tokens[-1]['sub-count'] > 0 \
                or tokens[-1]['token'] in AlwaysEmitTerminator:
            insert_terminator_into_output(tokens[-1]['indent'],
                                          IndentTokens[token],
                                          tokens[-1]['description'])
        tokens.pop()

        if debug:
            print("popped token line-no = {}".format(current_line_number),
                  file=sys.stderr)

    # this is useful when terminating a linear nest of if statements..
    if current_token not in OutdentedContinuations:
        while (len(tokens) > 0) \
                and same_indent(current_indent, tokens[-1]['indent']):
            token = tokens[-1]['token']

            if non_matching_only:
                if IndentTokens[token] == current_token:
                    return

            if tokens[-1]['sub-count'] > 0 \
                    or tokens[-1]['token'] in AlwaysEmitTerminator:
                insert_terminator_into_output(tokens[-1]['indent'],
                                              IndentTokens[token],
                                              tokens[-1]['description'])
            tokens.pop()

            if debug:
                print("#2 popped token line-no = {}"
                      .format(current_line_number),
                      file=sys.stderr)


def process_keyword_terminators(line, masked_chars=None):
    global current_indent
    global mismatched_terminator_found

    if debug:
        print("-" * 40, file=sys.stderr)
        print("process_keyword_terminators=[{}]".format(line),
              file=sys.stderr)

    description = ''
    m = re.search(r'^(\s*)(else if|[^\s]+)(.*)$', line, re.IGNORECASE)

    if m is not None:
        current_indent = len(m.group(1))
        keyword = m.group(2).casefold()

        if keyword != '.':
            if len(tokens) > 0:
                if greater_indent(current_indent, tokens[-1]['indent']):
                    tokens[-1]['sub-count'] += 1

        if add_annotations:
            if masked_chars is not None and keyword in AnnotateTokens:
                description = unmask_masked_substr(line,
                                                   masked_chars, m.span(3))
                description = re.sub(r'\s+', " ", description)
        if debug:
            print("token(1) = {} current_indent={}"
                  .format(keyword, current_indent), file=sys.stderr)
    else:
        m = re.search(r'^(\s*)', line)
        current_indent = len(m.group(1))
        keyword = ''
        description = ''
        if debug:
            print("'empty line' token = {} current_indent={}"
                  .format(keyword, current_indent), file=sys.stderr)
        return ''

    # if we find an extant end terminator, try to match it with what we've
    # seen so far..
    if keyword in IndentTerminators:
        description = '*** WAS UNABLE TO MATCH TERMINATOR! ***'
        matched_terminator = False
        while len(tokens) > 0:
            token = tokens[-1]['token']
            if IndentTokens[token] != keyword:
                process_terminator_insertions(keyword, True)
                continue

            if add_annotations:
                description = tokens[-1]['description']
            else:
                description = ''
            matched_terminator = True
            tokens.pop()
            break

        if not matched_terminator:
            mismatched_terminator_found = True

        return description

    process_terminator_insertions(keyword)

    if keyword in IndentTokens:
        m = re.search(r'([^\s.]+)\s*(\.)?\s*$', line, re.IGNORECASE)
        last_token = m.group(1).casefold()
        if IndentTokens[keyword] != last_token:
            tokens.append({'token': keyword, 'indent': current_indent,
                           'description': description, 'sub-count': 0})
            if debug:
                print("pushed token {} line-no = {}"
                      .format(keyword, current_line_number),
                      file=sys.stderr)
        else:
            if debug:
                print("found start: {} and end: {} on same line!".format(
                    keyword, last_token), file=sys.stderr)

    return ''


def terminate_current_paragraph():
    # terminate the previous paragraph, if one exists and had at least
    # one statement associated with it.
    if len(paragraph_tokens) == 0:
        return
    paragraph_name = paragraph_tokens[-1]

    if paragraph_statement_count[paragraph_name] > 0:
        if add_annotations:
            line = "{}{} *> {} {}".format(
                " " * area_b_num_spaces,
                end_paragraph_token,
                end_paragraph_or_section_annotation,
                paragraph_name)
        else:
            line = "{}{}".format(
                " " * area_b_num_spaces,
                end_paragraph_token)

        if use_uppercase:
            line = line.upper()
        elif use_lowercase:
            line = line.casefold()

        # this is to inform process_keyword_terminators(),
        # so that it can close out any outstanding terminators.
        process_keyword_terminators(line)
        flush_dot_line(line)

        paragraph_statement_count[paragraph_name] = 0
        paragraph_tokens.pop()

        if debug:
            print(("inserted end paragraph/section terminator (.)"
                   + " at line-no = {}").format(current_line_number),
                  file=sys.stderr)


def process_paragraph_termination(line):

    global procedure_division_line_num, procedure_division_end_line_num

    keyword = None
    indent = 0

    if debug:
        print("-" * 40, file=sys.stderr)
        print("process_paragraph_termination={}[{}]"
              .format(current_line_number, line),
              file=sys.stderr)

    m = re.search(section_tokenizer_re, line, re.IGNORECASE)
    if m is not None:
        # note that we want to preserve the current case of the paragraph name
        # here!
        keyword = m.group(1)
        token2 = m.group(2)
        if token2 is None:
            token2 = ""

        token1 = keyword.casefold()
        token2 = token2.casefold()

        end_of_program = is_end_program(token1, token2)
        if is_paragraph_or_section(token1, token2) \
                or end_of_program:
            terminate_current_paragraph()

            if end_of_program:
                paragraph_tokens.clear()
                paragraph_statement_count.clear()
                tokens.clear()
                procedure_division_line_num = -1
                procedure_division_end_line_num = -1
            else:

                paragraph_tokens.append(keyword)
                paragraph_statement_count[keyword] = 0

            if debug:
                print("paragraph/section = {} current_indent={} stack size={}"
                      .format(keyword, indent, len(paragraph_tokens)),
                      file=sys.stderr)
    else:
        m = re.search(r'^(\s*)([^\s]+)(\s|$)', line)
        if m is not None:
            keyword = m.group(2).casefold()
            if keyword != '.' and len(paragraph_tokens) > 0:
                paragraph_name = paragraph_tokens[-1]
                paragraph_statement_count[paragraph_name] += 1

            if debug:
                print("token = {} current_indent={}".format(keyword, indent),
                      file=sys.stderr)

            if keyword == end_paragraph_token:
                if len(paragraph_tokens) > 0:
                    paragraph_name = paragraph_tokens[-1]
                    paragraph_statement_count[paragraph_name] = 0

                    paragraph_tokens.pop()

                    if debug:
                        print("matched paragraph terminator token '.'"
                              + " line-no = {}"
                              + " new stack size = {}"
                              .format(current_line_number,
                                      len(paragraph_tokens)),
                              file=sys.stderr)
                    ret_line = "{} {}".format(
                        end_paragraph_or_section_annotation,
                        paragraph_name)
                    if use_uppercase:
                        ret_line = ret_line.upper()
                    elif use_lowercase:
                        ret_line = ret_line.casefold()

                    return (False, ret_line)
                else:
                    if debug:
                        print("encountered unexpected '.', at line {}"
                              .format(current_line_number), file=sys.stderr)

        else:
            if debug:
                print("(empty line)", file=sys.stderr)
            return (True, "")

    return (False, "")


parser = argparse.ArgumentParser(
    description=USAGE_DOCS,
    formatter_class=argparse.RawTextHelpFormatter)

parser.add_argument('filename',
                    help='name of the cobol source file')
parser.add_argument('-l', '--lowercase',
                    help='convert to all lowercase',
                    action="store_true")
parser.add_argument('-u', '--uppercase',
                    help='convert to all uppercase.',
                    action="store_true")
parser.add_argument('-cl', '--clear-left-margin',
                    help='blank out left margin (cols 1-6, inclusive)',
                    action="store_true")
parser.add_argument('-cr', '--clear-right-margin',
                    help='blank out right margin (cols >= 73)',
                    action="store_true")
parser.add_argument('-cs', '--clear-empty-asterisk-comments',
                    help='blank out empty asterisk comment lines',
                    action="store_true")
parser.add_argument(
    '-ca', '--clear-all',
    help='''blank out left and right margins and blank out empty
asterisk comment lines''',
    action="store_true")
parser.add_argument('-A', '--suppress-annotations',
                    help='suppress annotation of terminators',
                    action="store_true")
parser.add_argument(
    '-b', '--batch-mode',
    help='''suppress printing of final line indicating the output file
that the changes were written to''',
    action="store_true")
parser.add_argument('-d', '--debug',
                    help='enable verbose debugging output',
                    action="store_true")
parser.add_argument('-o', '--output-filename',
                    help='output to specified filename')

argcomplete.autocomplete(parser)
args = parser.parse_args()

input_filename = args.filename
input_filename = os.path.expanduser(input_filename)
input_filename = os.path.realpath(input_filename)

header_info = []

batch_mode = args.batch_mode
use_lowercase = args.lowercase
use_uppercase = args.uppercase

header_info.append("clean-cobol.py")

if use_lowercase:
    header_info.append('--lowercase')
elif use_uppercase:
    header_info.append('--uppercase')

clear_left_margin = args.clear_left_margin
clear_right_margin = args.clear_right_margin
clear_empty_asterisk_comments = args.clear_empty_asterisk_comments

if args.clear_all:
    clear_left_margin = True
    clear_right_margin = True
    clear_empty_asterisk_comments = True

if clear_left_margin:
    header_info.append("--clear-left-margin")
if clear_right_margin:
    header_info.append("--clear-right-margin")

if clear_empty_asterisk_comments:
    header_info.append("--clear-empty-asterisk-comments")

add_annotations = not args.suppress_annotations
if not add_annotations:
    header_info.append("--suppress-annotations")

if len(header_info) == 1:
    header_info.append("(noargs)")

debug = args.debug

if args.output_filename is None:
    (basename, extension) = split_into_basename_and_ext(input_filename)
    if len(extension) == 0:
        extension = 'cbl'

    output_filename = "".join((basename,
                               generated_file_marker,
                               '.',
                               extension))
else:
    output_filename = args.output_filename
    output_filename = os.path.expanduser(output_filename)
    output_filename = os.path.realpath(output_filename)

if input_filename == output_filename:
    fatal("Input and output filenames are IDENTICAL: {}"
          .format(output_filename))


procedure_division_line_num = -1
current_line_number = 0

procedure_division_token = 'procedure division'
procedure_division_end_line_num = -1

with open(input_filename) as input_file, \
        open(output_filename, 'w') as output_file:

    for line in input_file:
        current_line_number += 1
        line_comments = ''
        annotation = ""
        dot_annotation = ""

        if current_line_number == 1:
            print_multiline_comment(" ".join(header_info))

        line = line.rstrip()

        if is_asterisk_comment_line(line):
            if clear_empty_asterisk_comments \
                    and is_empty_asterisk_comment_line(line):
                queue_asterisk_comments("")
                continue

            queue_asterisk_comments(line)
            continue

        (line, line_comments) = split_at_comment(line)
        (line, left_column, right_column) = clear_out_margins(
            line,
            clear_left_margin,
            clear_right_margin)

        line_chars = list(line)
        masked_chars = ['\x00'] * len(line_chars)
        mask_quoted(line_chars, masked_chars)

        if use_uppercase:
            uppercase_line(line_chars)
        elif use_lowercase:
            lowercase_line(line_chars)

        if procedure_division_line_num < 0:
            m = re.search(procedure_division_token, "".join(line_chars),
                          re.IGNORECASE)
            if m is not None:
                procedure_division_line_num = current_line_number

        if (procedure_division_line_num > 0) \
                and (procedure_division_end_line_num < 0):
            if '.' in line_chars:
                procedure_division_end_line_num = current_line_number
                paragraph_tokens.append(implicit_first_paragraph_name)
                paragraph_statement_count[implicit_first_paragraph_name] = 0

        if (procedure_division_end_line_num > 0) \
                and (current_line_number > procedure_division_end_line_num):
            remove_trailing_dot(line_chars)

            if is_continuation_line(line_chars):
                flush_line(line_chars, masked_chars, line_comments,
                           left_column, right_column)
                continue

            line = "".join(line_chars)
            (is_empty_line, dot_annotation) = process_paragraph_termination(
                line)
            if is_empty_line:
                queue_comments(line_chars, masked_chars, line_comments,
                               left_column, right_column)
                continue

            annotation = process_keyword_terminators(line, masked_chars)

        # It's always going to be, at most, one or the other, but never
        # both at the same time.
        annotation = annotation + dot_annotation

        if len(annotation) > 0 and len(line_comments) == 0:
            flush_line(line_chars, masked_chars, line_comments
                       + ' : ' + annotation, left_column, right_column)
        else:
            flush_line(line_chars, masked_chars, line_comments, left_column,
                       right_column)

    terminate_current_paragraph()
    flush_queued_comments()

if mismatched_terminator_found:
    print("  WARNING: Encountered one or more MISMATCHED terminators!",
          file=sys.stderr)
    print("  Check the generated output file for details.", file=sys.stderr)

if not batch_mode:
    print("(changes are in {})".format(output_filename))
