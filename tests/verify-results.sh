#!/usr/bin/env bash
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

# the order in which these flag combos appear correspond to the numerical ordering of
# each test. e.g. '--lowercase' only flag combo results will look like:
# <base_filename>.baseline.1.cbl and <base_filename>.current.1.cbl, respectively.
flag_combos=(
    ''  # 0
    '--lowercase'  # 1
    '--uppercase'  # 2
    '--clear-left-margin'  # 3
    '--clear-right-margin'  # 4
    '--clear-empty-asterisk-comments'  #5
    '--clear-all'  #6
    '--suppress-annotations'  #7
    '--clear-left-margin --clear-right-margin'  #8
    '--lowercase --clear-all'  #9
    '--uppercase --clear-all'  #10
    )


usage() {
    local prog="$(basename "$0")"

    cat <<EOF

Usage: ${prog} [-g/--generate] [-f/--force] [FILE1 FILE2..]

    -g/--generate --> generate baseline results
    -f/--force    --> Use with the -g/--generate option, to force overwrite
                      of existing baseline result files. By default, this
                      script will SKIP OVER generation of baseline results
                      that already exist.
    -p/--path <path to clean-cobol.py executable script>
                  --> use specified path, in preference to the
                      'clean-cobol.py' from your PATH.
    -v/--verbose  --> show diffs, if there is a discrepancy between the
                      baseline and current results.
    -t/--test <number> --> run ONLY test number. by default, will
                      run all available tests.
    -C/--no-color --> suppress text colors. useful if you are redirecting
                      the output of this script to a file, for further
                      processing.

    FILE1 FILE2          --> if specified, will run all tests on FILE1 FILE2
                      etc., instead of on the files listed in
                      'list-of-test-files.text'.

This shell script runs ${prog} a number of times on a set of cobol
source files, using different flags.

EOF
}



# how to tell if we're running in a terminal versus being piped.
# this is useful as we only want to generate color text output if we're
# running in an interactive terminal, as the color codes tend to
# garble the results a bit.
is_running_in_terminal() {
    [[ -t 1 ]] || [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]
}

#
# color sequence, followed by text
# example:
#   echo -e "hello ${color_txtred} there ${color_reset} "
#
# setting terminal colors only makes sense in the context of an
# interactive terminal
set_text_colors() {
    local enable_flag="$1"
    if (( enable_flag == 1 ))
    then
        export color_txtblk='\e[0;30m' # Black - Regular
        export color_txtred='\e[0;31m' # Red
        export color_txtgrn='\e[0;32m' # Green
        export color_txtylw='\e[0;33m' # Yellow
        export color_txtblu='\e[0;34m' # Blue
        export color_txtpur='\e[0;35m' # Purple
        export color_txtcyn='\e[0;36m' # Cyan
        export color_txtwht='\e[0;37m' # White
        export color_bldblk='\e[1;30m' # Black - Bold
        export color_bldred='\e[1;31m' # Red
        export color_bldgrn='\e[1;32m' # Green
        export color_bldylw='\e[1;33m' # Yellow
        export color_bldblu='\e[1;34m' # Blue
        export color_bldpur='\e[1;35m' # Purple
        export color_bldcyn='\e[1;36m' # Cyan
        export color_bldwht='\e[1;37m' # White
        export color_unkblk='\e[4;30m' # Black - Underline
        export color_undred='\e[4;31m' # Red
        export color_undgrn='\e[4;32m' # Green
        export color_undylw='\e[4;33m' # Yellow
        export color_undblu='\e[4;34m' # Blue
        export color_undpur='\e[4;35m' # Purple
        export color_undcyn='\e[4;36m' # Cyan
        export color_undwht='\e[4;37m' # White
        export color_bakblk='\e[40m'   # Black - Background
        export color_bakred='\e[41m'   # Red
        export color_badgrn='\e[42m'   # Green
        export color_bakylw='\e[43m'   # Yellow
        export color_bakblu='\e[44m'   # Blue
        export color_bakpur='\e[45m'   # Purple
        export color_bakcyn='\e[46m'   # Cyan
        export color_bakwht='\e[47m'   # White
        export color_reset='\e[0m'    # Text Reset
    else
        unset color_txtblk color_txtred color_txtgrn color_txtylw color_txtblu \
            color_txtpur color_txtcyn color_txtwht color_bldblk color_bldred \
            color_bldgrn color_bldylw color_bldblu color_bldpur color_bldcyn \
            color_bldwht color_unkblk color_undred color_undgrn color_undylw \
            color_undblu color_undpur color_undcyn color_undwht color_bakblk \
            color_bakred color_badgrn color_bakylw color_bakblu color_bakpur \
            color_bakcyn color_bakwht color_reset
    fi
}

unset IFS  # avoid potential issues with IFS being set to a non-standard value..
cleaner="$(which 'clean-cobol.py')"
getopt="$(which 'getopt')"

if [[ $getopt == "" ]] || [[ $(getopt --version) != *getopt* ]]
then
    cat <<EOF

    You need to have the GNU version of getopt in your PATH!
EOF
    exit 1
fi

list_of_test_files='list-of-test-files.text'

force_overwrite=0

if ! PARSED_OPTIONS="$($getopt -n "$0"  -o gfvhCp:t: \
    --long "generate,force,verbose,help,no-color,test:,path:" -- "$@")"
then
    #Bad arguments, something has gone wrong with the getopt command.
    usage && exit 1
fi

# A little magic, necessary when using getopt.
eval set -- "$PARSED_OPTIONS"

modifier=current
verbose=0
limit_test_number=-1
enable_colors=1

if is_running_in_terminal
then
    set_text_colors 1
else
    set_text_colors 0
fi

while true
do
    case "$1" in
        -g | --generate)
            modifier=baseline
            shift
            ;;

        -f | --force)
            force_overwrite=1
            shift
            ;;

        -v | --verbose)
            verbose=1
            shift
            ;;

        -C | --no-color)
            enable_colors=0
            shift
            ;;

        -p | --path)
            path="$2"
            cleaner="$(which "${path}")"
            [[ $cleaner != "" ]] && printf "${color_txtgrn}" &&
                echo && echo "(using ${cleaner})" &&
                printf "${color_reset}"
            shift 2
            ;;

        -t | --test)
            limit_test_number="$2"
            shift 2
            ;;

        -h | --help)
            usage
            exit 1
            ;;

        --)
            shift
            break
            ;;

        *)
            break
            ;;
    esac
done

diff_options=('--strip-trailing-cr' '--color=auto')
if (( enable_colors == 1 ))
then
    diff_options[1]='--color=always'
else
    diff_options[1]='--color=never'
fi

set_text_colors "$enable_colors"

file_list=()
if (( $# > 0 ))
then
    file_list+=("$@")
else
    readarray -t file_list < "${list_of_test_files}"
fi

[[ $cleaner == "" ]] && echo "Unable to find 'clean-cobol.py' executable script in your PATH!" && exit 1



common_flags='--batch-mode'
program_name="$(basename "$0")"
fatal_error=0

[[ ! -f "${list_of_test_files}" ]] && echo "file ${list_of_test_files} is MISSING!" && exit 1

for file in "${file_list[@]}"
do
    if [[ ! -f "${file}" ]]
    then
        printf "${color_txtred}"
        echo "input file ${file} does not exist!"
        printf "${color_reset}"
        fatal_error=1
        continue
    fi

    if [[ "${file}" != *.* ]]
    then
        printf "${color_txtred}"
        cat <<EOF
  Input file ${file} is missing an extension, such as
  '.cbl' or '.cob'.
EOF
        printf "${color_reset}"
        fatal_error=1
        continue
    fi

    if [[ "${file}" == *.current* ]] || [[ "${file}" == *.baseline* ]]
    then
        printf "${color_txtred}"
        cat <<EOF
  File: ${file}
  To avoid recursive naming issues, you may NOT run
  ${program_name} on files that have either
  '.current' or '.baseline'
  as part of their filename.
  -----
EOF
        printf "${color_reset}"
        fatal_error=1
        continue
    fi
done

max_test_number=$(( ${#flag_combos[@]} - 1 ))
if (( limit_test_number > -1 ))
then
    if (( limit_test_number > max_test_number))
    then
        printf "${color_txtred}"
        cat <<EOF

You specified test number ${limit_test_number} which is greater
than the available range of: 0 through ${max_test_number}, inclusive.

EOF
        printf "${color_reset}"
        fatal_error=1
    fi
fi

(( fatal_error == 1 )) && echo && exit 1

for file in "${file_list[@]}"
do
    base=${file%%.*}
    ext=${file#*.}

    for index in "${!flag_combos[@]}"
    do
        (( limit_test_number > -1 )) && (( limit_test_number != index )) && continue

        output_file=${base}.${index}.${modifier}.${ext}
        baseline_output_file=${base}.${index}.baseline.${ext}

        if (( force_overwrite == 0 )) && [[ ${output_file} == "${baseline_output_file}" ]]
        then
            if [[ -f "${baseline_output_file}" ]]
            then
                printf "${color_txtylw}"
                echo "(SKIPPING regeneration of: ${baseline_output_file})"
                printf "${color_reset}"
                continue
            fi
        fi

        ${cleaner} "${file}" ${flag_combos[$index]} ${common_flags} -o "${output_file}"

        if [[ ${output_file} == "${baseline_output_file}" ]]
        then
            printf "${color_txtgrn}"
            echo "(generated: ${baseline_output_file})"
            printf "${color_reset}"
        else
            if [[ ! -f ${baseline_output_file} ]]
            then
                printf "${color_txtred}"
                echo "ERROR: baseline file: ${baseline_output_file} does not exist!"
                printf "${color_reset}"
                continue
            fi

            if ! diff "${diff_options[@]}" \
                    "${baseline_output_file}" \
                    "${output_file}" 1>/dev/null 2>&1
            then
                printf "${color_txtred}"
                echo "FAILED: ${baseline_output_file} : ${output_file}"
                printf "${color_reset}"

                if (( verbose == 1 ))
                then
                    diff "${diff_options[@]}" \
                        "${baseline_output_file}" \
                        "${output_file}"
                    echo '-------------------------------------------'
                fi
            else
                printf "${color_bldgrn}"
                echo "PASSED: ${baseline_output_file} : ${output_file}"
                printf "${color_reset}"
            fi
        fi
    done
done
