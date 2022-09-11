      **> clean-cobol.py --clear-left-margin
       identification division.
       program-id. process-command-args.

       environment division.

       data division.

       working-storage section.

       01 misc-vars.
           05 idx pic 9(2).
           05 arg-len pic 9(2) value zero.


       linkage section.

       copy "param-group.cpy".



       procedure division using param-group.

           accept param-count from argument-number

           if param-count > 10
               move 10 to param-count
           end-if *> : param-count > 10

           perform
               varying idx from 1 by 1
               until idx > param-count
               accept param(idx) from argument-value
               *> null terminate each argument.
               inspect param(idx)
                   replacing trailing space by x'00'

           end-perform

           goback
           . *> : end <main>
