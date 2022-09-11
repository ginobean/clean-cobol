
       *> used by process-command-args.cbl as well as apps that
       *> call process-command-args.

       01 param-group.
           05 param-count pic 99.
           05 param pic x(256) occurs 10 times.
