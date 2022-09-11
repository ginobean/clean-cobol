      **> clean-cobol.py --uppercase
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROCESS-COMMAND-ARGS.

       ENVIRONMENT DIVISION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01 MISC-VARS.
           05 IDX PIC 9(2).
           05 ARG-LEN PIC 9(2) VALUE ZERO.


       LINKAGE SECTION.

       COPY "param-group.cpy".



       PROCEDURE DIVISION USING PARAM-GROUP.

           ACCEPT PARAM-COUNT FROM ARGUMENT-NUMBER

           IF PARAM-COUNT > 10
               MOVE 10 TO PARAM-COUNT
           END-IF *> : PARAM-COUNT > 10

           PERFORM
               VARYING IDX FROM 1 BY 1
               UNTIL IDX > PARAM-COUNT
               ACCEPT PARAM(IDX) FROM ARGUMENT-VALUE
               *> null terminate each argument.
               INSPECT PARAM(IDX)
                   REPLACING TRAILING SPACE BY X'00'

           END-PERFORM

           GOBACK
           . *> : END <MAIN>
