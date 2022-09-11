      **> clean-cobol.py --uppercase --clear-left-margin --clear-right-m
      **> argin --clear-empty-asterisk-comments
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-SET-1.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

       SELECT INPUT-FILE ASSIGN TO 'input-file.text'
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD PIC X(80).


       WORKING-STORAGE SECTION.
002300*> USED BY PROCESS-COMMAND-ARGS.CBL AS WELL AS APPS THAT          AST
       *> CALL PROCESS-COMMAND-ARGS.                                    AST

       COPY "param-group.cpy".



       01 WORK-VARS.
           05 FLAG-1 PIC X VALUE 'N'.
               88 FLAG-1-VALID VALUE 'Y'.
           05 FLAG-2 PIC X VALUE 'Y'.
               88 FLAG-2-VALID VALUE 'Y'.
           05 FLAG-3 PIC X VALUE 'N'.
           05 FLAG-4 PIC X VALUE 'Y'.
           05 FLAG-5 PIC X VALUE 'N'.
           05 FLAG-6 PIC X VALUE 'Y'.
           05 CHANGE-PERCENT PIC ZZ9.99- VALUE SPACE.

           05 IDX-1 PIC 9(3).
           05 MSG PIC X(80).
           05 EOF-SWITCH PIC X VALUE 'N'.
               88 INPUT-FILE-EOF VALUE 'Y' FALSE 'N'.



       PROCEDURE DIVISION.

       MAIN.
           CALL 'process-command-args' USING PARAM-GROUP
               ON EXCEPTION
                   DISPLAY 'the SUBROUTINE CALL TO process-command-args
      -                ' FAILED!'
           END-CALL

           PERFORM DO-STUFF
           PERFORM DO-STUFF-2
           PERFORM DO-STUFF-3
           PERFORM DO-STUFF-4
           PERFORM DO-STUFF-5
           PERFORM DO-STUFF-6
           PERFORM DO-STUFF-7
           PERFORM DO-STUFF-8
           PERFORM DO-STUFF-9

           PERFORM DO-STUFF-10



       GOBACK
           . *> END MAIN


       *> test handling of 'if', 'else', 'then'                         AST
       DO-STUFF.

           *> t1: test if statement by itself                           DECAST
           IF FLAG-1 = 'N'
               DISPLAY 't1: flag-1 is false'
           END-IF *> FLAG-1 = 'N'

           *> t2: test if with a two part expression                    AST
           IF FLAG-1 = 'Y' OR FLAG-2 = 'Y'
               DISPLAY 't2: at least one of two flags is ''Y'''
           END-IF *> FLAG-1 = 'Y' OR FLAG-2 = 'Y'

           *> t3: test if with else clause                              AST
           IF FLAG-1 = 'N'
               DISPLAY 't3: flag-1 is false'
           ELSE
               DISPLAY 't3: flag-2 is true'
           END-IF *> FLAG-1 = 'N'

           *> t4: test if with 'then'                                   AST
           IF FLAG-1 = 'N'
           THEN
               DISPLAY 't4: flag-1 is false'
           END-IF *> FLAG-1 = 'N'

           *> t5: test if with a matching end-if                        AST
           IF FLAG-1 = 'N'
           THEN
               DISPLAY 't5: flag-1 is false'
           END-IF *> : FLAG-1 = 'N'

           *> t6: test if with multiple statements within it.           AST
           IF FLAG-1 = 'N'
               DISPLAY 't6: hello world'
               DISPLAY '  t6: hello land'
               DISPLAY '  t6: hello mellow'
           END-IF *> FLAG-1 = 'N'

           *> t7: test if-else with multiple statements w/i.            AST
           IF FLAG-1 = 'N'
               DISPLAY 't7: when in the sessions'
               DISPLAY '  t7: of sweet silent thoughts,'
               DISPLAY '  t7: I summon up remembrance'
           ELSE
               DISPLAY '  t7: of things past.'
               DISPLAY '  t7: I sight the lack of many a thing'
           END-IF *> FLAG-1 = 'N'


           *> t8: test if-then with multiple statements within it.      AST
           IF FLAG-1 = 'N'
           THEN
               DISPLAY 't8: hello world'
               DISPLAY '   t8: hello land'
               DISPLAY '  t8: hello mellow'
           END-IF *> FLAG-1 = 'N'

           *> t9: test if-then-else with multiple statements w/i.       DECAST
           IF FLAG-1 = 'N'
           THEN
               DISPLAY '       t9: when in the sessions'
               DISPLAY '  t9: of sweet silent thoughts,'
               DISPLAY '  t9: I summon up remembrance'
           ELSE
               DISPLAY '  t9: of things past.'
               DISPLAY '  t9: I sigh the lack of many a thing'
           END-IF *> FLAG-1 = 'N'

           *> t10: test 1 line if statement                             AST
           IF FLAG-6 = 'Y' DISPLAY 't10: and with old woes'
           END-IF *> FLAG-6 = 'Y' DISPLAY 't10: and with old woes'

           *> t10.1: test 1 line if-ele statement                       AST
           IF FLAG-6 = 'N' DISPLAY "t10.1" ELSE DISPLAY 't10.1: heavy'
           END-IF *> FLAG-6 = 'N' DISPLAY "t10.1" ELSE DISPLAY 't10.1: heavy'
           . *> END DO-STUFF


       STUFF-SET-1 SECTION.
       *> test already terminated if end-if pairs.                      AST
       DO-STUFF-2.

           *> t11: test if - end-if statement by itself                 AST
           IF FLAG-1 = 'N'
               DISPLAY 't11: flag-1 is false'
           END-IF *> : FLAG-1 = 'N'

           *> t12: test if - end-if  with a two part expression
           IF FLAG-1 = 'Y' OR FLAG-2 = 'Y'
               DISPLAY 't12: at least one of two flags is ''Y'''
           END-IF *> : FLAG-1 = 'Y' OR FLAG-2 = 'Y'

           *> t13: test if end-if with else clause                      AST
           IF FLAG-1 = 'N'
               DISPLAY 't13: flag-1 is false'
           ELSE
               DISPLAY 't13: flag-2 is true'
           END-IF *> : FLAG-1 = 'N'

           *> t14: test if end-if with 'then'                           AST
           IF FLAG-1 = 'N'
           THEN
               DISPLAY 't14: flag-1 is false'
           END-IF *> : FLAG-1 = 'N'

           *> t15: test if end-if with a matching end-if
           IF FLAG-1 = 'N'
           THEN
               DISPLAY 't15: flag-1 is false'
           END-IF *> : FLAG-1 = 'N'

           *> t16: test if end-if with multiple statements within it.
           IF FLAG-1 = 'N'
               DISPLAY 't16: hello world'
               DISPLAY '  t6: hello land'
               DISPLAY '  t6: hello mellow'
           END-IF *> : FLAG-1 = 'N'

           *> t17: test if-else end-if with multiple statements w/i.
           IF FLAG-1 = 'N'
               DISPLAY 't17: when in the sessions'
               DISPLAY '  t7: of sweet silent thoughts,'
               DISPLAY '  t7: I summon up remembrance'
           ELSE
               DISPLAY '  t7: of things past.'
               DISPLAY '  t7: I sight the lack of many a thing'
           END-IF *> : FLAG-1 = 'N'

           *> t18: test if-then end-if with multiple statements within it.
           IF FLAG-1 = 'N'
           THEN
               DISPLAY 't18: hello world'
               DISPLAY '   t8: hello land'
               DISPLAY '  t8: hello mellow'
           END-IF *> : FLAG-1 = 'N'

           *> t19: test if-then-else end-if with multiple statements w/i.
           IF FLAG-1 = 'N'
           THEN
               DISPLAY '       t19: when in the sessions'
               DISPLAY '  t19: of sweet silent thoughts,'
               DISPLAY '  t19: I summon up remembrance'
           ELSE
               DISPLAY '  t19: of things past.'
               DISPLAY '  t19: I sigh the lack of many a thing'
           END-IF *> : FLAG-1 = 'N'

           *> t20: start token and end terminator are on same line.
           IF FLAG-5 = 'N' DISPLAY 't20: and with' END-IF
           . *> END DO-STUFF-2


       DO-STUFF-3.

           *> t21: linear nested if's.
           IF NOT FLAG-1-VALID
               DISPLAY 't21: now is the winter'
               DISPLAY 't21: of our discontent.'
           ELSE IF FLAG-2-VALID
               DISPLAY 't21: a quick brown fox'
               DISPLAY 't21: jumped over the lazy dog'
               DISPLAY 't21: and up a small hill'
           ELSE IF FLAG-3 = 'Y'
               DISPLAY 't21: a thing of beauty is'
               DISPLAY 't21: a joy forever'
               DISPLAY 't21: it''s loveliness increases.'
           ELSE IF NOT FLAG-4 = 'Y'
               DISPLAY 't21: it will never pass into nothingness'
               DISPLAY 't21: but still will keep a bower'
               DISPLAY 't21: quiet and safe for us.'
           ELSE IF FLAG-5 = 'Y' OR FLAG-6 = 'Y'
               DISPLAY 't21: it was a lover and his lass'
               DISPLAY 't21: with a hey and ho and a '
               DISPLAY 't21: hey, nonino'
               DISPLAY 't21: that over the cornfield did pass.'
           ELSE
               DISPLAY 't21: in the springtime'
               DISPLAY 't21: the pretty little ringtime'
               DISPLAY 't21: yada yada yada'
           END-IF *> FLAG-5 = 'Y' OR FLAG-6 = 'Y'
           END-IF *> NOT FLAG-4 = 'Y'
           END-IF *> FLAG-3 = 'Y'
           END-IF *> FLAG-2-VALID
           END-IF *> NOT FLAG-1-VALID
           DISPLAY 't21: shall i compare thee to a summer''s day?'
           DISPLAY 't21: thou art more lovely and more temperate.'
           DISPLAY 't21: rough winds do shake the darling buds of may.'
           . *> END DO-STUFF-3


       DO-STUFF-4.

           *> t22: nested if's, with a continuation line.
           IF NOT FLAG-1-VALID
               IF FLAG-2-VALID
                   IF FLAG-3 = 'N'
                       DISPLAY 't22: from fairest creatures we desire in
      -    'crease. that thereby beauty''s rose might never die'
                   END-IF *> FLAG-3 = 'N'
               END-IF *> FLAG-2-VALID
           END-IF *> NOT FLAG-1-VALID


           *> t23: nested if's, with multiple continuation lines.
           IF NOT FLAG-1-VALID
               IF FLAG-2-VALID
                   IF FLAG-3 = 'N'
                       DISPLAY 't23: but, as the riper should by time de
      -    'decrease, His tender heir might bear his memory But thou, co
      -       'ntracted to thine own bright eyes..'
                   END-IF *> FLAG-3 = 'N'
               END-IF *> FLAG-2-VALID
           END-IF *> NOT FLAG-1-VALID

           *> t24: nested if then else's.                               AST
           IF FLAG-1-VALID
               IF NOT FLAG-2-VALID
                   IF FLAG-3 = 'Y'
                       DISPLAY 't24: when forty winters shall besiege'
                       DISPLAY 't24: thy brow and dig deep trenches'
                   ELSE
                       DISPLAY 't24: in thy beauty''s field, '
                       DISPLAY "t24: thy youth's proud livery, so gazed"
                       DISPLAY 't24: on now, will be a tatter''d'
                       DISPLAY 't24: weed of small worth held'
                   END-IF *> FLAG-3 = 'Y'
               ELSE
                   DISPLAY 't24: look in thy glass'
               END-IF *> NOT FLAG-2-VALID
           ELSE
               DISPLAY 't24: and test the face thou viewest'
               DISPLAY "now is the time that face should form another"
           END-IF *> FLAG-1-VALID

           *> t25: nested if's with some linear nested if's inside
           IF FLAG-1-VALID OR FLAG-2-VALID
               IF FLAG-3 = 'N'
                   IF FLAG-4 = 'Y'
                       DISPLAY 't25: when forty winters shall besiege'
                       DISPLAY 't25: thy brow and dig deep trenches'

                       IF NOT FLAG-1-VALID
                          DISPLAY '    t25: now is the winter'
                          DISPLAY 't25: of our discontent.'
                       ELSE IF FLAG-2-VALID
                          DISPLAY 't25: a quick brown fox'
                          DISPLAY 't25: jumped over the lazy dog'
                          DISPLAY 't25: and up a small hill'
                       ELSE IF FLAG-3 = 'Y'
                          DISPLAY 't25: a thing of beauty is'
                          DISPLAY 't25: a joy forever'
                          DISPLAY 't25: it''s loveliness increases.'
                       ELSE IF NOT FLAG-4 = 'Y'
                          DISPLAY 't25: it will never pass into nothingn
      -                        'ess'
                          DISPLAY 't25: but still will keep a bower'
                          DISPLAY 't25: quiet and safe for us.'
                       ELSE IF FLAG-5 = 'Y' OR FLAG-6 = 'Y'
                          DISPLAY 't25: it was a lover and his lass'
                          DISPLAY 't25: with a hey and ho and a '
                          DISPLAY 't25: hey, nonino'
                          DISPLAY 't25: that over the cornfield did pass
      -                        '.'
                       ELSE
                          DISPLAY 't25: in the springtime'
                          DISPLAY 't25: the pretty little ringtime'
                          DISPLAY 't25: yada yada yada'

                       *> the COBOL compiler itself doesn't notice the
                       *> indentation levels, so these end-if's are
                       *> needed here to correlate with the intentions
                       *> manifested by the indent levels, so that
                       *> we get the SAME output results before and     AST
                       *> after filtering through clean-cobol-source.py AST
                       END-IF *> : FLAG-5 = 'Y' OR FLAG-6 = 'Y'
                       END-IF *> : NOT FLAG-4 = 'Y'
                       END-IF *> : FLAG-3 = 'Y'
                       END-IF *> : FLAG-2-VALID
                       END-IF *> : NOT FLAG-1-VALID
                       DISPLAY 't25: shall i compare thee to a summer''s
      -                     'day?'
                       DISPLAY 't25: thou art more lovely and more tempe
      -                    'rate.'
                       DISPLAY 't25: rough winds do shake the darling bu
      -                    'ds of may.'
                   ELSE
                       DISPLAY 't25: in thy beauty''s field, '
                       DISPLAY "t25: thy youth's proud livery, so gazed"
                       DISPLAY 't25: on now, will be a tatter''d'
                       DISPLAY 't25: weed of small worth held'
                   END-IF *> FLAG-4 = 'Y'
               ELSE
                   DISPLAY 't25: look in thy glass'
               END-IF *> FLAG-3 = 'N'
           ELSE
               DISPLAY 't25: and test the face thou viewest'
               DISPLAY "t25: now is the time that face should form."
           END-IF *> FLAG-1-VALID OR FLAG-2-VALID
           . *> END DO-STUFF-4

       STUFF-SET-2 SECTION.

       DO-STUFF-5.
           *> t26: test case where paragraph already ends in a
           *> single dot on a line by itself
           DISPLAY 't26: shall i compare thee to a summer''s day?'
           DISPLAY 't26: thou art more lovely and more temperate'
           . *> single solitary dot                                     AST

       DO-STUFF-6.
           *> t27: test single dot on a line by itself with no
           *> comments on same line.
           DISPLAY 't27: rough winds do shake the darling buds of may'
           . *> : END DO-STUFF-6

       DO-STUFF-7.
           *> t28: test that dots within are not removed.
           MOVE 123.45 TO CHANGE-PERCENT
           MOVE 987.61 TO CHANGE-PERCENT
           . *> END DO-STUFF-7

       *> non-inline performs. should not be terminated by
       *> end-perform, as GnuCOBOL 3.x will reject it.
       DO-STUFF-8.

           *> t29
           MOVE 't29' TO MSG
           PERFORM MORE-STUFF
           VARYING IDX-1 FROM 1 BY 1
           UNTIL IDX-1 = 4

           MOVE 't30' TO MSG
           PERFORM MORE-STUFF
           WITH TEST BEFORE
           UNTIL IDX-1 > 3

           MOVE 't31' TO MSG
           PERFORM MORE-STUFF
           WITH TEST AFTER
           UNTIL IDX-1 > 3

           MOVE 't32' TO MSG
           PERFORM MORE-STUFF VARYING IDX-1 FROM 1 BY 1 UNTIL IDX-1 = 4

           MOVE 't33' TO MSG
           PERFORM MORE-STUFF WITH TEST BEFORE UNTIL IDX-1 > 3

           MOVE 't34' TO MSG
           PERFORM MORE-STUFF WITH TEST AFTER UNTIL IDX-1 > 3
           . *> END DO-STUFF-8

       *> inline performs
       DO-STUFF-9.

           *> t40
           *> note that this test will FAIL, due to the inline
           *> clean-cobol.py expects the inline section to be
           *> indented to the right of the 'perform' line.
           *> this is for documentation purposes ONLY.
           *> update: It looks like GnuCobol 3.x already
           *> *requires* an end-perform, at the end of an inline
           *> perform, so this particular issue is moot.
        *>    perform varying idx-1 from 1 by 1
        *>    until idx-1 > 4
        *>    display 't40'.                                            AST

           *> t41                                                       AST
           PERFORM VARYING IDX-1 FROM 1 BY 1
           UNTIL IDX-1 > 4
                 DISPLAY 't41: look in thy glass and tell the face'
           END-PERFORM

           *> t42                                                       AST
           PERFORM VARYING IDX-1 FROM 1 BY 1
           UNTIL IDX-1 > 4
                 DISPLAY 't42: thou viewest'
                 DISPLAY 't42: now is the time that face should form'
                 DISPLAY 't42: another.'
           END-PERFORM

           *> t43
           PERFORM
           WITH TEST AFTER
           VARYING IDX-1 FROM 1 BY 1
           UNTIL IDX-1 > 3
                 DISPLAY 't43: whose fresh repair if now thou not'
                 DISPLAY 't43: renewest. thou dost beguild the world'
                 DISPLAY 't43: unbless some mother.'
           END-PERFORM

           *> t44
           MOVE 1 TO IDX-1
           PERFORM
           UNTIL IDX-1 > 3
               DISPLAY "t44: for where is she so fair whose uneared"
               ADD 1 TO IDX-1
           END-PERFORM

           *> t45                                                       AST
           MOVE 1 TO IDX-1
           PERFORM
           WITH TEST AFTER
           UNTIL IDX-1 > 4
               DISPLAY "t45: womb disdains the tillage"
               DISPLAY "t45: of thy husbandry?"
               COMPUTE IDX-1 = IDX-1 + 1
           END-PERFORM

           *> t46 : read/write/rewrite/start/search/call all
           *> have extremely similar patterns.
           OPEN INPUT INPUT-FILE
           PERFORM UNTIL INPUT-FILE-EOF
               READ INPUT-FILE
                   AT END
                       SET INPUT-FILE-EOF TO TRUE
                   NOT AT END
                       DISPLAY 't46: ' INPUT-RECORD
               END-READ
           END-PERFORM

           CLOSE INPUT-FILE


           *> t47: test case where the end terminator is already presentAST
           OPEN INPUT INPUT-FILE
           PERFORM UNTIL INPUT-FILE-EOF
               READ INPUT-FILE
                   AT END
                       SET INPUT-FILE-EOF TO TRUE
                   NOT AT END
                       DISPLAY 't47: ' INPUT-RECORD
                       CONTINUE *> no-op statement.
               END-READ
           END-PERFORM

           CLOSE INPUT-FILE
           . *> END DO-STUFF-9



       DO-STUFF-10.

           EVALUATE TRUE
               WHEN FLAG-3 = 'N'
                   DISPLAY 't51: shall i compare thee to a summer''s'
                   DISPLAY 't51: day? thou art more lovely and more '
               WHEN FLAG-4 = 'Y'
                   DISPLAY 't51: temperate'
           END-EVALUATE

           DISPLAY 't51: rough winds do shake the darling buds of may'

           *> t52
           EVALUATE TRUE
               WHEN FLAG-3 = 'Y'
                   DISPLAY 't52: shall i compare thee to a summer''s'
                   DISPLAY 't52: day? thou art more lovely and more '
               WHEN FLAG-4 = 'Y'
                   DISPLAY 't52: temperate'
           END-EVALUATE

           DISPLAY 't52: rough winds do shake the darling buds of may'

           IF FLAG-1
           >= 'M'
               DISPLAY 't53: test conditional operator handling'
           END-IF *> FLAG-1
           DISPLAY 't53: after conditional.'
           . *> END DO-STUFF-10


       MORE-STUFF SECTION.
           *> t53 -- '*' comments should be passed through, unmodified.
      * example of an '*' comment.
      * yet another '*' comment.

           *> t54 - these next two '*' comments should be filtered out.


           DISPLAY 'test:' MSG
           . *> END MORE-STUFF
