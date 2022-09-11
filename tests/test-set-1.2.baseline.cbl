      **> clean-cobol.py --uppercase
000100 IDENTIFICATION DIVISION.                                         AST
000200 PROGRAM-ID. TEST-SET-1.                                          AST
000300                                                                  AST
000400 ENVIRONMENT DIVISION.                                            AST
000500                                                                  AST
000600 INPUT-OUTPUT SECTION.                                            AST
000700                                                                  AST
000800 FILE-CONTROL.                                                    AST
000900                                                                  AST
001000 SELECT INPUT-FILE ASSIGN TO 'input-file.text'                    AST
001100     ORGANIZATION IS LINE SEQUENTIAL.                             AST
001200                                                                  AST
001300 DATA DIVISION.                                                   AST
001400                                                                  AST
001500 FILE SECTION.                                                    AST
001600 FD INPUT-FILE.                                                   AST
001700 01 INPUT-RECORD PIC X(80).                                       AST
001800                                                                  AST
001900                                                                  AST
002000 WORKING-STORAGE SECTION.                                         AST
002300*> USED BY PROCESS-COMMAND-ARGS.CBL AS WELL AS APPS THAT          AST
002400 *> CALL PROCESS-COMMAND-ARGS.                                    AST
002500                                                                  AST
002600 COPY "param-group.cpy".                                          AST
002900                                                                  AST
003000                                                                  AST
003400                                                                  AST
003500 01 WORK-VARS.                                                    AST
003600     05 FLAG-1 PIC X VALUE 'N'.                                   AST
003700         88 FLAG-1-VALID VALUE 'Y'.                               AST
003900     05 FLAG-2 PIC X VALUE 'Y'.                                   AST
004000         88 FLAG-2-VALID VALUE 'Y'.                               AST
           05 FLAG-3 PIC X VALUE 'N'.                                   AST
           05 FLAG-4 PIC X VALUE 'Y'.                                   AST
           05 FLAG-5 PIC X VALUE 'N'.                                   AST
004100     05 FLAG-6 PIC X VALUE 'Y'.                                   AST
004200     05 CHANGE-PERCENT PIC ZZ9.99- VALUE SPACE.                   AST
004500                                                                  DECAST
004400     05 IDX-1 PIC 9(3).                                           AST
004600     05 MSG PIC X(80).                                            AST
004700     05 EOF-SWITCH PIC X VALUE 'N'.                               AST
004800         88 INPUT-FILE-EOF VALUE 'Y' FALSE 'N'.                   AST
004900                                                                  AST
005000                                                                  AST
005100                                                                  AST
005200 PROCEDURE DIVISION.                                              AST
005400                                                                  AST
005500 MAIN.                                                            AST
005600     CALL 'process-command-args' USING PARAM-GROUP                AST
005700         ON EXCEPTION                                             AST
005800             DISPLAY 'the SUBROUTINE CALL TO process-command-args AST
005900-                ' FAILED!'                                       AST
           END-CALL
006000                                                                  AST
006100     PERFORM DO-STUFF                                             AST
006200     PERFORM DO-STUFF-2                                           AST
006300     PERFORM DO-STUFF-3                                           AST
006400     PERFORM DO-STUFF-4                                           AST
006600     PERFORM DO-STUFF-5                                           AST
006700     PERFORM DO-STUFF-6                                           AST
006800     PERFORM DO-STUFF-7                                           AST
006900     PERFORM DO-STUFF-8                                           AST
007000     PERFORM DO-STUFF-9                                           AST
007100                                                                  AST
007200     PERFORM DO-STUFF-10                                          AST
007300                                                                  AST
007400                                                                  DECAST
007500                                                                  DECAST
007600 GOBACK                                                           DECAST
           . *> END MAIN
007700                                                                  AST
007800                                                                  AST
007900 *> test handling of 'if', 'else', 'then'                         AST
008000 DO-STUFF.                                                        DECAST
008100                                                                  DECAST
           *> t1: test if statement by itself                           DECAST
           IF FLAG-1 = 'N'                                              AST
008200         DISPLAY 't1: flag-1 is false'                            AST
           END-IF *> FLAG-1 = 'N'
008300                                                                  AST
008400     *> t2: test if with a two part expression                    AST
           IF FLAG-1 = 'Y' OR FLAG-2 = 'Y'                              AST
008600         DISPLAY 't2: at least one of two flags is ''Y'''         AST
           END-IF *> FLAG-1 = 'Y' OR FLAG-2 = 'Y'
                                                                        AST
           *> t3: test if with else clause                              AST
           IF FLAG-1 = 'N'                                              AST
               DISPLAY 't3: flag-1 is false'                            AST
           ELSE                                                         AST
               DISPLAY 't3: flag-2 is true'                             AST
           END-IF *> FLAG-1 = 'N'
                                                                        AST
           *> t4: test if with 'then'                                   AST
           IF FLAG-1 = 'N'                                              AST
           THEN                                                         AST
               DISPLAY 't4: flag-1 is false'                            AST
           END-IF *> FLAG-1 = 'N'
                                                                        AST
           *> t5: test if with a matching end-if                        AST
           IF FLAG-1 = 'N'                                              DECAST
           THEN                                                         DECAST
               DISPLAY 't5: flag-1 is false'                            DECAST
           END-IF *> : FLAG-1 = 'N'                                     AST
                                                                        AST
           *> t6: test if with multiple statements within it.           AST
           IF FLAG-1 = 'N'                                              DECAST
               DISPLAY 't6: hello world'                                DECAST
               DISPLAY '  t6: hello land'                               DECAST
               DISPLAY '  t6: hello mellow'                             AST
           END-IF *> FLAG-1 = 'N'
                                                                        AST
           *> t7: test if-else with multiple statements w/i.            AST
           IF FLAG-1 = 'N'                                              AST
               DISPLAY 't7: when in the sessions'                       AST
               DISPLAY '  t7: of sweet silent thoughts,'                AST
               DISPLAY '  t7: I summon up remembrance'                  AST
           ELSE                                                         AST
               DISPLAY '  t7: of things past.'                          AST
               DISPLAY '  t7: I sight the lack of many a thing'         AST
           END-IF *> FLAG-1 = 'N'
                                                                        AST
                                                                        AST
           *> t8: test if-then with multiple statements within it.      AST
           IF FLAG-1 = 'N'                                              AST
           THEN                                                         AST
               DISPLAY 't8: hello world'                                AST
               DISPLAY '   t8: hello land'                              AST
               DISPLAY '  t8: hello mellow'                             AST
           END-IF *> FLAG-1 = 'N'
                                                                        AST
           *> t9: test if-then-else with multiple statements w/i.       DECAST
           IF FLAG-1 = 'N'                                              DECAST
           THEN                                                         DECAST
               DISPLAY '       t9: when in the sessions'                AST
               DISPLAY '  t9: of sweet silent thoughts,'                AST
               DISPLAY '  t9: I summon up remembrance'                  AST
           ELSE                                                         DECAST
               DISPLAY '  t9: of things past.'                          DECAST
               DISPLAY '  t9: I sigh the lack of many a thing'          DECAST
           END-IF *> FLAG-1 = 'N'
                                                                        AST
           *> t10: test 1 line if statement                             AST
           IF FLAG-6 = 'Y' DISPLAY 't10: and with old woes'             AST
           END-IF *> FLAG-6 = 'Y' DISPLAY 't10: and with old woes'
                                                                        AST
           *> t10.1: test 1 line if-ele statement                       AST
           IF FLAG-6 = 'N' DISPLAY "t10.1" ELSE DISPLAY 't10.1: heavy'  AST
           END-IF *> FLAG-6 = 'N' DISPLAY "t10.1" ELSE DISPLAY 't10.1: heavy'
           . *> END DO-STUFF
                                                                        AST

       STUFF-SET-1 SECTION.
       *> test already terminated if end-if pairs.                      AST
       DO-STUFF-2.                                                      AST
                                                                        AST
           *> t11: test if - end-if statement by itself                 AST
           IF FLAG-1 = 'N'                                              AST
               DISPLAY 't11: flag-1 is false'                           AST
           END-IF *> : FLAG-1 = 'N'                                     AST
                                                                        AST
           *> t12: test if - end-if  with a two part expression
           IF FLAG-1 = 'Y' OR FLAG-2 = 'Y'
               DISPLAY 't12: at least one of two flags is ''Y'''
           END-IF *> : FLAG-1 = 'Y' OR FLAG-2 = 'Y'
                                                                        AST
           *> t13: test if end-if with else clause                      AST
           IF FLAG-1 = 'N'                                              AST
               DISPLAY 't13: flag-1 is false'
           ELSE
               DISPLAY 't13: flag-2 is true'
           END-IF *> : FLAG-1 = 'N'                                     AST
                                                                        AST
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
           IF FLAG-1 = 'N'                                              AST
               DISPLAY 't17: when in the sessions'                      AST
               DISPLAY '  t7: of sweet silent thoughts,'                AST
               DISPLAY '  t7: I summon up remembrance'
           ELSE
               DISPLAY '  t7: of things past.'
               DISPLAY '  t7: I sight the lack of many a thing'         AST
           END-IF *> : FLAG-1 = 'N'                                     AST
                                                                        AST
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

                                                                        AST
       DO-STUFF-3.                                                      AST
                                                                        AST
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
               DISPLAY 't21: hey, nonino'                               AST
               DISPLAY 't21: that over the cornfield did pass.'         AST
           ELSE                                                         AST
               DISPLAY 't21: in the springtime'
               DISPLAY 't21: the pretty little ringtime'
               DISPLAY 't21: yada yada yada'
           END-IF *> FLAG-5 = 'Y' OR FLAG-6 = 'Y'
           END-IF *> NOT FLAG-4 = 'Y'
           END-IF *> FLAG-3 = 'Y'
           END-IF *> FLAG-2-VALID
           END-IF *> NOT FLAG-1-VALID
           DISPLAY 't21: shall i compare thee to a summer''s day?'      AST
           DISPLAY 't21: thou art more lovely and more temperate.'      AST
           DISPLAY 't21: rough winds do shake the darling buds of may.' AST
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
                                                                        AST
           *> t24: nested if then else's.                               AST
           IF FLAG-1-VALID                                              AST
               IF NOT FLAG-2-VALID
                   IF FLAG-3 = 'Y'
                       DISPLAY 't24: when forty winters shall besiege'
                       DISPLAY 't24: thy brow and dig deep trenches'    AST
                   ELSE                                                 AST
                       DISPLAY 't24: in thy beauty''s field, '          AST
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
                       ELSE IF FLAG-2-VALID                             AST
                          DISPLAY 't25: a quick brown fox'              AST
                          DISPLAY 't25: jumped over the lazy dog'       AST
                          DISPLAY 't25: and up a small hill'
                       ELSE IF FLAG-3 = 'Y'
                          DISPLAY 't25: a thing of beauty is'
                          DISPLAY 't25: a joy forever'                  AST
                          DISPLAY 't25: it''s loveliness increases.'    AST
                       ELSE IF NOT FLAG-4 = 'Y'                         AST
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
                       END-IF *> : FLAG-5 = 'Y' OR FLAG-6 = 'Y'         AST
                       END-IF *> : NOT FLAG-4 = 'Y'
                       END-IF *> : FLAG-3 = 'Y'
                       END-IF *> : FLAG-2-VALID
                       END-IF *> : NOT FLAG-1-VALID                     AST
                       DISPLAY 't25: shall i compare thee to a summer''sAST
      -                     'day?'                                      AST
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
           DISPLAY 't26: thou art more lovely and more temperate'       AST
           . *> single solitary dot                                     AST
                                                                        AST
       DO-STUFF-6.
           *> t27: test single dot on a line by itself with no
           *> comments on same line.
           DISPLAY 't27: rough winds do shake the darling buds of may'  AST
           . *> : END DO-STUFF-6                                        AST
                                                                        AST
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
                                                                        AST
           MOVE 't31' TO MSG                                            AST
           PERFORM MORE-STUFF                                           AST
           WITH TEST AFTER
           UNTIL IDX-1 > 3

           MOVE 't32' TO MSG                                            AST
           PERFORM MORE-STUFF VARYING IDX-1 FROM 1 BY 1 UNTIL IDX-1 = 4 AST
                                                                        AST
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
                                                                        AST
           *> t41                                                       AST
           PERFORM VARYING IDX-1 FROM 1 BY 1
           UNTIL IDX-1 > 4
                 DISPLAY 't41: look in thy glass and tell the face'
           END-PERFORM                                                  AST
                                                                        AST
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
           PERFORM                                                      AST
           UNTIL IDX-1 > 3                                              AST
               DISPLAY "t44: for where is she so fair whose uneared"    AST
               ADD 1 TO IDX-1
           END-PERFORM

           *> t45                                                       AST
           MOVE 1 TO IDX-1                                              AST
           PERFORM                                                      AST
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
                                                                        AST
                                                                        AST
           *> t47: test case where the end terminator is already presentAST
           OPEN INPUT INPUT-FILE
           PERFORM UNTIL INPUT-FILE-EOF
               READ INPUT-FILE
                   AT END                                               AST
                       SET INPUT-FILE-EOF TO TRUE                       AST
                   NOT AT END                                           AST
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
      *
      *
           DISPLAY 'test:' MSG
           . *> END MORE-STUFF
