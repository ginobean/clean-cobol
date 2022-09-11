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
002600 copy "param-group.cpy".                                          AST
002900                                                                  AST
003000                                                                  AST
003400                                                                  AST
003500 01 WORK-VARS.                                                    AST
003600     05 flag-1 pic x value 'N'.                                   AST
003700         88 flag-1-valid value 'Y'.                               AST
003900     05 flag-2 pic x value 'Y'.                                   AST
004000         88 flag-2-valid value 'Y'.                               AST
           05 flag-3 pic x value 'N'.                                   AST
           05 flag-4 pic x value 'Y'.                                   AST
           05 flag-5 pic x value 'N'.                                   AST
004100     05 flag-6 pic x value 'Y'.                                   AST
004200     05 change-percent pic zz9.99- value space.                   AST
004500                                                                  DECAST
004400     05 idx-1 pic 9(3).                                           AST
004600     05 msg pic x(80).                                            AST
004700     05 eof-switch pic x value 'N'.                               AST
004800         88 input-file-eof value 'Y' false 'N'.                   AST
004900                                                                  AST
005000                                                                  AST
005100                                                                  AST
005200 PROCEDURE DIVISION.                                              AST
005400                                                                  AST
005500 MAIN.                                                            AST
005600     call 'process-command-args' using PARAM-GROUP                AST
005700         ON EXCEPTION                                             AST
005800             DISPLAY 'the SUBROUTINE CALL TO process-command-args AST
005900-                ' FAILED!'                                       AST
006000                                                                  AST
006100     perform do-stuff.                                            AST
006200     perform do-stuff-2.                                          AST
006300     perform do-stuff-3.                                          AST
006400     perform do-stuff-4.                                          AST
006600     perform do-stuff-5.                                          AST
006700     perform do-stuff-6.                                          AST
006800     perform do-stuff-7.                                          AST
006900     perform do-stuff-8.                                          AST
007000     perform do-stuff-9.                                          AST
007100                                                                  AST
007200     perform do-stuff-10.                                         AST
007300                                                                  AST
007400                                                                  DECAST
007500                                                                  DECAST
007600 GOBACK.                                                          DECAST
007700                                                                  AST
007800                                                                  AST
007900 *> test handling of 'if', 'else', 'then'                         AST
008000 DO-STUFF.                                                        DECAST
008100                                                                  DECAST
           *> t1: test if statement by itself                           DECAST
           if flag-1 = 'N'                                              AST
008200         display 't1: flag-1 is false'.                           AST
008300                                                                  AST
008400     *> t2: test if with a two part expression                    AST
           if flag-1 = 'Y' or flag-2 = 'Y'                              AST
008600         display 't2: at least one of two flags is ''Y'''.        AST
                                                                        AST
           *> t3: test if with else clause                              AST
           if flag-1 = 'N'                                              AST
               display 't3: flag-1 is false'                            AST
           else                                                         AST
               display 't3: flag-2 is true'.                            AST
                                                                        AST
           *> t4: test if with 'then'                                   AST
           if flag-1 = 'N'                                              AST
           then                                                         AST
               display 't4: flag-1 is false'.                           AST
                                                                        AST
           *> t5: test if with a matching end-if                        AST
           if flag-1 = 'N'                                              DECAST
           then                                                         DECAST
               display 't5: flag-1 is false'                            DECAST
           end-if.                                                      AST
                                                                        AST
           *> t6: test if with multiple statements within it.           AST
           if flag-1 = 'N'                                              DECAST
               display 't6: hello world'                                DECAST
               display '  t6: hello land'                               DECAST
               display '  t6: hello mellow'.                            AST
                                                                        AST
           *> t7: test if-else with multiple statements w/i.            AST
           if flag-1 = 'N'                                              AST
               display 't7: when in the sessions'                       AST
               display '  t7: of sweet silent thoughts,'                AST
               display '  t7: I summon up remembrance'                  AST
           else                                                         AST
               display '  t7: of things past.'                          AST
               display '  t7: I sight the lack of many a thing'.        AST
                                                                        AST
                                                                        AST
           *> t8: test if-then with multiple statements within it.      AST
           if flag-1 = 'N'                                              AST
           then                                                         AST
               display 't8: hello world'                                AST
               display '   t8: hello land'                              AST
               display '  t8: hello mellow'.                            AST
                                                                        AST
           *> t9: test if-then-else with multiple statements w/i.       DECAST
           if flag-1 = 'N'                                              DECAST
           then                                                         DECAST
               display '       t9: when in the sessions'                AST
               display '  t9: of sweet silent thoughts,'                AST
               display '  t9: I summon up remembrance'                  AST
           else                                                         DECAST
               display '  t9: of things past.'                          DECAST
               display '  t9: I sigh the lack of many a thing'.         DECAST
                                                                        AST
           *> t10: test 1 line if statement                             AST
           if flag-6 = 'Y' display 't10: and with old woes'.            AST
                                                                        AST
           *> t10.1: test 1 line if-ele statement                       AST
           if flag-6 = 'N' display "t10.1" else display 't10.1: heavy'. AST
                                                                        AST

       stuff-set-1 section.
       *> test already terminated if end-if pairs.                      AST
       do-stuff-2.                                                      AST
                                                                        AST
           *> t11: test if - end-if statement by itself                 AST
           if flag-1 = 'N'                                              AST
               display 't11: flag-1 is false'                           AST
           end-if                                                       AST
                                                                        AST
           *> t12: test if - end-if  with a two part expression
           if flag-1 = 'Y' or flag-2 = 'Y'
               display 't12: at least one of two flags is ''Y'''
           end-if
                                                                        AST
           *> t13: test if end-if with else clause                      AST
           if flag-1 = 'N'                                              AST
               display 't13: flag-1 is false'
           else
               display 't13: flag-2 is true'
           end-if                                                       AST
                                                                        AST
           *> t14: test if end-if with 'then'                           AST
           if flag-1 = 'N'
           then
               display 't14: flag-1 is false'
           end-if

           *> t15: test if end-if with a matching end-if
           if flag-1 = 'N'
           then
               display 't15: flag-1 is false'
           end-if.

           *> t16: test if end-if with multiple statements within it.
           if flag-1 = 'N'
               display 't16: hello world'
               display '  t6: hello land'
               display '  t6: hello mellow'
           end-if

           *> t17: test if-else end-if with multiple statements w/i.
           if flag-1 = 'N'                                              AST
               display 't17: when in the sessions'                      AST
               display '  t7: of sweet silent thoughts,'                AST
               display '  t7: I summon up remembrance'
           else
               display '  t7: of things past.'
               display '  t7: I sight the lack of many a thing'         AST
           end-if                                                       AST
                                                                        AST
           *> t18: test if-then end-if with multiple statements within it.
           if flag-1 = 'N'
           then
               display 't18: hello world'
               display '   t8: hello land'
               display '  t8: hello mellow'
           end-if

           *> t19: test if-then-else end-if with multiple statements w/i.
           if flag-1 = 'N'
           then
               display '       t19: when in the sessions'
               display '  t19: of sweet silent thoughts,'
               display '  t19: I summon up remembrance'
           else
               display '  t19: of things past.'
               display '  t19: I sigh the lack of many a thing'
           end-if.

           *> t20: start token and end terminator are on same line.
           if flag-5 = 'N' display 't20: and with' end-if.

                                                                        AST
       do-stuff-3.                                                      AST
                                                                        AST
           *> t21: linear nested if's.
           if not flag-1-valid
               display 't21: now is the winter'
               display 't21: of our discontent.'
           else if flag-2-valid
               display 't21: a quick brown fox'
               display 't21: jumped over the lazy dog'
               display 't21: and up a small hill'
           else if flag-3 = 'Y'
               display 't21: a thing of beauty is'
               display 't21: a joy forever'
               display 't21: it''s loveliness increases.'
           else if not flag-4 = 'Y'
               display 't21: it will never pass into nothingness'
               display 't21: but still will keep a bower'
               display 't21: quiet and safe for us.'
           else if flag-5 = 'Y' or flag-6 = 'Y'
               display 't21: it was a lover and his lass'
               display 't21: with a hey and ho and a '
               display 't21: hey, nonino'                               AST
               display 't21: that over the cornfield did pass.'         AST
           else                                                         AST
               display 't21: in the springtime'
               display 't21: the pretty little ringtime'
               display 't21: yada yada yada'.
           display 't21: shall i compare thee to a summer''s day?'      AST
           display 't21: thou art more lovely and more temperate.'      AST
           display 't21: rough winds do shake the darling buds of may.'.AST


       do-stuff-4.

           *> t22: nested if's, with a continuation line.
           if not flag-1-valid
               if flag-2-valid
                   if flag-3 = 'N'
                       display 't22: from fairest creatures we desire in
      -    'crease. that thereby beauty''s rose might never die'.


           *> t23: nested if's, with multiple continuation lines.
           if not flag-1-valid
               if flag-2-valid
                   if flag-3 = 'N'
                       display 't23: but, as the riper should by time de
      -    'decrease, His tender heir might bear his memory But thou, co
      -       'ntracted to thine own bright eyes..'.
                                                                        AST
           *> t24: nested if then else's.                               AST
           if flag-1-valid                                              AST
               if not flag-2-valid
                   if flag-3 = 'Y'
                       display 't24: when forty winters shall besiege'
                       display 't24: thy brow and dig deep trenches'    AST
                   else                                                 AST
                       display 't24: in thy beauty''s field, '          AST
                       display "t24: thy youth's proud livery, so gazed"
                       display 't24: on now, will be a tatter''d'
                       display 't24: weed of small worth held'
               else
                   display 't24: look in thy glass'
           else
               display 't24: and test the face thou viewest'
               display "now is the time that face should form another".

           *> t25: nested if's with some linear nested if's inside
           if flag-1-valid or flag-2-valid
               if flag-3 = 'N'
                   if flag-4 = 'Y'
                       display 't25: when forty winters shall besiege'
                       display 't25: thy brow and dig deep trenches'

                       if not flag-1-valid
                          display '    t25: now is the winter'
                          display 't25: of our discontent.'
                       else if flag-2-valid                             AST
                          display 't25: a quick brown fox'              AST
                          display 't25: jumped over the lazy dog'       AST
                          display 't25: and up a small hill'
                       else if flag-3 = 'Y'
                          display 't25: a thing of beauty is'
                          display 't25: a joy forever'                  AST
                          display 't25: it''s loveliness increases.'    AST
                       else if not flag-4 = 'Y'                         AST
                          display 't25: it will never pass into nothingn
      -                        'ess'
                          display 't25: but still will keep a bower'
                          display 't25: quiet and safe for us.'
                       else if flag-5 = 'Y' or flag-6 = 'Y'
                          display 't25: it was a lover and his lass'
                          display 't25: with a hey and ho and a '
                          display 't25: hey, nonino'
                          display 't25: that over the cornfield did pass
      -                        '.'
                       else
                          display 't25: in the springtime'
                          display 't25: the pretty little ringtime'
                          display 't25: yada yada yada'

                       *> the COBOL compiler itself doesn't notice the
                       *> indentation levels, so these end-if's are
                       *> needed here to correlate with the intentions
                       *> manifested by the indent levels, so that
                       *> we get the SAME output results before and     AST
                       *> after filtering through clean-cobol-source.py AST
                       end-if                                           AST
                       end-if
                       end-if
                       end-if
                       end-if                                           AST
                       display 't25: shall i compare thee to a summer''sAST
      -                     'day?'                                      AST
                       display 't25: thou art more lovely and more tempe
      -                    'rate.'
                       display 't25: rough winds do shake the darling bu
      -                    'ds of may.'
                   else
                       display 't25: in thy beauty''s field, '
                       display "t25: thy youth's proud livery, so gazed"
                       display 't25: on now, will be a tatter''d'
                       display 't25: weed of small worth held'
               else
                   display 't25: look in thy glass'
           else
               display 't25: and test the face thou viewest'
               display "t25: now is the time that face should form.".

       stuff-set-2 section.

       do-stuff-5.
           *> t26: test case where paragraph already ends in a
           *> single dot on a line by itself
           display 't26: shall i compare thee to a summer''s day?'
           display 't26: thou art more lovely and more temperate'       AST
           . *> single solitary dot                                     AST
                                                                        AST
       do-stuff-6.
           *> t27: test single dot on a line by itself with no
           *> comments on same line.
           display 't27: rough winds do shake the darling buds of may'  AST
           .                                                            AST
                                                                        AST
       do-stuff-7.
           *> t28: test that dots within are not removed.
           move 123.45 to change-percent.
           move 987.61 to change-percent.

       *> non-inline performs. should not be terminated by
       *> end-perform, as GnuCOBOL 3.x will reject it.
       do-stuff-8.

           *> t29
           move 't29' to msg .
           perform more-stuff
           varying idx-1 from 1 by 1
           until idx-1 = 4.

           move 't30' to msg.
           perform more-stuff
           with test before
           until idx-1 > 3.
                                                                        AST
           move 't31' to msg.                                           AST
           perform more-stuff                                           AST
           with test after
           until idx-1 > 3.

           move 't32' to msg.                                           AST
           perform more-stuff varying idx-1 from 1 by 1 until idx-1 = 4.AST
                                                                        AST
           move 't33' to msg.
           perform more-stuff with test before until idx-1 > 3.

           move 't34' to msg.
           perform more-stuff with test after until idx-1 > 3.

       *> inline performs
       do-stuff-9.

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
           perform varying idx-1 from 1 by 1
           until idx-1 > 4
                 display 't41: look in thy glass and tell the face'
           end-perform.                                                 AST
                                                                        AST
           *> t42                                                       AST
           perform varying idx-1 from 1 by 1
           until idx-1 > 4
                 display 't42: thou viewest'
                 display 't42: now is the time that face should form'
                 display 't42: another.'
           end-perform.

           *> t43
           perform
           with test after
           varying idx-1 from 1 by 1
           until idx-1 > 3
                 display 't43: whose fresh repair if now thou not'
                 display 't43: renewest. thou dost beguild the world'
                 display 't43: unbless some mother.'
           end-perform

           *> t44
           move 1 to idx-1.
           perform                                                      AST
           until idx-1 > 3                                              AST
               display "t44: for where is she so fair whose uneared"    AST
               add 1 to idx-1
           end-perform

           *> t45                                                       AST
           move 1 to idx-1.                                             AST
           perform                                                      AST
           with test after
           until idx-1 > 4
               display "t45: womb disdains the tillage"
               display "t45: of thy husbandry?"
               compute idx-1 = idx-1 + 1
           end-perform.

           *> t46 : read/write/rewrite/start/search/call all
           *> have extremely similar patterns.
           open input input-file.
           perform until input-file-eof
               read input-file
                   at end
                       set input-file-eof to true
                   not at end
                       display 't46: ' input-record
           end-perform.

           close input-file.
                                                                        AST
                                                                        AST
           *> t47: test case where the end terminator is already presentAST
           open input input-file.
           perform until input-file-eof
               read input-file
                   at end                                               AST
                       set input-file-eof to true                       AST
                   not at end                                           AST
                       display 't47: ' input-record
                       continue *> no-op statement.
               end-read
           end-perform.

           close input-file.



       do-stuff-10.

           evaluate true
               when flag-3 = 'N'
                   display 't51: shall i compare thee to a summer''s'
                   display 't51: day? thou art more lovely and more '
               when flag-4 = 'Y'
                   display 't51: temperate'.

           display 't51: rough winds do shake the darling buds of may'.

           *> t52
           evaluate true
               when flag-3 = 'Y'
                   display 't52: shall i compare thee to a summer''s'
                   display 't52: day? thou art more lovely and more '
               when flag-4 = 'Y'
                   display 't52: temperate'
           end-evaluate.

           display 't52: rough winds do shake the darling buds of may'.

           if flag-1
           >= 'M'
               display 't53: test conditional operator handling'
           display 't53: after conditional.'.


       more-stuff section.
           *> t53 -- '*' comments should be passed through, unmodified.
      * example of an '*' comment.
      * yet another '*' comment.

           *> t54 - these next two '*' comments should be filtered out.
      *
      *
           display 'test:' msg.
