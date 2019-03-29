       IDENTIFICATION DIVISION.
       PROGRAM-ID. TODOHANDLER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WEBINPUT ASSIGN TO KEYBOARD
           FILE STATUS IS IN-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD WEBINPUT.
          01 CHUNK-OF-POST     PIC X(1024).

       WORKING-STORAGE SECTION.
       01 IN-STATUS            PIC 9999.
       01 NEWLINE              PIC X     VALUE X'0A'.
       01 DATE-FORMAT.
          05 DATE-YEAR PIC 9999.
          05 FILLER PIC X VALUE "-".
          05 DATE-MONTH PIC 99.
          05 FILLER PIC X VALUE "-".
          05 DATE-DAY PIC 99.
          05 FILLER PIC X VALUE "-".

       COPY TODOACTIONS.
       COPY CGIUTILDATA.
       COPY TODOITEM.

       PROCEDURE DIVISION.
           PERFORM U01-PRINT-HEADER

           PERFORM U00-PARSE-WEBINPUT

           PERFORM READ-TODOLIST-ACTION

           EVALUATE TRUE
           WHEN ACTION-ADD
             PERFORM ADD-TODOLIST-ENTRY
           WHEN ACTION-SHOW
             PERFORM SHOW-TODOLIST
           WHEN ACTION-DELETE
             PERFORM DELETE-TODOLIST
           WHEN ACTION-DELETEITEM
             PERFORM DELETE-ITEM
           WHEN ACTION-SORT
             PERFORM SORT-ITEM
           WHEN ACTION-modify
             PERFORM MODIFY-ITEM
           WHEN OTHER
             DISPLAY "Action unbekannt: " TODO-ACTION
           END-EVALUATE
           GOBACK
          .

       U00-PARSE-WEBINPUT SECTION.
           OPEN INPUT WEBINPUT
           IF IN-STATUS < 10 THEN
            READ WEBINPUT END-READ
            IF IN-STATUS > 9 THEN
              MOVE SPACES TO CHUNK-OF-POST
            END-IF
           END-IF
           CLOSE WEBINPUT
           MOVE CHUNK-OF-POST TO REQUEST-STRING
          .

       U01-PRINT-HEADER SECTION.
           DISPLAY "CONTENT-TYPE: TEXT/HTML"
           NEWLINE
          .

       READ-PARAMETER-VALUE SECTION.
           MOVE CHUNK-OF-POST TO REQUEST-STRING
           CALL "CGIUTIL" USING REQUEST-PARAMETERS
          EXIT.

       READ-TODOLIST-ACTION SECTION.
           MOVE "action" TO PARAMETER-NAME
           PERFORM READ-PARAMETER-VALUE
           MOVE PARAMETER-VALUE TO TODO-ACTION
          EXIT.

       READ-ITEM SECTION.
         MOVE "content" TO PARAMETER-NAME
         PERFORM READ-PARAMETER-VALUE
         MOVE PARAMETER-VALUE TO DESCRIPTION

         MOVE "user" TO PARAMETER-NAME
         PERFORM READ-PARAMETER-VALUE
         MOVE PARAMETER-VALUE TO USER

         MOVE "prio" TO PARAMETER-NAME
         PERFORM READ-PARAMETER-VALUE
         MOVE PARAMETER-VALUE TO PRIO

         MOVE "duedate" TO PARAMETER-NAME
         PERFORM READ-PARAMETER-VALUE
         DISPLAY PARAMETER-VALUE UPON SYSERR
         MOVE PARAMETER-VALUE TO DATE-FORMAT
         MOVE DATE-YEAR TO DUE-YEAR
         MOVE DATE-MONTH TO DUE-MONTH
         MOVE DATE-DAY TO DUE-DAY
         EXIT.


       ADD-TODOLIST-ENTRY SECTION.
          PERFORM READ-ITEM
           MOVE FUNCTION CURRENT-DATE TO ADDED


           CALL "TODOLISTE" USING TODO-ACTION
                                  TD-ITEM
                                  ITEM-TO-DELETE
                                  WHICH-KIND-SORT
          EXIT.

       READ-POS SECTION.
          MOVE "pos" TO PARAMETER-NAME
          PERFORM READ-PARAMETER-VALUE
          MOVE PARAMETER-VALUE TO ITEM-TO-DELETE
          EXIT.

       DELETE-ITEM SECTION.
          PERFORM READ-POS
          CALL "TODOLISTE" USING TODO-ACTION
                                 TD-ITEM
                                 ITEM-TO-DELETE
                                 WHICH-KIND-SORT

          EXIT.

       SHOW-TODOLIST SECTION.
          CALL "TODOLISTE" USING TODO-ACTION
          EXIT.

       DELETE-TODOLIST SECTION.
          CALL "TODOLISTE" USING TODO-ACTION
          EXIT.

       SORT-ITEM SECTION.
          MOVE "sorttype" TO PARAMETER-NAME
          PERFORM READ-PARAMETER-VALUE
          DISPLAY PARAMETER-VALUE UPON SYSERR
          MOVE PARAMETER-VALUE TO WHICH-KIND-SORT
          CALL "TODOLISTE" USING TODO-ACTION
                                 TD-ITEM
                                 ITEM-TO-DELETE
                                 WHICH-KIND-SORT
          EXIT.

       MODIFY-ITEM SECTION.
          PERFORM READ-ITEM
          PERFORM READ-POS
          CALL "TODOLISTE" USING TODO-ACTION
                                 TD-ITEM
                                 ITEM-TO-DELETE
                                 WHICH-KIND-SORT
          EXIT.


       END PROGRAM TODOHANDLER.
