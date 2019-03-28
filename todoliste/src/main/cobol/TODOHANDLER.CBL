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
           WHEN ACTION-SORTUSER
             PERFORM SORT-ITEM-BY-USER
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

       ADD-TODOLIST-ENTRY SECTION.
           MOVE "content" TO PARAMETER-NAME
      *     DISPLAY PARAMETER-NAME UPON SYSERR
           PERFORM READ-PARAMETER-VALUE
      *     DISPLAY PARAMETER-VALUE UPON SYSERR

           MOVE PARAMETER-VALUE TO NEW-TODO-ITEM
           MOVE "user" TO PARAMETER-NAME
           PERFORM READ-PARAMETER-VALUE
           MOVE PARAMETER-VALUE TO ACTIVE-USER


           CALL "TODOLISTE" USING TODO-ACTION
                                  ACTIVE-USER
                                  NEW-TODO-ITEM
                                  0
          EXIT.

       DELETE-ITEM SECTION.
          MOVE "pos" TO PARAMETER-NAME
          PERFORM READ-PARAMETER-VALUE
          CALL "TODOLISTE" USING TODO-ACTION
                                 ACTIVE-USER
                                 0
                                 PARAMETER-VALUE
          EXIT.

       SHOW-TODOLIST SECTION.
          CALL "TODOLISTE" USING TODO-ACTION
          EXIT.

       DELETE-TODOLIST SECTION.
          CALL "TODOLISTE" USING TODO-ACTION
          EXIT.

       SORT-ITEM-BY-USER SECTION.
          CALL "TODOLISTE" USING TODO-ACTION
          EXIT.

       END PROGRAM TODOHANDLER.
