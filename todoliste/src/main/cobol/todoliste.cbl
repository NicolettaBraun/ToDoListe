       IDENTIFICATION DIVISION.
       PROGRAM-ID. TODOLISTE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
         FILE-CONTROL.
         SELECT TDLIST ASSIGN TO 'todolist.txt'
            ORGANIZATION IS LINE SEQUENTIAL.
         SELECT PRINT-FILE ASSIGN TO 'todolist.txt'
            ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
          FD TDLIST.
          01 TDLIST-FILE.
             05 ITEM-ID PIC 9(5).
             05 ITEM-CONTENT PIC X(35).
       WORKING-STORAGE SECTION.
       01 TEMP-FIELDS.
        05  ITEM-TO-DELETE          PIC 999 VALUE 1.
        05  NUMBER-OF-TODOS         PIC 999 VALUE 0.
           88 LIST-IS-EMPTY VALUE ZERO.
        05  COUNTER                 PIC 999.
        05  TODOLISTE.
            07  TODO-ITEM PIC X(35) OCCURS 999.
       01 WS-EOF PIC A(1).
       01 WS-TDLIST.
        05 ITEM-ID PIC X(5).
        05 ITEM-CONTENT PIC X(35).
       LINKAGE SECTION.
        COPY todoactions.
        COPY todoitem.
       PROCEDURE DIVISION USING TODO-ACTION NEW-TODO-ITEM.
           DISPLAY "Taste gedruckt" UPON SYSERR
           EVALUATE TRUE
           WHEN ACTION-SHOW
              PERFORM READ-TODOLIST-FROM-FILE
           WHEN ACTION-ADD
              PERFORM ADD-NEW-TODO-ITEM
           WHEN ACTION-DELETE
              PERFORM CLEAR-LIST
           WHEN ACTION-DELETEITEM
              PERFORM DELETE-ITEM
           END-EVALUATE
           GOBACK
          .

       ADD-NEW-TODO-ITEM SECTION.
           DISPLAY NUMBER-OF-TODOS " ToDos" UPON SYSERR

           MOVE NEW-TODO-ITEM
             TO TODO-ITEM (NUMBER-OF-TODOS)

           MOVE NUMBER-OF-TODOS
             TO ITEM-ID IN WS-TDLIST
           MOVE NEW-TODO-ITEM
             TO ITEM-CONTENT IN WS-TDLIST

           OPEN EXTEND TDLIST
           WRITE TDLIST-FILE FROM WS-TDLIST
           CLOSE TDLIST

          EXIT.

       CLEAR-LIST SECTION.
          OPEN OUTPUT TDLIST
          CLOSE TDLIST
          EXIT.

       DISPLAY-ITEM SECTION.
          DISPLAY "<li>" ITEM-CONTENT IN WS-TDLIST "</li>"
          EXIT.

       DELETE-ITEM SECTION.
           PERFORM READ-TODOLIST-FROM-FILE
           MOVE SPACES TO TODO-ITEM (ITEM-TO-DELETE)
           ADD 1 TO ITEM-TO-DELETE
           PERFORM READ-TODOLIST-FROM-FILE
      *     PERFORM WITH TEST AFTER
      *       VARYING COUNTER FROM ITEM-TO-DELETE BY 1 UNTIL
      *            COUNTER = NUMBER-OF-TODOS
      *          MOVE TODO-ITEM(COUNTER)
      *            TO TODO-ITEM(COUNTER - 1)
      *     END-PERFORM
           OPEN OUTPUT TDLIST
           MOVE 0 TO COUNTER
           DISPLAY "Die Methode wird aufgerufen"
           DISPLAY NUMBER-OF-TODOS
           PERFORM NUMBER-OF-TODOS TIMES
               ADD 1 TO COUNTER
               WRITE TDLIST-FILE FROM TODO-ITEM(COUNTER)
               DISPLAY "Die Schleife läuft"
           END-PERFORM
           CLOSE TDLIST
          EXIT.

       READ-TODOLIST-FROM-FILE SECTION.
             OPEN INPUT TDLIST
             DISPLAY "<ul>"
             PERFORM UNTIL WS-EOF='Y'
                 READ TDLIST INTO WS-TDLIST
                    AT END MOVE 'Y' TO WS-EOF
                    NOT AT END PERFORM DISPLAY-ITEM
                       ADD 1 TO NUMBER-OF-TODOS
                 END-READ
             END-PERFORM
             DISPLAY "</ul>"
             CLOSE TDLIST
          EXIT.

       END PROGRAM TODOLISTE.
