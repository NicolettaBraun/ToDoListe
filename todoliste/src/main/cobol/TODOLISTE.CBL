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
        05  NUMBER-OF-TODOS         PIC 9(5) VALUE 0.
           88 LIST-IS-EMPTY VALUE ZERO.
        05  COUNTER                 PIC 9(5).
        05  TODOLISTE.
            07  TODO-ITEM PIC X(35) OCCURS 999.
       01 WS-EOF PIC A(1).
       01 WS-TDLIST.
        05 ITEM-ID PIC X(5).
        05 ITEM-CONTENT PIC X(35).
       01 WS-POSITION PIC 9(5).
       LINKAGE SECTION.
        COPY TODOACTIONS.
        COPY TODOITEM.
       PROCEDURE DIVISION USING TODO-ACTION NEW-TODO-ITEM
         ITEM-TO-DELETE.
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
         PERFORM READ-TODOLIST-FROM-FILE
         ADD 1 TO NUMBER-OF-TODOS
         MOVE NEW-TODO-ITEM TO TODO-ITEM (NUMBER-OF-TODOS)
         MOVE NUMBER-OF-TODOS TO ITEM-ID IN WS-TDLIST
         MOVE NEW-TODO-ITEM TO ITEM-CONTENT IN WS-TDLIST
         OPEN EXTEND TDLIST
         WRITE TDLIST-FILE FROM WS-TDLIST
         CLOSE TDLIST
         EXIT.

       CLEAR-LIST SECTION.
         OPEN OUTPUT TDLIST
         CLOSE TDLIST
         EXIT.

       DISPLAY-ITEM SECTION.
         DISPLAY "<li onClick='javascript:deleteItem("WS-POSITION")'"
         " id='" WS-POSITION
         "'>" 
         ITEM-CONTENT IN WS-TDLIST 
      *  "<input value='test' type='button'>"
         "</li>"
         EXIT.

       DELETE-ITEM SECTION.
         PERFORM READ-TODOLIST-FROM-FILE
         DISPLAY ITEM-TO-DELETE UPON SYSERR
         PERFORM VARYING COUNTER FROM ITEM-TO-DELETE BY 1 UNTIL
             COUNTER = NUMBER-OF-TODOS
               MOVE TODO-ITEM(COUNTER)
               TO TODO-ITEM(COUNTER - 1)
         END-PERFORM
         OPEN OUTPUT TDLIST
         SUBTRACT 1 FROM NUMBER-OF-TODOS GIVING NUMBER-OF-TODOS
         MOVE 0 TO COUNTER
         PERFORM NUMBER-OF-TODOS TIMES
               ADD 1 TO COUNTER
               WRITE TDLIST-FILE FROM TODO-ITEM(COUNTER)
         END-PERFORM
         CLOSE TDLIST
         EXIT.

       CHANGE-POSITION SECTION.
         EXIT.

       READ-TODOLIST-FROM-FILE SECTION.
         OPEN INPUT TDLIST
         DISPLAY "<ul>"
         PERFORM UNTIL WS-EOF='Y'
               READ TDLIST INTO WS-TDLIST
               AT END MOVE 'Y' TO WS-EOF
                 NOT AT END 
                   MOVE NUMBER-OF-TODOS TO WS-POSITION
                   PERFORM DISPLAY-ITEM
                       ADD 1 TO NUMBER-OF-TODOS
                       MOVE WS-TDLIST TO TODO-ITEM(NUMBER-OF-TODOS)
               END-READ
         END-PERFORM
         DISPLAY "</ul>"
         CLOSE TDLIST
         EXIT.

       END PROGRAM TODOLISTE.
