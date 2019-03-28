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
              10 ITEM-ID PIC 9(5).
              10 USER PIC X(10) VALUE "TEST".
              10 ADDED.
                    15 ADDED-DATE.
                         20  ADDED-YEAR         PIC 9(04).
                         20  ADDED-MONTH        PIC 9(02).
                         20  ADDED-DAY          PIC 9(02).
                    15 ADDED-TIME.
                         20  ADDED-HOURS        PIC 9(02).
                         20  ADDED-MINUTE       PIC 9(02).
                         20  ADDED-SECOND       PIC 9(02).
                         20  ADDED-MILLISECONDS PIC 9(02).
              10 DUE.
                    15 DUE-DATE.
                         20  DUE-YEAR         PIC 9(04).
                         20  DUE-MONTH        PIC 9(02).
                         20  DUE-DAY          PIC 9(02).
                    15 DUE-TIME.
                         20  DUE-HOURS        PIC 9(02).
                         20  DUE-MINUTE       PIC 9(02).
                         20  DUE-SECOND       PIC 9(02).
                         20  DUE-MILLISECONDS PIC 9(02).
              10 CATEGORY PIC 9.
              10 DESCRIPTION PIC X(35).
       WORKING-STORAGE SECTION.
       01 TEMP-FIELDS.
        05  NUMBER-OF-TODOS         PIC 9(5) VALUE 0.
           88 LIST-IS-EMPTY VALUE ZERO.
        05  COUNTER                 PIC 9(5).
        05  TODOLISTE.
            07  TODO-ITEM  OCCURS 999.
                10 ITEM-ID PIC 9(5).
                10 USER PIC X(10).
                10 ADDED.
                      15 ADDED-DATE.
                           20  ADDED-YEAR         PIC 9(04).
                           20  ADDED-MONTH        PIC 9(02).
                           20  ADDED-DAY          PIC 9(02).
                      15 ADDED-TIME.
                           20  ADDED-HOURS        PIC 9(02).
                           20  ADDED-MINUTE       PIC 9(02).
                           20  ADDED-SECOND       PIC 9(02).
                           20  ADDED-MILLISECONDS PIC 9(02).
                10 DUE.
                      15 DUE-DATE.
                           20  DUE-YEAR         PIC 9(04).
                           20  DUE-MONTH        PIC 9(02).
                           20  DUE-DAY          PIC 9(02).
                      15 DUE-TIME.
                           20  DUE-HOURS        PIC 9(02).
                           20  DUE-MINUTE       PIC 9(02).
                           20  DUE-SECOND       PIC 9(02).
                           20  DUE-MILLISECONDS PIC 9(02).
                10 CATEGORY PIC 9 VALUE 0.
                  88 None VALUE 0.
                10 DESCRIPTION PIC X(35).
       01 WS-EOF PIC A(1).
       01 WS-TDLIST.
           10 ITEM-ID PIC 9(5).
           10 USER PIC X(10).
           10 ADDED.
                 15 ADDED-DATE.
                      20  ADDED-YEAR         PIC 9(04).
                      20  ADDED-MONTH        PIC 9(02).
                      20  ADDED-DAY          PIC 9(02).
                 15 ADDED-TIME.
                      20  ADDED-HOURS        PIC 9(02).
                      20  ADDED-MINUTE       PIC 9(02).
           10 DUE.
                 15 DUE-DATE.
                      20  DUE-YEAR         PIC 9(04).
                      20  DUE-MONTH        PIC 9(02).
                      20  DUE-DAY          PIC 9(02).
                 15 DUE-TIME.
                      20  DUE-HOURS        PIC 9(02).
                      20  DUE-MINUTE       PIC 9(02).
           10 CATEGORY PIC 9.
           10 DESCRIPTION PIC X(35).
       01 WS-POSITION PIC 9(5).
       01 ITEM-COUNTER PIC 9(5).
       01 SCHLEIFENLAUF PIC 9(5).

       LINKAGE SECTION.
        COPY TODOACTIONS.
        COPY TODOITEM.

       PROCEDURE DIVISION USING TODO-ACTION ACTIVE-USER NEW-TODO-ITEM
         ITEM-TO-DELETE.
           EVALUATE TRUE
           WHEN ACTION-SHOW
              PERFORM READ-TODOLIST-FROM-FILE
           WHEN ACTION-ADD
              PERFORM ADD-NEW-TODO-ITEM
           WHEN ACTION-DELETE
              PERFORM CLEAR-LIST
           WHEN ACTION-DELETEITEM
              PERFORM READ-TODOLIST-FROM-FILE
              PERFORM DELETE-ITEM
              PERFORM WRITE-IN-FILE-AFTER-CHANGE
           END-EVALUATE
           GOBACK
         .

       ADD-NEW-TODO-ITEM SECTION.
           PERFORM READ-TODOLIST-FROM-FILE
           PERFORM ADD-ITEM-TO-LIST
           OPEN EXTEND TDLIST
           WRITE TDLIST-FILE FROM WS-TDLIST
           CLOSE TDLIST
           EXIT.


       ADD-ITEM-TO-LIST SECTION.
           ADD 1 TO NUMBER-OF-TODOS
           MOVE NUMBER-OF-TODOS TO ITEM-ID IN WS-TDLIST
           MOVE NEW-TODO-ITEM TO DESCRIPTION IN WS-TDLIST
           MOVE FUNCTION CURRENT-DATE TO ADDED IN WS-TDLIST
           MOVE ACTIVE-USER TO USER IN WS-TDLIST
           MOVE 0 TO CATEGORY IN WS-TDLIST

           MOVE WS-TDLIST TO TODO-ITEM (NUMBER-OF-TODOS)
           EXIT.

       CLEAR-LIST SECTION.
           OPEN OUTPUT TDLIST
           CLOSE TDLIST
           EXIT.

       DISPLAY-ITEM SECTION.
         DISPLAY "<li "
      *   "onClick='javascript:deleteItem("WS-POSITION")'"
         "onClick='javascript:showMenu("WS-POSITION")'"
         "class='testClass'"
         "'>"

         ADDED-YEAR IN WS-TDLIST
         "-" ADDED-MONTH IN WS-TDLIST
         "-" ADDED-DAY IN WS-TDLIST
         SPACE
         " " ADDED-HOURS IN WS-TDLIST
         ":" ADDED-MINUTE IN WS-TDLIST
         SPACE
         USER IN WS-TDLIST
         SPACE
         DESCRIPTION IN WS-TDLIST
      *  "<input value='test' type='button'>"
         "</li>"
         EXIT.

       DELETE-ITEM SECTION.
         MOVE ITEM-TO-DELETE TO ITEM-COUNTER
         SUBTRACT 1 FROM NUMBER-OF-TODOS GIVING SCHLEIFENLAUF
         PERFORM UNTIL ITEM-COUNTER = SCHLEIFENLAUF
               MOVE TODO-ITEM(ITEM-COUNTER + 2)
               TO TODO-ITEM(ITEM-COUNTER + 1)
               ADD 1 TO ITEM-COUNTER
         END-PERFORM
         SUBTRACT 1 FROM NUMBER-OF-TODOS
         EXIT.

       WRITE-IN-FILE-AFTER-CHANGE SECTION.
         OPEN OUTPUT TDLIST
         MOVE 0 TO ITEM-COUNTER
         PERFORM NUMBER-OF-TODOS TIMES
               ADD 1 TO ITEM-COUNTER
               WRITE TDLIST-FILE FROM TODO-ITEM(ITEM-COUNTER)
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
               NOT AT END PERFORM MOVING-FILE-TO-LIST
               END-READ
         END-PERFORM
         DISPLAY "</ul>"
         CLOSE TDLIST
         EXIT.

       MOVING-FILE-TO-LIST SECTION.
         MOVE NUMBER-OF-TODOS TO WS-POSITION
         ADD 1 TO NUMBER-OF-TODOS
         MOVE WS-TDLIST TO TODO-ITEM(NUMBER-OF-TODOS)
         PERFORM DISPLAY-ITEM
         EXIT.

       REFACTOR-ID SECTION.
         PERFORM VARYING COUNTER FROM 1 UNTIL COUNTER > NUMBER-OF-TODOS
             MOVE COUNTER TO ITEM-ID IN TODO-ITEM(COUNTER)
         END-PERFORM
         EXIT.

       END PROGRAM TODOLISTE.
