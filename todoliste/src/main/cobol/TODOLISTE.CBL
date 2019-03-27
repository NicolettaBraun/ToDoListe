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
                         20  FILLER             PIC X(01)  VALUE "-".
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
                         20  DUE              PIC X(01)  VALUE "-".
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
                           20  FILLER             PIC X(01)  VALUE "-".
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
                           20  DUE              PIC X(01)  VALUE "-".
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
                      20  ADDED-FILLER             PIC X(01)  VALUE "-".
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
                      20  DUE-FILLER               PIC X(01)  VALUE "-".
                      20  DUE-MONTH        PIC 9(02).
                      20  DUE-DAY          PIC 9(02).
                 15 DUE-TIME.
                      20  DUE-HOURS        PIC 9(02).
                      20  DUE-MINUTE       PIC 9(02).
                      20  DUE-SECOND       PIC 9(02).
                      20  DUE-MILLISECONDS PIC 9(02).
           10 CATEGORY PIC 9.
           10 DESCRIPTION PIC X(35).
       01 WS-POSITION PIC 9(5).
       01 TESTCOUNTER PIC 9(5).
       01 SCHLEIFENLAUF PIC 9(5).
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
         MOVE NEW-TODO-ITEM TO DESCRIPTION IN WS-TDLIST
         MOVE NUMBER-OF-TODOS TO ITEM-ID IN WS-TDLIST
         MOVE FUNCTION CURRENT-DATE TO ADDED IN WS-TDLIST
         MOVE "DEFAULT" TO USER IN WS-TDLIST
         MOVE 0 TO CATEGORY IN WS-TDLIST
         MOVE WS-TDLIST TO TODO-ITEM (NUMBER-OF-TODOS)
         OPEN EXTEND TDLIST
         WRITE TDLIST-FILE FROM WS-TDLIST
         CLOSE TDLIST
         EXIT.

       CLEAR-LIST SECTION.
         OPEN OUTPUT TDLIST
         CLOSE TDLIST
         EXIT.

       DISPLAY-ITEM SECTION.
         DISPLAY "<li "
<<<<<<< HEAD
         "style='float: left; clear: both;' "
         "onClick='javascript:deleteItem("WS-POSITION")'"
         " id='" WS-POSITION
=======
      *   "onClick='javascript:deleteItem("WS-POSITION")'"
         "onClick='javascript:showMenu()'"
>>>>>>> f63119b51bbeff47bf803e6f5f8bc7b8771e570f
         "'>"
         DESCRIPTION IN WS-TDLIST
      *  "<input value='test' type='button'>"
         "</li>"
         EXIT.

       DELETE-ITEM SECTION.
         PERFORM READ-TODOLIST-FROM-FILE
         MOVE ITEM-TO-DELETE TO TESTCOUNTER
         SUBTRACT 1 FROM NUMBER-OF-TODOS GIVING SCHLEIFENLAUF
         PERFORM UNTIL TESTCOUNTER = SCHLEIFENLAUF
               MOVE TODO-ITEM(TESTCOUNTER + 2)
               TO TODO-ITEM(TESTCOUNTER + 1)
               ADD 1 TO TESTCOUNTER
         END-PERFORM
         OPEN OUTPUT TDLIST
         SUBTRACT 1 FROM NUMBER-OF-TODOS
         MOVE 0 TO TESTCOUNTER
         PERFORM NUMBER-OF-TODOS TIMES
               ADD 1 TO TESTCOUNTER
               WRITE TDLIST-FILE FROM TODO-ITEM(TESTCOUNTER)
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

               NOT AT END PERFORM UNTERSECTION-READ

               END-READ
         END-PERFORM
         DISPLAY "</ul>"
         CLOSE TDLIST
         EXIT.


       UNTERSECTION-READ SECTION.
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
