       IDENTIFICATION DIVISION.
       PROGRAM-ID. TODOLISTE.


       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
         FILE-CONTROL.
         SELECT TDLIST ASSIGN TO 'todolist.txt'
            ORGANIZATION IS LINE SEQUENTIAL.
         SELECT PRINT-FILE ASSIGN TO 'todolist.txt'
            ORGANIZATION IS LINE SEQUENTIAL.
         SELECT TDLIST-WORK ASSIGN TO 'testsort.txt'
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
          FD TDLIST.
          01 TDLIST-FILE.
              10 ITEM-ID PIC 9(5).
              10 USER PIC X(10).
              10 PRIO PIC X.
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
          SD TDLIST-WORK.
          01 TDLIST-FILE-W.
              10 ITEM-ID-W PIC 9(5).
              10 USER-W PIC X(10) VALUE "TEST".
              10 ADDED-W.
                    15 ADDED-DATE-W.
                         20  ADDED-YEAR-W         PIC 9(04).
                         20  FILLER             PIC X(01)  VALUE "-".
                         20  ADDED-MONTH-W        PIC 9(02).
                         20  ADDED-DAY-W          PIC 9(02).
                    15 ADDED-TIME-W.
                         20  ADDED-HOURS-W        PIC 9(02).
                         20  ADDED-MINUTE-W       PIC 9(02).
                         20  ADDED-SECOND-W       PIC 9(02).
                         20  ADDED-MILLISECONDS-W PIC 9(02).
              10 DUE-W.
                    15 DUE-DATE-W.
                         20  DUE-YEAR-W         PIC 9(04).
                         20  DUE              PIC X(01)  VALUE "-".
                         20  DUE-MONTH-W        PIC 9(02).
                         20  DUE-DAY-W          PIC 9(02).
                    15 DUE-TIME-W.
                         20  DUE-HOURS-W        PIC 9(02).
                         20  DUE-MINUTE-W       PIC 9(02).
                         20  DUE-SECOND-W       PIC 9(02).
                         20  DUE-MILLISECONDS-W PIC 9(02).
              10 CATEGORY-W PIC 9.
              10 DESCRIPTION-W PIC X(35).
       WORKING-STORAGE SECTION.
       01 TEMP-FIELDS.
        05  NUMBER-OF-TODOS         PIC 9(3) VALUE 0.
           88 LIST-IS-EMPTY VALUE ZERO.
        05  COUNTER                 PIC 9(5).
        05  TODOLISTE.
            07  TODO-ITEM  OCCURS 999.
                10 ITEM-ID PIC 9(3).
                10 USER PIC X(10).
                10 PRIO PIC X.
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
           10 ITEM-ID PIC 9(3).
           10 USER PIC X(10).
           10 PRIO PIC X.
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
       01 ITEM-COUNTER PIC 9(5).
       01 SCHLEIFENLAUF PIC 9(5).
       01 DISPLAY-DATE-TIME.
           05 DISPLAY-DATE.
              10  DISPLAY-YEAR         PIC 9(04).
              10  DISPLAY-MONTH        PIC 9(02).
              10  DISPLAY-DAY          PIC 9(02).
           05 DISPLAY-TIME.
              10  DISPLAY-HOURS        PIC 9(02).
              10  DISPLAY-MINUTE       PIC 9(02).

       LINKAGE SECTION.
        COPY TODOACTIONS.
        COPY TODOITEM.

       PROCEDURE DIVISION USING TODO-ACTION ACTIVE-USER
             NEW-ITEM-DESCRIPTION ITEM-TO-DELETE WHICH-KIND-SORT.
           EVALUATE TRUE
           WHEN ACTION-SHOW
              PERFORM READ-TODOLIST-FROM-FILE
           WHEN ACTION-ADD
              PERFORM ADD-NEW-ITEM
           WHEN ACTION-DELETE
              PERFORM CLEAR-LIST
           WHEN ACTION-DELETEITEM
              PERFORM READ-TODOLIST-FROM-FILE
              PERFORM DELETE-ITEM
              PERFORM WRITE-IN-FILE-AFTER-CHANGE
           WHEN ACTION-SORT
              PERFORM SORT-USER
           END-EVALUATE
           GOBACK
         .

       ADD-NEW-ITEM SECTION.
           PERFORM READ-TODOLIST-FROM-FILE
           PERFORM ADD-ITEM-TO-LIST
           OPEN EXTEND TDLIST
           WRITE TDLIST-FILE FROM WS-TDLIST
           CLOSE TDLIST
           EXIT.


       ADD-ITEM-TO-LIST SECTION.
           MOVE NEW-ITEM-DESCRIPTION TO DESCRIPTION IN WS-TDLIST
           MOVE FUNCTION CURRENT-DATE TO ADDED IN WS-TDLIST
           MOVE ACTIVE-USER TO USER IN WS-TDLIST
           MOVE 0 TO CATEGORY IN WS-TDLIST

           PERFORM MOVING-FILE-TO-LIST
           EXIT.

       CLEAR-LIST SECTION.
           OPEN OUTPUT TDLIST
           CLOSE TDLIST
           EXIT.

       DISPLAY-ITEM SECTION.
         DISPLAY "<li "
      *   "onClick='javascript:deleteItem("WS-POSITION")'"
         "onClick='javascript:showMenu("ITEM-ID in WS-TDLIST")'"
         "class='testClass'"
         "'>"
         DISPLAY ITEM-ID IN WS-TDLIST "." SPACE

         MOVE ADDED IN WS-TDLIST TO DISPLAY-DATE-TIME
         PERFORM DISPLAY-FORMATTED-TIME

         DISPLAY SPACE USER IN WS-TDLIST
         SPACE
         DESCRIPTION IN WS-TDLIST
      *  "<input value='test' type='button'>"
         "</li>"
         EXIT.

       DISPLAY-FORMATTED-TIME SECTION.

                DISPLAY DISPLAY-YEAR
                "-" DISPLAY-MONTH
                "-" DISPLAY-DAY
                SPACE
                " " DISPLAY-HOURS
                ":" DISPLAY-MINUTE
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
         PERFORM REFACTOR-ID
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
         ADD 1 TO NUMBER-OF-TODOS
         MOVE NUMBER-OF-TODOS TO ITEM-ID IN WS-TDLIST
         MOVE WS-TDLIST TO TODO-ITEM(NUMBER-OF-TODOS)
         PERFORM DISPLAY-ITEM
         EXIT.

       REFACTOR-ID SECTION.
         PERFORM VARYING COUNTER FROM 1 UNTIL COUNTER > NUMBER-OF-TODOS
             MOVE COUNTER TO ITEM-ID IN TODO-ITEM(COUNTER)
         END-PERFORM
         EXIT.

       WHICH-SORT SECTION.
         DISPLAY WHICH-KIND-SORT UPON SYSERR
         EVALUATE WHICH-KIND-SORT
         WHEN "user"
           PERFORM SORT-USER
         WHEN "category"
           PERFORM SORT-CATEGORY
         END-EVALUATE
         EXIT.

       SORT-USER SECTION.
         DISPLAY "glos" UPON SYSERR
         SORT TDLIST-WORK ON ASCENDING KEY USER IN TDLIST-FILE USING
           TDLIST GIVING TDLIST
         EXIT.
       
       SORT-CATEGORY SECTION.
         SORT TDLIST-WORK ON ASCENDING KEY CATEGORY IN TDLIST-FILE USING 
           TDLIST GIVING TDLIST
         EXIT.

       END PROGRAM TODOLISTE.
