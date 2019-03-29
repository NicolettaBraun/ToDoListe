
       01 TD-ITEM.
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
       01 WHICH-KIND-SORT PIC X(10).
