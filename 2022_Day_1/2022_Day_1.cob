       IDENTIFICATION DIVISION.
       PROGRAM-ID. 2022_Day_1.

       ENVIRONMENT DIVISION.
         INPUT-OUTPUT SECTION.
           FILE-CONTROL.
      *    Define our input file
           SELECT INPUT-FILE ASSIGN TO "data.txt"
             ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
         FILE SECTION.
         FD INPUT-FILE.
      *  Because storing data is hard .. we don't
         01 NOT-USED PIC 9(5).

         WORKING-STORAGE SECTION.

      *  Somewhere to store the current total snacks for an elf
         01 SNACK_TOTAL PIC 9(5) VALUE 0.

      *  Place to store the top three snack values
         01 MAX_SNACKS_1 PIC 9(5) VALUE 0.
         01 MAX_SNACKS_2 PIC 9(5) VALUE 0.
         01 MAX_SNACKS_3 PIC 9(5) VALUE 0.

      *  And somewhere to store the sum of the top three
         01 MAX_THREE_SUM PIC 9(6) VALUE 0.

         01 NUM-BUFFER PIC 9(5).

       PROCEDURE DIVISION.

       OPEN-INPUT-FILE.

           OPEN INPUT INPUT-FILE.

           PERFORM 2246 TIMES
             READ INPUT-FILE INTO NUM-BUFFER
      *      DISPLAY "BUFFER : " NUM-BUFFER

             IF NUM-BUFFER EQUAL 00000 THEN
      *        Snack total is now how many snacks an elf holds
               IF SNACK_TOTAL GREATER MAX_SNACKS_1 THEN
                 MOVE MAX_SNACKS_2 TO MAX_SNACKS_3
                 MOVE MAX_SNACKS_1 TO MAX_SNACKS_2
                 MOVE SNACK_TOTAL TO MAX_SNACKS_1
               END-IF
      *        Start of new elf
      *        DISPLAY "NEXT ELF"
               MOVE 0 TO SNACK_TOTAL
             END-IF

             ADD NUM-BUFFER TO SNACK_TOTAL

           END-PERFORM.

           CLOSE INPUT-FILE.

       END-OPEN-INPUT-FILE.

       PRINT-TOTALS.

           DISPLAY "Part one = " MAX_SNACKS_1.

           ADD MAX_SNACKS_1 TO MAX_THREE_SUM.
           ADD MAX_SNACKS_2 TO MAX_THREE_SUM.
           ADD MAX_SNACKS_3 TO MAX_THREE_SUM.

           DISPLAY "Part two = " MAX_THREE_SUM.

       ALL-FINISHED.
           STOP RUN.
      *END-ALL-FINISHED
