       IDENTIFICATION DIVISION.
       PROGRAM-ID. 2022_Day_1.

       ENVIRONMENT DIVISION.
         INPUT-OUTPUT SECTION.
           FILE-CONTROL.
      *    Define our input file
           SELECT INPUT-FILE ASSIGN TO "data/day01.txt"
             ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

         FILE SECTION.
         FD INPUT-FILE.
      *  Make a data structure to house our elfs
         01 ELFS.
      *    I believe this has to be hard coded ..
           02 SNACKS PIC A(5) OCCURS 2246 TIMES.

         WORKING-STORAGE SECTION.

      *  Make a data structure to house the elfs snacks
      *    I believe this has to be hard coded ..
         01 SNACK OCCURS 2246 TIMES.
           02 CALORIES PIC 9(5) VALUE 0.

         01 SNACK-PILE OCCURS 247 TIMES.
           02 TOTAL PIC 9(5) VALUE 0.

         01 RUNNING_TOTAL PIC 9(5) VALUE 0.

      *  Pointer to a snack
         01 S PIC 9(4) VALUE 1.
      *  Pointer to a total
         01 T PIC 9(3) VALUE 1.

      *  Used for number shifting shenanigans
         01 NUM-BUFFER PIC 9(5) VALUE ZEROES.

      *  Place to store the top three snack values
         01 MAX_SNACKS_1 PIC 9(5) VALUE 0.
         01 MAX_SNACKS_2 PIC 9(5) VALUE 0.
         01 MAX_SNACKS_3 PIC 9(5) VALUE 0.

      *  And somewhere to store the sum of the top three
         01 MAX_THREE_SUM PIC 9(6) VALUE 0.

      *  End of file pointer
         01 EOF PIC A(1).

       PROCEDURE DIVISION.

           OPEN INPUT INPUT-FILE.
           PERFORM READ-ELF UNTIL EOF = "Y".
           CLOSE INPUT-FILE.

      *    PERFORM PRINT-SNACKS VARYING S FROM 1 BY 1 UNTIL S = 2247.
           PERFORM BUILD-PILES.
      *    PERFORM PRINT-TOTALS VARYING T FROM 1 BY 1 UNTIL T = 248.
           PERFORM FIND-TOP-THREE.

           PERFORM PART-ONE.
           PERFORM PART-TWO.

           PERFORM ALL-FINISHED.
           STOP RUN.

       READ-ELF.
           READ INPUT-FILE AT END MOVE "Y" TO EOF.
           IF EOF IS NOT = "Y"
             MOVE ELFS TO SNACK(S)

      *      If this number does not have a 5th character
             IF SNACK(S)(5:1) LESS 0 OR GREATER 9 THEN
               MOVE SNACK(S) TO NUM-BUFFER
      *        Fill the number with zeroes (e.g. 00000)
               MOVE ZEROES TO SNACK(S)
      *        Shift it to the right by one
               MOVE NUM-BUFFER TO SNACK(S)(2:4)
             END-IF

             ADD 1 TO S
           END-IF.

       BUILD-PILES.
           PERFORM VARYING S FROM 1 BY 1 UNTIL S = 2248
             IF CALORIES(S) GREATER 0 THEN
               ADD CALORIES(S) TO RUNNING_TOTAL
             ELSE
               ADD RUNNING_TOTAL TO TOTAL(T)
               ADD 1 TO T
               MOVE 0 TO RUNNING_TOTAL
             END-IF
           END-PERFORM.

       FIND-TOP-THREE.
           PERFORM VARYING T FROM 1 BY 1 UNTIL T = 248
             IF TOTAL(T) GREATER MAX_SNACKS_1 THEN
                 MOVE MAX_SNACKS_2 TO MAX_SNACKS_3
                 MOVE MAX_SNACKS_1 TO MAX_SNACKS_2
                 MOVE TOTAL(T) TO MAX_SNACKS_1
               END-IF
           END-PERFORM.

       PART-ONE.
           DISPLAY "Part one = ", MAX_SNACKS_1.

       PART-TWO.
           ADD MAX_SNACKS_1 TO MAX_THREE_SUM.
           ADD MAX_SNACKS_2 TO MAX_THREE_SUM.
           ADD MAX_SNACKS_3 TO MAX_THREE_SUM.
           DISPLAY "Part two = ", MAX_THREE_SUM.

       PRINT-SNACKS.
           DISPLAY "Snack : ", S, " = ", CALORIES(S).

       PRINT-TOTALS.
           DISPLAY "Total : ", T, " = ", TOTAL(T).

       ALL-FINISHED.
           STOP RUN.
