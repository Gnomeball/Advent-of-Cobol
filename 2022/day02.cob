       IDENTIFICATION DIVISION.
       PROGRAM-ID. 2022_Day_2.

       ENVIRONMENT DIVISION.
         INPUT-OUTPUT SECTION.
           FILE-CONTROL.
      *    Define our input file
           SELECT INPUT-FILE ASSIGN TO "data/day02.txt"
             ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

         FILE SECTION.
         FD INPUT-FILE.
         01 ROUNDS.
      *    I believe this has to be hard coded ..
           02 ROUND PIC A(3) OCCURS 2500 TIMES.

         WORKING-STORAGE SECTION.

      *  How many rounds there are
         01 N_GAMES PIC 9(4) VALUE 2500.

      *  I believe this has to be hard coded ..
         01 GAMES OCCURS 2500 TIMES.
           02 GAME PIC A(3).

      *  To store our running totals
         01 RUNNING_TOTAL_1 PIC 9(5) VALUE 0.
         01 RUNNING_TOTAL_2 PIC 9(5) VALUE 0.

      *  Pointer to a game
         01 G PIC 9(4) VALUE 1.

      *  End of file pointer
         01 EOF PIC A(1).

       PROCEDURE DIVISION.

           OPEN INPUT INPUT-FILE.
           PERFORM READ-GAMES UNTIL EOF = "Y".
           CLOSE INPUT-FILE.

      *    PERFORM PRINT-GAMES VARYING R FROM 1 BY 1 UNTIL R > N_GAMES.

           PERFORM PLAY-GAMES.

           PERFORM PART-ONE.
           PERFORM PART-TWO.

           PERFORM ALL-FINISHED.
           STOP RUN.

       READ-GAMES.
           READ INPUT-FILE AT END MOVE "Y" TO EOF.
           IF EOF IS NOT = "Y"
             MOVE ROUNDS TO GAMES(G)
             ADD 1 TO G
           END-IF.

       PLAY-GAMES.
           PERFORM VARYING G FROM 1 BY 1 UNTIL G > N_GAMES
      *      Ok this is quite dumb, there has to be a nicer way
             IF GAME(G) EQUAL "A X" THEN
               ADD 4 TO RUNNING_TOTAL_1
               ADD 3 TO RUNNING_TOTAL_2
             END-IF
             IF GAME(G) EQUAL "A Y" THEN
               ADD 8 TO RUNNING_TOTAL_1
               ADD 4 TO RUNNING_TOTAL_2
             END-IF
             IF GAME(G) EQUAL "A Z" THEN
               ADD 3 TO RUNNING_TOTAL_1
               ADD 8 TO RUNNING_TOTAL_2
             END-IF
             IF GAME(G) EQUAL "B X" THEN
               ADD 1 TO RUNNING_TOTAL_1, RUNNING_TOTAL_2
             END-IF
             IF GAME(G) EQUAL "B Y" THEN
               ADD 5 TO RUNNING_TOTAL_1, RUNNING_TOTAL_2
             END-IF
             IF GAME(G) EQUAL "B Z" THEN
               ADD 9 TO RUNNING_TOTAL_1, RUNNING_TOTAL_2
             END-IF
             IF GAME(G) EQUAL "C X" THEN
               ADD 7 TO RUNNING_TOTAL_1
               ADD 2 TO RUNNING_TOTAL_2
             END-IF
             IF GAME(G) EQUAL "C Y" THEN
               ADD 2 TO RUNNING_TOTAL_1
               ADD 6 TO RUNNING_TOTAL_2
             END-IF
             IF GAME(G) EQUAL "C Z" THEN
               ADD 6 TO RUNNING_TOTAL_1
               ADD 7 TO RUNNING_TOTAL_2
             END-IF
           END-PERFORM.

       PART-ONE.
           DISPLAY "Part one = ", RUNNING_TOTAL_1.

       PART-TWO.
           DISPLAY "Part two = ", RUNNING_TOTAL_2.

       PRINT-GAMES.
           DISPLAY "Game : ", G, " = ", GAME(G).

       ALL-FINISHED.
           STOP RUN.
