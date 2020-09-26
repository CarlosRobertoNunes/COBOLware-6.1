       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWSQLT.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  07/10/2014.
       SECURITY.      *************************************************
                      *                                               *
                      * SET/GET para COMMIT automatico/manual         *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  COMMIT-MODE         PIC  9(001) VALUE 0.
           88 AUTO-COMMIT           VALUE 0.
           88 COMMIT-MANUAL         VALUE 1.

       LINKAGE SECTION.

       01  FUNCAO              PIC  X(001).
       01  COMMIT-MODE-LK      PIC  9(001).

       PROCEDURE DIVISION USING FUNCAO COMMIT-MODE-LK.

           IF  FUNCAO = 'S' OR 's'
               MOVE COMMIT-MODE-LK TO COMMIT-MODE
           ELSE
               MOVE COMMIT-MODE    TO COMMIT-MODE-LK
           END-IF
           GOBACK.

       END PROGRAM CWSQLT.
