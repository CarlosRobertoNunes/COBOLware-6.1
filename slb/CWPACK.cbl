       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWPACK.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  30/11/2010.
       SECURITY.      *************************************************
                      *                                               *
                      * Retira espa‡os duplos de campo at‚ 8192 bytes *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 NOVO                 PIC X(32768) VALUE SPACES.
           05 TONUM                PIC  9(001) VALUE 0.
           05 I             COMP-X PIC  9(008) VALUE 0.
           05 Y             COMP-X PIC  9(008) VALUE 0.
           05 X91-RESULT    COMP-X PIC  9(002) VALUE 0.
           05 X91-FUNCTION  COMP-X PIC  9(002) VALUE 16.
           05 X91-PARAMETER COMP-X PIC  9(002) VALUE 0.

       LINKAGE SECTION.

       01  CAMPO           PIC X.
       01  TAMANHO         PIC 9(008) COMP-X.
       01  TAMANHO-RETORNO PIC 9(008) COMP-X.

       PROCEDURE DIVISION USING CAMPO TAMANHO
                                      TAMANHO-RETORNO.

       000-INICIO.

           CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER
           IF   X91-PARAMETER < 2
                GOBACK
           END-IF
           IF   X91-PARAMETER = 3
           AND  TAMANHO-RETORNO(1:1) = '*'
                MOVE CAMPO(1:TAMANHO) TO NOVO
                MOVE ALL '0' TO CAMPO(1:TAMANHO)
                MOVE TAMANHO TO Y
                PERFORM VARYING I FROM LENGTH OF TAMANHO BY -1
                        UNTIL I = 0
                        IF  NOVO (I: 1) NUMERIC
                            MOVE NOVO  (I: 1)
                              TO CAMPO (Y: 1)
                            SUBTRACT 1 FROM Y
                        END-IF
                END-PERFORM
                GOBACK
           END-IF
           MOVE 0      TO Y
           MOVE SPACES TO NOVO
           INSPECT CAMPO (1: TAMANHO) CONVERTING LOW-VALUES TO SPACES
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > TAMANHO
                   IF I < TAMANHO
                   AND CAMPO(I: 2) = SPACE
                      CONTINUE
                   ELSE
                      IF CAMPO (I:1) NUMERIC
                      AND Y > 1
                      AND NOVO (Y - 1:2) = ' 0'
                          SUBTRACT 1 FROM Y
                          MOVE SPACE TO NOVO (Y: 1)
                          IF CAMPO (I:1) = '0'
                             EXIT PERFORM CYCLE
                          END-IF
                      END-IF
                      ADD 1 TO Y
                      MOVE CAMPO (I:1) TO NOVO (Y:1)
                      IF NOVO (1:1) = SPACE OR '0'
                         MOVE 0 TO Y
                      END-IF
                   END-IF
           END-PERFORM
           MOVE NOVO TO CAMPO(1: TAMANHO)
           IF   X91-PARAMETER > 2
                IF CAMPO(Y:1) = SPACE
                   SUBTRACT 1 FROM Y
                END-IF
                MOVE Y TO TAMANHO-RETORNO
           END-IF.

       000-99-FIM. GOBACK.

       END PROGRAM CWPACK.
