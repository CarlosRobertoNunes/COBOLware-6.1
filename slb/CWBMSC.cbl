      $SET NOOSVS
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWBMSC.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  09/08/2008.
       SECURITY.      *************************************************
                      *                                               *
                      *  Tratamento de campos desprotegidos           *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 ATT          PIC X      VALUE SPACE.
           05 I     comp-5 PIC S9(04) VALUE 0.

       LINKAGE SECTION.

       01   OPT            PIC  X.
       01   TAMANHO comp-x PIC  9(08).
       01   DIGITOS comp-5 PIC S9(04).
       01   DS.
            05 DATANAME    PIC  X(09).
            05 SUBSCRIPT   PIC  X(02).
       01   CAMPO          PIC  X.
       01   A              PIC  X.
            88 ATTR VALUE 'H' '1' '2' '3' '4' '5' '6' '7'.
       01   accepted       PIC  X.

       PROCEDURE DIVISION USING OPT TAMANHO DIGITOS
                                DS
                                CAMPO A accepted.

       000-INICIO.

           EVALUATE TRUE
               WHEN OPT = "S"
                    IF accepted(1:TAMANHO) NOT = LOW-VALUES
                    AND (CAMPO(1:TAMANHO) = LOW-VALUES)
                       move accepted(1:TAMANHO)
                         to campo(1:TAMANHO)
                    END-IF
               WHEN OPT = "P"
                    IF ATTR
                       MOVE A TO ATT
                       INSPECT ATT CONVERTING '24' TO '42'
                       EXEC COBOLware SetFocus
                            FIELD DATANAME
                            SUBSCRIPT SUBSCRIPT
                            OPTION ATT
                       END-EXEC
                    END-IF
                    IF A = "-"
                       EXEC COBOLware SetFocus
                            FIELD DATANAME FIXED
                            SUBSCRIPT SUBSCRIPT
                       END-EXEC
                    ELSE
                       EXEC COBOLware SetFocus
                            FIELD DATANAME UNPROTECT
                            SUBSCRIPT SUBSCRIPT
                       END-EXEC
                       IF DIGITOS = -1
                          EXEC COBOLware SetFocus
                               FIELD DATANAME
                            SUBSCRIPT SUBSCRIPT
                          END-EXEC
                       END-IF
                    END-IF
               WHEN OPT = "R"
                    move accepted(1:TAMANHO) to campo(1:TAMANHO)
               WHEN OPT = "C"
                    MOVE 0 TO DIGITOS
                    IF NOT A = '-'
                       PERFORM VARYING I FROM 1 BY 1
                                       UNTIL I > TAMANHO
                              IF  CAMPO(I:1) NOT = accepted(I:1)
                                  ADD 1 TO DIGITOS
                              END-IF
                       END-PERFORM
                       IF DIGITOS = 0
                          MOVE LOW-VALUES TO CAMPO(1:TAMANHO)
                       END-IF
                    END-IF
           END-EVALUATE.

       000-99-FIM. GOBACK.

       END PROGRAM CWBMSC.
