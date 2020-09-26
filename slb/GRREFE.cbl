       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GRREFE.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  19/03/1988.
       SECURITY.      *************************************************
                      *                                               *
                      *   Retorna mes e ano de referencia P/extenso   *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 I                              PIC  9(006) VALUE ZERO.
           05 Y                              PIC  9(006) VALUE ZERO.
           05 BASE.
              10 MES-BASE                    PIC  X(009).
              10 ANO-BASE                    PIC  /99.
           05 FILLER REDEFINES BASE.
              10 BYTE-BASE  PIC X OCCURS 12.
           05 BASE2 VALUE SPACES.
              10 BYTE-BASE2 PIC X OCCURS 12.
           05 TABELA-DE-MESES.
              10 FILLER PIC X(009) VALUE "  JANEIRO".
              10 FILLER PIC X(009) VALUE "FEVEREIRO".
              10 FILLER PIC X(009) VALUE "    MARCO".
              10 FILLER PIC X(009) VALUE "    ABRIL".
              10 FILLER PIC X(009) VALUE "     MAIO".
              10 FILLER PIC X(009) VALUE "    JUNHO".
              10 FILLER PIC X(009) VALUE "    JULHO".
              10 FILLER PIC X(009) VALUE "   AGOSTO".
              10 FILLER PIC X(009) VALUE " SETEMBRO".
              10 FILLER PIC X(009) VALUE "  OUTUBRO".
              10 FILLER PIC X(009) VALUE " NOVEMBRO".
              10 FILLER PIC X(009) VALUE " DEZEMBRO".
              10 FILLER PIC X(009) VALUE "      13§".
           05 FILLER REDEFINES TABELA-DE-MESES.
              10 NOME-MES PIC X(009) OCCURS 13.
           05 HOJE.
              10 AA-H                  PIC  9(002) VALUE ZEROS.
              10 MM-H                  PIC  9(002) VALUE ZEROS.
              10 DD-H                  PIC  9(002) VALUE ZEROS.

       LINKAGE SECTION.

       01   PARAMETROS-GRREFE.
            05 GRREFE-ALINHA  PIC  X(001).
               88 ESQUERDA VALUE "E" "e".
            05 GRREFE-MMAA    PIC  9(004).
            05 FILLER REDEFINES GRREFE-MMAA.
               10 GRREFE-MM   PIC  9(002).
               10 FILLER REDEFINES GRREFE-MM PIC X(002).
                  88 MES-OK VALUE "01" THRU "13".
               10 GRREFE-AA   PIC  9(002).
               10 FILLER REDEFINES GRREFE-AA PIC X(002).
                  88 ANO-OK VALUE "00" THRU "99".
            05 GRREFE-EXTENSO PIC  X(012).

       PROCEDURE DIVISION USING PARAMETROS-GRREFE.

       010-PROCESSAMENTO.

           IF   NOT ANO-OK
           OR   NOT MES-OK
                ACCEPT HOJE FROM DATE
                MOVE MM-H TO GRREFE-MM
                MOVE AA-H TO GRREFE-AA.

           MOVE NOME-MES (GRREFE-MM) TO MES-BASE
           MOVE GRREFE-AA            TO ANO-BASE

           IF   ESQUERDA
                MOVE ZERO   TO Y
                MOVE SPACES TO BASE2
                PERFORM 020-SHIFT-RIGHT THRU 020-99-FIM
                        VARYING I FROM 1 BY 1
                                UNTIL I GREATER 12
                MOVE BASE2 TO BASE.

            MOVE BASE TO GRREFE-EXTENSO.

       010-99-FIM. EXIT PROGRAM.

       020-SHIFT-RIGHT.

           IF   BYTE-BASE (I) NOT EQUAL SPACE
                ADD  1             TO Y
                MOVE BYTE-BASE (I) TO BYTE-BASE2 (Y).

       020-99-FIM. EXIT.

       END PROGRAM GRREFE.
