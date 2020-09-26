       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GRSDAY.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  05/03/1996.
       SECURITY.      *************************************************
                      *                                               *
                      *   Subtrai nnnn dias de uma data               *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
             05 T-DMAX.
                10 FILLER              PIC  X(012) VALUE "312831303130".
                10 FILLER              PIC  X(012) VALUE "313130313031".
             05 RT-DMAX REDEFINES T-DMAX.
                10 DIA-MAX OCCURS 12   PIC  9(002).
             05 W-TP-ANO               PIC  9(001).
                88 ANO-BISSEXTO                    VALUE  0.
             05 W-DIA                  PIC  9(004).
             05 W-NADA                 PIC  9(002).

       LINKAGE SECTION.

       01  PARAMETROS-GRSDAY.
           05 DDMMAA.
              10 DD      PIC 9(02).
              10 MM      PIC 9(02).
              10 AA      PIC 9(02).
           05 GRSDAY-NUM-DIAS   PIC 9(04).

       PROCEDURE DIVISION USING PARAMETROS-GRSDAY.

       000-INICIO.

           IF DD > GRSDAY-NUM-DIAS
              COMPUTE W-DIA = DD - GRSDAY-NUM-DIAS
              MOVE W-DIA TO DD
              GO TO 000-00-FIM.

           COMPUTE W-DIA = GRSDAY-NUM-DIAS - DD.

       000-01-CHECK-MES-ANO.

           COMPUTE MM = MM - 1.
           IF MM = ZERO
              MOVE 12 TO MM
              COMPUTE AA = AA - 1
              IF AA = ZERO
                 MOVE 99 TO AA.

       000-01-CHECK-ANO.

           DIVIDE AA BY 4
                  GIVING W-NADA
                         REMAINDER W-TP-ANO.

           IF   ANO-BISSEXTO
                MOVE 29 TO DIA-MAX (2)
           ELSE
                MOVE 28 TO DIA-MAX (2).

       000-02-MENOS-DIA.

           IF   W-DIA GREATER DIA-MAX (MM)
                COMPUTE W-DIA = W-DIA - DIA-MAX (MM)
                GO     TO 000-01-CHECK-MES-ANO.

           COMPUTE DD = DIA-MAX (MM) - W-DIA.

       000-00-FIM. EXIT PROGRAM.

       END PROGRAM GRSDAY.
