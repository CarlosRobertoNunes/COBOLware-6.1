       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GRDATA.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  13/06/1988.
       SECURITY.      *************************************************
                      *                                               *
                      *   Fornece Data (dd/mm/aa) e horas hh:mm:ss    *
                      *                                               *
                      *************************************************

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 HOJE.
              10 AA-H-1                PIC  9(002) VALUE ZEROS.
              10 MM-H-1                PIC  9(002) VALUE ZEROS.
              10 DD-H-1                PIC  9(002) VALUE ZEROS.
           05 DATA-DE-HOJE.
              10 DD-H-2                PIC  9(002) VALUE ZEROS.
              10 FILLER                PIC  X(001) VALUE "/".
              10 MM-H-2                PIC  9(002) VALUE ZEROS.
              10 FILLER                PIC  X(001) VALUE "/".
              10 AA-H-2                PIC  9(002) VALUE ZEROS.
           05 TEMPO.
              10 HH-1                  PIC  9(002) VALUE ZEROS.
              10 MM-1                  PIC  9(002) VALUE ZEROS.
              10 SS-1                  PIC  9(002) VALUE ZEROS.
              10 DD-1                  PIC  9(002) VALUE ZEROS.
           05 HORA.
              10 HH-2                  PIC  9(002) VALUE ZEROS.
              10 FILLER                PIC  X(001) VALUE ":".
              10 MM-2                  PIC  9(002) VALUE ZEROS.
              10 FILLER                PIC  X(001) VALUE ":".
              10 SS-2                  PIC  9(002) VALUE ZEROS.

       LINKAGE SECTION.

       01  DIA                  PIC   X(008).
       01  HORAS                PIC   X(008).

       PROCEDURE DIVISION USING DIA HORAS.

       000-INICIO.

           ACCEPT  HOJE    FROM DATE
           ACCEPT  TEMPO   FROM TIME
           MOVE DD-H-1       TO DD-H-2
           MOVE MM-H-1       TO MM-H-2
           MOVE AA-H-1       TO AA-H-2
           MOVE HH-1         TO HH-2
           MOVE MM-1         TO MM-2
           MOVE SS-1         TO SS-2
           MOVE DATA-DE-HOJE TO DIA
           MOVE HORA         TO HORAS.

       000-99-FIM. EXIT PROGRAM.

       END PROGRAM GRDATA.
