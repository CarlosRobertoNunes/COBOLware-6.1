       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWUFOK.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  21/04/1989.
       SECURITY.      *************************************************
                      *                                               *
                      *   Verifica se Unidade da federacao e' valida  *
                      *                                               *
                      *************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 UF-ANTERIOR              PIC  X(002) VALUE "%%".
           05 RT-ANTERIOR              PIC  X(002) VALUE "%%".
           05 I                   COMP PIC  9(002) VALUE ZERO.
           05 VALUE
         "ACALAMAPBABRCEDFESFNGOMAMGMSMTPAPBPEPIPRRJRNRORRRSSCSESPTOEX".
              10 ESTADO      OCCURS 30 PIC  X(002).

       LINKAGE SECTION.

       01  UF                          PIC  X(002).

       PROCEDURE DIVISION USING UF.

       000-INICIO.

           IF   UF EQUAL UF-ANTERIOR
                MOVE RT-ANTERIOR TO UF
           ELSE
                PERFORM VARYING I FROM 1 BY 1
                          UNTIL I GREATER 30
                             OR (ESTADO (I) EQUAL UF)
                END-PERFORM
                IF   I GREATER 30
                     MOVE SPACES TO UF
                                    RT-ANTERIOR
                                    UF-ANTERIOR
                ELSE
                     MOVE UF TO UF-ANTERIOR
                                RT-ANTERIOR
                END-IF
           END-IF.

       000-99-FIM. EXIT PROGRAM.

       END PROGRAM CWUFOK.
