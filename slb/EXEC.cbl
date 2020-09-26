       IDENTIFICATION DIVISION.
       AUTHOR.        COBOLware Servicws Ltda.
       DATE-WRITTEN.  01/11/1996.
       SECURITY.      *************************************************
                      *                                               *
                      * Emulacao de rotinas do Micro Base COBOL em    *
                      * Micro Focus COBOL                             *
                      *                                               *
                      *************************************************
                      *                                               *
                      * Execucao de comandos do sistema operacioal    *
                      *                                               *
                      *************************************************

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 I                        PIC 9(003) COMP VALUE 0.
           05 CMDLINE                  PIC X(160) VALUE SPACES.

       COPY CWEXEC.

       LINKAGE SECTION.

       01  FS             PIC X(002).
       01  INTCMD         PIC X(001).
       01  LINCMD.
           05 BYTE-LINCMD PIC X(001) OCCURS 160.

       PROCEDURE DIVISION USING FS INTCMD LINCMD.

       000-INICIO.

           MOVE SPACES TO CMDLINE
           PERFORM VARYING I FROM 1 BY 1 UNTIL BYTE-LINCMD (I) = "$"
                                            OR I > 160
                   MOVE BYTE-LINCMD (I) TO CMDLINE (I: 1)
           END-PERFORM

           SET CWEXEC-HIDE  TO TRUE
           MOVE CMDLINE     TO CWEXEC-COMMAND
           CALL "CWEXE2" USING PARAMETROS-CWEXEC

           IF  (CWEXEC-RETORNO NOT = 0)
                MOVE "01" TO FS
           ELSE
                MOVE "00" TO FS
           END-IF.

       000-99-FIM. GOBACK.
