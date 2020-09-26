       IDENTIFICATION DIVISION.
       PROGRAM-ID.    XSDISPUP.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  09/09/2017.
       SECURITY.      *************************************************
                      *                                               *
                      *  Suporte a programas gerados pelo XSEED       *
                      *  Verificar execucao indevida                  *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 TASK                     PIC  9(006) VALUE ZERO.
           05 NOME                     PIC  X(030) VALUE SPACES.
           05 PROGRAMA                 PIC  X(008) VALUE SPACES.
           05 MENSAGEM
              VALUE "FATAL ERROR: Program XXXXX, Access denied".
              10         PIC X(21).
              10 ISPEC-D PIC X(05).
              10         PIC X(52).

       LINKAGE SECTION.

       01  GLB-DATABASE            PIC X(10).
       01  ISPEC                   PIC X(05).
       01  GLB-CALL                PIC X(05).

       PROCEDURE DIVISION USING GLB-DATABASE ISPEC GLB-CALL.

       000-INICIO.

           CALL "CWGETU" USING NOME TASK PROGRAMA "?"
           MOVE 0 TO TASK
           CALL "XSDTASK" USING TASK

           IF   PROGRAMA = ISPEC
                MOVE PROGRAMA TO GLB-CALL
           END-IF

           IF   GLB-CALL = SPACES OR LOW-VALUES
                MOVE ISPEC TO ISPEC-D
                EXEC COBOLware Send
                     Message MENSAGEM
                END-EXEC
                STOP RUN
           END-IF.

       000-99-FIM. GOBACK.

       END PROGRAM XSDISPUP.
