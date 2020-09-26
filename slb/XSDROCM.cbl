       IDENTIFICATION DIVISION.
       PROGRAM-ID.    XSDROCM.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  09/09/2017.
       SECURITY.      *************************************************
                      *                                               *
                      *  Suporte a programas gerados pelo XSEED       *
                      *  Transfere GLBXLP para SPOOL e executa        *
                      *  CWMEN9                                       *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       77  GLB-WSTATION     PIC X(017) VALUE SPACES.
       77  GLB-PAGEMARK     PIC X(003) VALUE SPACES.
       77  GLB-PAGESKIP     PIC X(001) VALUE SPACES.
       77  GLB-INITSTN      PIC X(017) VALUE SPACES.
       77  GLB-STATUSCODE   PIC X(003) VALUE SPACES.

       LINKAGE SECTION.

       01  GLB-WTITLE              PIC X(60).
       01  GLB-REPNAME             PIC X(30).
       01  GLB-STATUS              PIC X(03).
       01  GLB-LSN                 PIC X(05).

       PROCEDURE DIVISION USING GLB-WTITLE
                                GLB-REPNAME
                                GLB-STATUS
                                GLB-LSN.

       000-INICIO.

           MOVE ".PA"         TO GLB-PAGEMARK
           MOVE "<Spool>"     TO GLB-WSTATION
           CALL "XSDSPOOL" USING GLB-WTITLE
                                 GLB-WSTATION
                                 GLB-PAGEMARK
                                 GLB-PAGESKIP
                                 GLB-LSN
                                 GLB-INITSTN
                                 GLB-STATUSCODE.

       000-99-FIM. GOBACK.

       END PROGRAM XSDROCM.
