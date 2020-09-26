       IDENTIFICATION DIVISION.
       PROGRAM-ID.    XSDLINEP.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  09/09/2017.
       SECURITY.      *************************************************
                      *                                               *
                      *  Suporte a programas gerados pelo XSEED       *
                      *  Imprime GLB-STBACKUP na impressora padr∆o    *
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

       01  GLB-STBACKUP            PIC X(30).
       01  GLB-SLASH               PIC X(01).
       01  GLB-REPPREFIX           PIC X(10).
       01  GLB-WTITLE              PIC X(60).
       01  GLB-XTITLE              PIC X(60).
       01  GLB-LSN                 PIC X(05).

       PROCEDURE DIVISION USING GLB-STBACKUP
                                GLB-SLASH
                                GLB-REPPREFIX
                                GLB-WTITLE
                                GLB-XTITLE
                                GLB-LSN.

       000-INICIO.

           MOVE GLB-STBACKUP  TO GLB-WTITLE
           INSPECT GLB-WTITLE CONVERTING '\' TO '/'
           CALL "XSDSPOOL" USING GLB-WTITLE
                                 GLB-WSTATION
                                 GLB-PAGEMARK
                                 GLB-PAGESKIP
                                 GLB-LSN
                                 GLB-INITSTN
                                 GLB-STATUSCODE.

       000-99-FIM. GOBACK.

       END PROGRAM XSDLINEP.
