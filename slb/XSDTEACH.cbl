       IDENTIFICATION DIVISION.
       PROGRAM-ID.    XSDTEACH.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  09/09/2017.
       SECURITY.      *************************************************
                      *                                               *
                      *  Suporte a programas gerados pelo XSEED       *
                      *  Exibe arquivo de ajuda
                      *                                               *
                      *************************************************
       LINKAGE SECTION.

       01  ISPEC                   PIC X(05).
       01  GLB-TEACH               PIC X(05).
       01  GLB-STATUS              PIC X(05).
       01  GLB-SLASH               PIC X(01).
       01  GLB-TITLE               PIC X(60).
       01  GLB-LSN                 PIC X(05).

       PROCEDURE DIVISION USING ISPEC
                                GLB-TEACH
                                GLB-STATUS
                                GLB-SLASH
                                GLB-TITLE
                                GLB-LSN.

           INSPECT GLB-TITLE CONVERTING LOW-VALUES TO SPACES
           IF   GLB-TITLE NOT = SPACES
                EXEC COBOLware Help
                     File GLB-TITLE
                END-EXEC
           END-IF

           GOBACK.

       END PROGRAM XSDTEACH.
