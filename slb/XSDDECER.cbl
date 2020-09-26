       IDENTIFICATION DIVISION.
       PROGRAM-ID.    XSDDECER.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  09/09/2017.
       SECURITY.      *************************************************
                      *                                               *
                      *  Suporte a programas gerados pelo XSEED       *
                      *  Tratamnto de File Status                     *
                      *                                               *
                      *************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY CWISAM.
       LINKAGE SECTION.

       01  GLB-TITLE               PIC  X(060).
       01  GLB-DBDATASET           PIC  X(010).
       01  GLB-STATUSCODE          PIC  X(002).

       PROCEDURE DIVISION USING
                          GLB-TITLE
                          GLB-DBDATASET
                          GLB-STATUSCODE.

           MOVE SPACES TO CWISAM-LABEL
           STRING GLB-TITLE DELIMITED BY SPACE
                  ' (' DELIMITED BY SIZE
                  GLB-DBDATASET DELIMITED BY SPACE
                  ')' DELIMITED BY SIZE
                  INTO CWISAM-LABEL
           MOVE GLB-STATUSCODE TO CWISAM-STATUS
           CALL "CWISAM" USING PARAMETROS-CWISAM
           GOBACK.
       END PROGRAM XSDDECER.
