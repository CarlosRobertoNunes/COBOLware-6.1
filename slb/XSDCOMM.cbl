       IDENTIFICATION DIVISION.
       PROGRAM-ID.    XSDCOMM INITIAL.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  09/09/2017.
       SECURITY.      *************************************************
                      *  Suporte a programas gerados pelo XSEED       *
                      *  Executa linha de comando                     *
                      *                                               *
                      *************************************************
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       COPY CWEXEC.

       LINKAGE SECTION.

       01  GLB-SENDMSG PIC X(80).

       PROCEDURE DIVISION USING GLB-SENDMSG.

           SET  CWEXEC-NOWARNING  TO TRUE
           SET  CWEXEC-ASSYNCRONE TO TRUE
           MOVE GLB-SENDMSG       TO CWEXEC-COMANDO
           CALL "CWEXEC"       USING PARAMETROS-CWEXEC
           GOBACK.

       END PROGRAM XSDCOMM.
