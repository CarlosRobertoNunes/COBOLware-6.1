       IDENTIFICATION DIVISION.
       PROGRAM-ID.    XSDSETUP.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  09/09/2017.
       SECURITY.      *************************************************
                      *                                               *
                      *  Suporte a programas gerados pelo XSEED       *
                      *  Ler variavel de ambiente                     *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       LINKAGE SECTION.

       01  GLB-STPARAMETRO         PIC X(20).
       01  GLB-VALUE               PIC X(30).

       PROCEDURE DIVISION USING GLB-STPARAMETRO
                                GLB-VALUE.

       800-INICIAIS.

           DISPLAY GLB-STPARAMETRO UPON ENVIRONMENT-NAME
           ACCEPT  GLB-VALUE       FROM ENVIRONMENT-VALUE
           GOBACK.
