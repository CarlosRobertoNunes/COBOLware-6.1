       IDENTIFICATION DIVISION.
       PROGRAM-ID.    XSDCLEAR.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  09/09/2017.
       SECURITY.      *************************************************
                      *                                               *
                      *  Suporte a programas gerados pelo XSEED       *
                      *  Apaga a tela                                 *
                      *                                               *
                      *************************************************
       PROCEDURE DIVISION.


           DISPLAY (1, 1) ERASE
           GOBACK.

       END PROGRAM XSDCLEAR.
