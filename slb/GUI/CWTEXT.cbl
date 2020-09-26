       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWTEXT.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  25/01/2001.
       SECURITY.      *************************************************
                      *  SP2                                          *
                      *  Adapta acentos de acordo com a configuracao  *
                      *  Sem efeito no modo gr fico                   *
                      *************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.

       LINKAGE SECTION.

       01  TXT             PIC X(32768).
       01  LEN             PIC 9(008) COMP-X.

       PROCEDURE DIVISION USING TXT LEN.

       000-INICIO.

           GOBACK.

       000-99-FIM. GOBACK.

       END PROGRAM CWTEXT.
