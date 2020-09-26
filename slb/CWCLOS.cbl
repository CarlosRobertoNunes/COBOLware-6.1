       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWCLOS.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  09/03/2009.
       SECURITY.      *************************************************
                      *                                               *
                      *  Fecha todos os arquivos abertos via  CWSQLC  *
                      *                                               *
                      *************************************************
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 OP                       PIC X(2) VALUE SPACES.
           05 LB                       PIC X(50) VALUE SPACES.

       01 CLOSEDS-TXT.
          05 CLOSEDS               PIC  9(004) VALUE 0.

       01 FCD.
          COPY xfhfcd.
      *   COPY F:\COBOL32\source\xfhfcd.

       LINKAGE SECTION.

       COPY CWCLOS.

       PROCEDURE DIVISION USING PARAMETROS-CWCLOS.

       000-INICIO.

           MOVE "C" TO OP
           CALL "CWSQLC" USING BY REFERENCE OP FCD LB.
           DISPLAY "CWCLOS" UPON ENVIRONMENT-NAME
           ACCEPT CLOSEDS-TXT FROM ENVIRONMENT-VALUE
           MOVE CLOSEDS TO CWCLOS-FILES.

       000-99-FIM. GOBACK.

       END PROGRAM CWCLOS.
