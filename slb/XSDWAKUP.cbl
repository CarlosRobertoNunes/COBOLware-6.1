       IDENTIFICATION DIVISION.
       PROGRAM-ID.    XSDWAKUP.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  09/09/2017.
       SECURITY.      *************************************************
                      *                                               *
                      *  Suporte a programas gerados pelo XSEED       *
                      *  Dispara Report sincrono                      *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 MENSAGEM                PIC  X(80) VALUE SPACES.
           05 GLB-INITSTN             PIC  X(17) VALUE ALL "0".
           05 OPERADOR                PIC  X(030).
           05 TASK                    PIC  9(006) VALUE 0.
           05 PROGRAMA                PIC  X(008) VALUE SPACES.
           05 FUNCAO-PROGRAMA         PIC  X(034) VALUE SPACES.
           05 OBS                     PIC  X(035) VALUE SPACES.
           05 HOJE                    PIC  9(008) VALUE 0.
           05 HORA                    PIC  X(008) VALUE SPACES.
           05 DATA-DE-HOJE            PIC  X(010) VALUE SPACES.

       COPY CWTIME.
       COPY CWGETL.

       LINKAGE SECTION.

       01  GLB-WKCOMANDO    PIC X(10).
       01  GLB-WKREPORT     PIC X(10).
       01  GLB-WKDEVICE     PIC X(02).
       01  GLB-LSN          PIC X(05).

       PROCEDURE DIVISION USING GLB-WKCOMANDO
                                GLB-WKREPORT
                                GLB-WKDEVICE
                                GLB-LSN.

           CALL  "CWGETU"      USING OPERADOR
                                     TASK
                                     PROGRAMA
                                     "?"
           IF   GLB-WKCOMANDO = "WAKEUP"
                CALL  "XSDTASK" USING TASK
                CANCEL "XSDTASK"
      *         CALL "CWGETU" USING OPERADOR TASK GLB-WKREPORT "#"
                MOVE "XSDWAKUP: Report iniciado" TO FUNCAO-PROGRAMA
                PERFORM 130-GRAVA-CWLOGF THRU 130-99-FIM
                CALL GLB-WKREPORT USING GLB-WKDEVICE
                                        GLB-LSN
                                        GLB-INITSTN
                ON EXCEPTION
                   STRING 'Report "'         DELIMITED BY SIZE
                          GLB-WKREPORT       DELIMITED BY SPACE
                          '" n∆o encontrado' DELIMITED BY SIZE
                     INTO MENSAGEM
                   EXEC COBOLware send
                        Message MENSAGEM
                   END-EXEC
                NOT ON EXCEPTION
                    MOVE "XSDWAKUP: Report encerrado" TO FUNCAO-PROGRAMA
                    PERFORM 130-GRAVA-CWLOGF THRU 130-99-FIM
                    CALL "CWGETU" USING OPERADOR TASK PROGRAMA "#"
                    CANCEL GLB-WKREPORT
                END-CALL
                CALL  "XSDTASK" USING TASK
           END-IF

           GOBACK.

       130-GRAVA-CWLOGF.

           CALL "CWGETL" USING PARAMETROS-CWGETL

           IF   CWGETL-LOG = 0
                EXIT PARAGRAPH
           END-IF

           PERFORM 131-DATE-TIME THRU 131-99-FIM

           MOVE FUNCAO-PROGRAMA    TO OBS
           CALL "CWLOGW" USING "#" OBS.

       130-99-FIM. EXIT.

       131-DATE-TIME.

           SET CWTIME-REVERSED     TO TRUE
           SET CWTIME-TODAY        TO TRUE
           CALL "CWTIME"        USING PARAMETROS-CWTIME
           MOVE CWTIME-DATE-FINAL  TO HOJE
           SET CWTIME-NORMAL       TO TRUE
           SET CWTIME-TODAY        TO TRUE
           CALL "CWTIME"        USING PARAMETROS-CWTIME
           MOVE CWTIME-TIME-FINAL  TO CWTIME-TIME
           SET CWTIME-EDIT         TO TRUE
           MOVE CWTIME-DATE-FINAL  TO CWTIME-DATE
           CALL "CWTIME"        USING PARAMETROS-CWTIME
           MOVE CWTIME-DATE-EDITED TO DATA-DE-HOJE
           MOVE CWTIME-TIME-EDITED TO HORA.

       131-99-FIM. EXIT.

       END PROGRAM XSDWAKUP.
