       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWMENL.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  27/02/2006.
       SECURITY.      *************************************************
                      *                                               *
                      *  Verifica permiss∆o de estaá∆o para o grupo   *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. call-convention 74 is staticWINAPI.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 COMPUTERNAME             PIC  X(025) VALUE SPACES.
           05 TTY                      PIC  X(025) VALUE SPACES.
           05 GRUPO-ID                 PIC  X(005) VALUE SPACES.

       78 HKEY-LOCAL-MACHINE                      value h"80000002".
       01 sub-key-name            pic x(59)       value
          Z"System\CurrentControlSet\Control\ComputerName\ComputerName".
       78 KEY-ALL-ACCESS                          value h"3F".
       01 key-handle              pic x(4) comp-5 value zeroes.
       01 key-value-buffer        pic x(100)      value spaces.
       01 key-value-type          pic x(4) comp-5 value zeroes.
       01 key-value-length        pic x(4) comp-5 value 100.
       01 my-ret-code             pic x(4) comp-5 value zeroes.

       COPY CWUNIX.
       COPY CWCONF.

       LINKAGE SECTION.

       01  FS    PIC X(002).
       01  NOME  PIC X(030).
       01  GRUPO PIC X(022).

       PROCEDURE DIVISION USING FS NOME GRUPO.

       000-INICIO.

           CALL "CWUNIX" USING PARAMETROS-CWUNIX
           IF  CWUNIX-ON
               DISPLAY "COMPUTERNAME"  UPON ENVIRONMENT-NAME
               ACCEPT  COMPUTERNAME    FROM ENVIRONMENT-VALUE
               IF   COMPUTERNAME = SPACES
                    DISPLAY "TTY"         UPON ENVIRONMENT-NAME
                    ACCEPT  TTY           FROM ENVIRONMENT-VALUE
                    MOVE    TTY (6: )       TO COMPUTERNAME
               ELSE
                    INSPECT COMPUTERNAME
                            CONVERTING MINUSCULAS TO MAIUSCULAS
               END-IF
           ELSE
               call staticWINAPI "RegOpenKeyExA"
                  using by value          HKEY-LOCAL-MACHINE
                        by reference      sub-key-name
                        by value          0
                        by value          KEY-ALL-ACCESS
                        by reference      key-handle
                  returning my-ret-code
               end-call
               if my-ret-code = 0
                  call staticWINAPI "RegQueryValueExA"
                     using by value          key-handle
                           by reference      z"ComputerName"
                           by value          0
                           by reference      key-value-type
                           by reference      key-value-buffer
                           by reference      key-value-length
                     returning my-ret-code
                  end-call
                  if my-ret-code = 0
                     MOVE SPACES TO COMPUTERNAME
                     STRING key-value-buffer DELIMITED BY X"00"
                        INTO COMPUTERNAME
                     INSPECT COMPUTERNAME
                             CONVERTING MINUSCULAS TO MAIUSCULAS
                  end-if
               end-if
               call staticWINAPI "RegCloseKey"
                  using by value key-handle
                  returning my-ret-code
               end-call
           END-IF

           IF  GRUPO        = SPACES
           OR  GRUPO (1: 5) = "Acesso"
               GOBACK
           END-IF

           SET CWSQLC-OPEN TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
           MOVE "GU"     TO CWCONF-REG
           MOVE GRUPO    TO CWCONF-ELEMENTO
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           SET CWSQLC-IGNORE-LOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO

           IF  FS-CWCONF < "10"
           AND CWCONF-GRUPO-ID NUMERIC
               MOVE CWCONF-GRUPO-ID TO GRUPO-ID
               MOVE "GS"            TO CWCONF-REG
               MOVE CWCONF-GRUPO-ID TO CWCONF-GU-ID
               SET CWSQLC-START TO TRUE
               SET CWSQLC-NOT-LESS TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
               SET CWSQLC-READ TO TRUE
               SET CWSQLC-NEXT TO TRUE
               CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
               IF   FS-CWCONF < "10"
               AND  CWCONF-TIPO = "GS"
               AND  GRUPO-ID = CWCONF-GU-ID
                    MOVE COMPUTERNAME TO CWCONF-IMPRESSORA
                    SET CWSQLC-READ TO TRUE
                    SET CWSQLC-EQUAL TO TRUE
                    CALL "CWCONF" USING CWSQLC
                                        CWCONF-REG
                                        FS-CWCONF
                                        KCO PCO
                    MOVE FS-CWCONF TO FS
               END-IF
           END-IF

           SET CWSQLC-CLOSE TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO.

       000-99-FIM. GOBACK.

       END PROGRAM CWMENL.
