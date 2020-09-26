       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWATTR.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  18/02/2005.
       SECURITY.      *************************************************
                      *                                               *
                      *  Informa atributos configurados pelo usu rio  *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FSINI  ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  FILE STATUS   IS FS-FSINI.

       DATA DIVISION.
       FILE SECTION.

       FD  FSINI
           VALUE OF FILE-ID IS LB-FSINI.

       01  FSINI-REG                   PIC X(5080).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 SIZE1         COMP-X PIC  9(002) VALUE 0.
           05 SIZE2         COMP-X PIC  9(002) VALUE 0.
           05 COBSW                PIC  X(030) VALUE SPACES.
           05 PROGRAMA             PIC  X(030) VALUE SPACES.
           05 BYTE                 PIC  X(008) VALUE SPACES.
           05 LETRA         COMP-X PIC  9(002) VALUE 0.
           05 N             COMP-X PIC  9(002) VALUE 0.
           05 B             COMP-X PIC  9(002) VALUE 0.
           05 N-255         COMP-X PIC  9(002) VALUE 0.
           05 RESULTADO     COMP-X PIC  9(002) VALUE 0.
           05 PARAMETRO.
              10 SW         COMP-X PIC  9(002) OCCURS 26.
           05 NUMERO               PIC  9(018) VALUE 0.
           05 CWNUMERO             PIC  X(018) VALUE SPACES.
           05 TESTENAME            PIC  X(030) VALUE SPACES.
           05 COBWARE3             PIC  X(255) VALUE SPACES.
           05 COBDIR               PIC  X(5000) VALUE SPACES.
           05 CWLOADS              PIC  X(5000) VALUE SPACES.
           05 PATH                 PIC  X(007)  VALUE "COBDIR".
           05 NEWcobdir            PIC  X(5000) VALUE SPACES.
           05 cobware              PIC  X(255) VALUE SPACES.
           05 I                    PIC  9(004) VALUE 0.
           05 II COMP-X            PIC  9(008) VALUE 0.
           05 I2                   PIC  9(004) VALUE 0.
           05 Y                    PIC  9(004) VALUE 0.
           05 NX                   PIC  9(002) COMP-X value 0.
           05 vez                  PIC  9(002) COMP-X value 0.
           05 VARNAME              PIC  X(255) VALUE SPACES.
           05 VARDATA              PIC  X(5000) VALUE SPACES.
           05 UPPER-REG            PIC  X(5080) VALUE SPACES.
           05 ER-FSINI.
              10 FS-FSINI          PIC  X(002) VALUE "00".
              10 LB-FSINI          PIC  X(255) VALUE "cwmenu.ini".
           05 SAVES.
     >*       10 ATTR                    PIC  X(001) VALUE X"8F".
     >        10 ATTR                    PIC  X(001) VALUE X"08".
      *       10 ATTR                    PIC  X(001) VALUE X"70".
      *       10 ATTR                    PIC  X(001) VALUE X"00".
              10 ENTRY-ATTR              PIC  X(001) VALUE X"F0".
      *       10 DISABLE-ATTR            PIC  X(001) VALUE X"70".
              10 DISABLE-ATTR            PIC  X(001) VALUE X"00".
              10 CURSOR-ATTR             PIC  X(001) VALUE X"00".
       copy cwunix.
      * 0 Preto
      * 1 Azul Marinho
      * 2 Verde Clorofila
      * 3 Azul m‚dio
      * 4 Vinho
      * 5 Roxo
      * 6 Verde Oliva
      * 7 Cinza claro *
      * 8 Cinza escuro
      * 9 Azul piscina
      * A Verde limÆo
      * B Azul claro *
      * C Vermelho
      * D Rosa
      * E Amarelo
      * F Branco

       LINKAGE SECTION.

       01   STATIC   PIC X(001).
       01   FIELD    PIC X(001).
       01   DISABLED PIC X(001).
       01   CURCOLOR PIC X(001).

       PROCEDURE DIVISION USING STATIC FIELD DISABLED CURCOLOR.

       000-INICIO.

           ON 1
              call "CWUNIX" USING PARAMETROS-CWUNIX
              IF CWUNIX-ON
                 MOVE "COBPATH" TO PATH
              END-IF
              DISPLAY "CWMENU.INI" UPON ENVIRONMENT-NAME
              ACCEPT LB-FSINI FROM ENVIRONMENT-VALUE
              DISPLAY "COBSW" UPON ENVIRONMENT-NAME
              ACCEPT COBSW FROM ENVIRONMENT-VALUE
              IF   COBSW = SPACES OR ALL X"00"
                   DISPLAY "CWCOBSW" UPON ENVIRONMENT-VALUE
      *            CALL X"91" USING RESULTADO X"0E" PARAMETRO
                   MOVE 4        TO SW (02)
                   MOVE 9        TO SW (11)
                   MOVE 1        TO SW (13)
                   MOVE 1        TO SW (15)
                   MOVE 11       TO SW (16)
                   MOVE 2        TO SW (19)
                   CALL X"91" USING RESULTADO X"0D" PARAMETRO
              END-IF
              GO TO LER-INI.

           IF CWUNIX-ON
              GOBACK
           END-IF

           ON 1
              DISPLAY "CWSTATIC-COLOR"  UPON ENVIRONMENT-NAME
              PERFORM AJUSTA
              IF  NUMERO NOT = 0
                  MOVE NUMERO TO NX
                  MOVE NX (1: 1) TO ATTR
              END-IF
              DISPLAY "CWFIELD-COLOR"   UPON ENVIRONMENT-NAME
              PERFORM AJUSTA
              IF  NUMERO NOT = 0
                  MOVE NUMERO TO NX
                  MOVE NX (1: 1) TO ENTRY-ATTR
              END-IF
              DISPLAY "CWDISABLED-COLOR" UPON ENVIRONMENT-NAME
              PERFORM AJUSTA
              IF  NUMERO NOT = 0
                  MOVE NUMERO TO NX
                  MOVE NX (1: 1) TO DISABLE-ATTR
              END-IF
              DISPLAY "CWCURSOR-COLOR" UPON ENVIRONMENT-NAME
              PERFORM AJUSTA
              IF  NUMERO NOT = 0
                  MOVE NUMERO TO NX
                  MOVE NX (1: 1) TO CURSOR-ATTR
              END-IF.

           MOVE ATTR         TO STATIC
           MOVE ENTRY-ATTR   TO FIELD
           MOVE DISABLE-ATTR TO DISABLED.
           MOVE CURSOR-ATTR  TO CURCOLOR.

       000-99-FIM. GOBACK.

       LER-INI.

           PERFORM TEST AFTER UNTIL FS-FSINI NOT = "9A"
                   OPEN INPUT FSINI
           END-PERFORM

           IF FS-FSINI = "30" OR "35"
              add 1 to vez
              evaluate vez
                  when 1
                       move "../cwmenu.ini" to LB-FSINI
                       GO TO LER-INI
                  when 2
                      move spaces to NEWcobdir
                      DISPLAY "COBOLWARE" UPON ENVIRONMENT-NAME
                      ACCEPT NEWcobdir FROM ENVIRONMENT-VALUE
                      If   NEWcobdir not = spaces
                           move spaces to LB-FSINI
                           STRING NEWcobdir delimited by space
                                  "/cwmenu.ini" delimited by size
                                 into LB-FSINI
                           GO TO LER-INI
                      end-if
              end-evaluate
           ELSE
              If   Vez < 4
                   DISPLAY "CWINI" UPON ENVIRONMENT-NAME
                   DISPLAY LB-FSINI UPON ENVIRONMENT-VALUE
              END-IF
           END-IF

           PERFORM TEST AFTER UNTIL FS-FSINI > "09"
                   READ FSINI INTO UPPER-REG
                   IF  FS-FSINI = "00"
                   AND (FSINI-REG (1: 1) NOT = "*")
                       PERFORM VARYING I FROM 1 BY 1 UNTIL I > 80
                                    OR FSINI-REG (I: 1) = "="
                               CONTINUE
                       END-PERFORM
                       IF  FSINI-REG (I: 1) = "="
                           MOVE I TO I2
                           SUBTRACT 1 FROM I
                           MOVE 1 TO Y
                           INSPECT UPPER-REG
                                   CONVERTING MINUSCULAS TO MAIUSCULAS
                           PERFORM UNTIL (FSINI-REG(Y:1) NOT = SPACE)
                                     AND (UPPER-REG (Y: 4) NOT = "SET ")
                                    IF  UPPER-REG (Y: 4) = "SET "
                                        ADD 4 TO Y
                                        SUBTRACT 4 FROM I
                                    ELSE
                                        ADD 1 TO Y
                                        SUBTRACT 1 FROM I
                                    END-IF
                           END-PERFORM
                           MOVE FSINI-REG(Y:I) TO VARNAME
                           COMPUTE I = I2 + 1
                           PERFORM UNTIL I > 80
                                   OR  (FSINI-REG (I: 1) NOT = SPACE)
                                       ADD 1 TO I
                           END-PERFORM
                           MOVE FSINI-REG (I:) TO VARDATA
                           IF   VARDATA(1: 1) = '$'
                                ADD 1 TO I
                                MOVE FSINI-REG (I:) TO VARDATA
                                DISPLAY VARDATA UPON ENVIRONMENT-NAME
                                MOVE    SPACES TO VARDATA
                                ACCEPT  VARDATA FROM ENVIRONMENT-VALUE
                           END-IF
                           MOVE VARNAME TO TESTENAME
                           INSPECT TESTENAME CONVERTING MINUSCULAS
                                           TO MAIUSCULAS
                           IF TESTENAME(1:7) = 'CWCOBSW'
                              PERFORM set-sw THRU fim-sw
                           ELSE
                              DISPLAY VARNAME UPON ENVIRONMENT-NAME
                              DISPLAY VARDATA UPON ENVIRONMENT-VALUE
                           END-IF
                           IF TESTENAME = 'CIL'
                              DISPLAY PATH   UPON ENVIRONMENT-NAME
                              ACCEPT  COBDIR FROM ENVIRONMENT-VALUE
                              move spaces to newcobdir
                              perform varying Size1 from length cobdir
                                             by -1 until Size1 = 0
                                             OR cobdir(Size1:1) <> ' '
                                      CONTINUE
                              end-perform
SP                            if cobdir(Size1:1) = ';'
SP                               subtract 1 from Size1
SP                            end-if
                              perform varying Size2 from length vardata
                                             by -1 until Size2 = 0
                                             OR vardata(Size2:1) <> ' '
                                      CONTINUE
                              end-perform
                              string cobdir(1:Size1)
                                     ';'
                                     vardata(1:Size2) delimited by size
                                 into newcobdir
                              If cwunix-on
                                 inspect newcobdir converting ';' to ':'
      *                       else
      *                          inspect newcobdir converting ':' to ';'
                              end-if
                              DISPLAY newcobdir UPON ENVIRONMENT-VALUE
                              IF NOT CWUNIX-ON
                                 DISPLAY "COBPATH"
                                    UPON ENVIRONMENT-NAME
                                 DISPLAY newcobdir
                                    UPON ENVIRONMENT-VALUE
                              END-IF
                           END-IF
                       END-IF
           END-PERFORM
           CLOSE FSINI

           IF  (LB-FSINI NOT = "fsclient.ini")
           AND (LB-FSINI NOT = "Sets.cfg")
               MOVE "fsclient.ini" TO LB-FSINI
               move 4 to vez
               GO TO LER-INI
           END-IF

           IF  LB-FSINI NOT = "Sets.cfg"
               MOVE "Sets.cfg" TO LB-FSINI
               GO TO LER-INI
           END-IF

           DISPLAY 'COBWARE3' UPON ENVIRONMENT-NAME
           ACCEPT   COBWARE3  FROM ENVIRONMENT-VALUE
           INSPECT  COBWARE3
                    CONVERTING MINUSCULAS TO MAIUSCULAS

           IF   COBWARE3 NOT = 'OFF'
                DISPLAY "COBOLWARE" UPON ENVIRONMENT-NAME
                ACCEPT COBWARE   FROM ENVIRONMENT-VALUE
                IF COBWARE = SPACES
                   MOVE '.' TO COBWARE
                END-IF
                MOVE SPACES TO COBWARE3
                STRING COBWARE DELIMITED BY SPACE
                       '/cobware3.lbr/COBWARE3' delimited by size
                     INTO COBWARE3
                CALL COBWARE3
                      ON EXCEPTION
                         CONTINUE
                END-CALL
          END-IF.

          DISPLAY 'CWLOADS' UPON ENVIRONMENT-NAME
          ACCEPT   CWLOADS   FROM ENVIRONMENT-VALUE
          IF    CWLOADS NOT = SPACES
                INSPECT CWLOADS CONVERTING ';:,|' TO SPACES
                CALL 'CWPACK' USING CWLOADS LENGTH CWLOADS II
                INITIALIZE PROGRAMA I2
                PERFORM VARYING I FROM 1 BY 1 UNTIL I > II
                        IF I = II
                        OR CWLOADS(I:1) = SPACE
                           IF PROGRAMA NOT = SPACES
                              CALL PROGRAMA
                           END-IF
                           INITIALIZE PROGRAMA I2
                        ELSE
                            ADD 1 TO I2
                            MOVE CWLOADS(I:1) TO PROGRAMA(I2:1)
                        END-IF
                END-PERFORM
          END-IF.

       FIM-LER-INI. GOBACK.

       AJUSTA.

           MOVE SPACES TO CWNUMERO
           MOVE 0      TO NUMERO
           ACCEPT CWNUMERO FROM ENVIRONMENT-VALUE
           MOVE 18 TO II
           PERFORM VARYING I FROM LENGTH OF CWNUMERO BY -1
                   UNTIL I = 0
                   IF  CWNUMERO (I: 1) NUMERIC
                       MOVE CWNUMERO (I: 1)
                         TO   NUMERO (II: 1)
                       SUBTRACT 1 FROM II
                   END-IF
           END-PERFORM.

       FIM-AJUSTA. EXIT.

       set-sw.

           MOVE TESTENAME(8:) TO VARNAME
           MOVE 0 TO LETRA

           PERFORM VARYING I FROM 1 BY 1
                             UNTIL I > LENGTH OF VARNAME
                              OR VARNAME (I:1) NUMERIC
                   IF VARNAME (I:1) > X'40'
                   AND VARNAME (I:1) < X'5B'
                       MOVE VARNAME (I:1) TO LETRA(1:)
                       SUBTRACT 64 FROM LETRA
                   END-IF
           END-PERFORM

           IF LETRA = 0
              GO TO fim-sw
           END-IF

           MOVE SPACES TO CWNUMERO
           MOVE 0      TO NUMERO

           IF I < LENGTH OF VARNAME
              MOVE VARNAME (I:) TO CWNUMERO
              MOVE SPACES TO VARNAME (I:)
           END-IF

           PERFORM VARYING I FROM LENGTH OF CWNUMERO BY -1
                   UNTIL I = 0
                   IF  CWNUMERO (I: 1) NUMERIC
                       MOVE CWNUMERO (I: 1)
                         TO   NUMERO (II: 1)
                       SUBTRACT 1 FROM II
                   END-IF
           END-PERFORM

           IF NUMERO = 0
              MOVE 1 TO NUMERO
           END-IF

           INSPECT VARDATA CONVERTING MINUSCULAS TO MAIUSCULAS
           CALL X"91" USING RESULTADO X"0E" PARAMETRO
           MOVE ALL "0"    TO BYTE
           MOVE SW (LETRA) TO N
           MOVE 128        TO N-255
           PERFORM VARYING B FROM 1 BY 1 UNTIL B > 8
                   IF N NOT < N-255
                      MOVE "1" TO BYTE (B:1)
                      SUBTRACT N-255 FROM N
                   END-IF
                   COMPUTE N-255 = N-255 / 2
           END-PERFORM

           COMPUTE B = 9 - NUMERO
           IF VARDATA = 'ON'
              MOVE "1" TO BYTE (B:1)
           END-IF
           IF VARDATA = 'OFF'
              MOVE "0" TO BYTE (B:1)
           END-IF
           MOVE 0 TO N
           MOVE 128 TO N-255
           PERFORM VARYING B FROM 1 BY 1 UNTIL B > 8
                   IF "1" = BYTE (B:1)
                      ADD N-255 TO N
                   END-IF
                   COMPUTE N-255 = N-255 / 2
           END-PERFORM
           MOVE N TO SW (LETRA)
      *    DISPLAY "COBSW"   UPON ENVIRONMENT-NAME
      *    DISPLAY "CWCOBSW" UPON ENVIRONMENT-VALUE
           CALL X"91" USING RESULTADO X"0D" PARAMETRO.

       fim-sw.  EXIT.
       END PROGRAM CWATTR.
