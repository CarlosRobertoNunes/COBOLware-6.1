       ATALHO.

           DISPLAY "COBSW" UPON ENVIRONMENT-NAME
           ACCEPT COBSW FROM ENVIRONMENT-VALUE
           IF   COBSW = SPACES OR ALL X"00"
                CALL X"91" USING RESULTADO X"0E" PARAMETRO
                ADD  4        TO SW (02)
                SUBTRACT 1  FROM SW (06)
                ADD  9        TO SW (16)
                ADD  1        TO SW (13)
                MOVE 0        TO SW (14)
                CALL X"91" USING RESULTADO X"0D" PARAMETRO
           END-IF

           DISPLAY "COBOLWARE" UPON ENVIRONMENT-NAME
           ACCEPT    COBWARE   FROM ENVIRONMENT-VALUE
           perform check-ware
           IF   COBWARE (1: 1) = "/"
                DISPLAY "COBPATH" UPON ENVIRONMENT-NAME
                ACCEPT  COBDIR FROM ENVIRONMENT-VALUE
                move 'cobware' to modo
           ELSE
                DISPLAY "COBDIR" UPON ENVIRONMENT-NAME
                ACCEPT  COBDIR FROM ENVIRONMENT-VALUE
                PERFORM UNTIL COBDIR = SPACES
                    PERFORM VARYING SizeDir FROM LENGTH COBDIR BY -1
                                    UNTIL SizeDir = 1
                                    OR (COBDIR (SizeDir:1) NOT = SPACE)
                            CONTINUE
                    END-PERFORM
                    PERFORM VARYING SizeCil FROM 1 BY 1
                                    UNTIL SizeCil = SizeDir
                                       OR COBDIR (SizeCil:1) = SPACE
                            CONTINUE
                    END-PERFORM
                    IF SizeDir = SizeCil
                       EXIT PERFORM
                    ELSE
                       MOVE COBDIR TO COBDIR2
                       PERFORM VARYING SizeCil From 1 By 1
                               UNTIL SizeCil > SizeDir
                                  OR COBDIR (SizeCil:1) = ';'
                               CONTINUE
                       END-PERFORM
                       IF COBDIR (SizeCil:1) = ';'
                          IF MFDIR = SPACES
                             SUBTRACT 1 FROM SIZECIL
                             MOVE COBDIR(1:SIZECIL) TO MFDIR
                             ADD 1 TO SizeCil
                          END-IF
                          ADD 1 TO SizeCil
                          MOVE COBDIR2 (SizeCil:) TO COBDIR
                          DISPLAY COBDIR UPON ENVIRONMENT-VALUE
                       END-IF
                       MOVE SPACES TO COBDIR2
                    END-IF
                END-PERFORM
           END-IF
           IF   COBWARE = SPACES
                IF  COBDIR NOT = SPACES
                    MOVE "1" TO JUNTA
                END-IF
                CALL "CBL_READ_DIR"   USING OLD-DIRECTORY
                                            SIZE-OLD-DIR
                INSPECT OLD-DIRECTORY
                        CONVERTING LOW-VALUE TO SPACE
                PERFORM VARYING I FROM SIZE-OLD-DIR
                                    BY -1
                                 UNTIL I = 1
                                    OR OLD-DIRECTORY (I: 1)
                                       NOT = SPACE
                        CONTINUE
                END-PERFORM
                DISPLAY "PWD"    UPON ENVIRONMENT-NAME
                ACCEPT   COBWARE FROM ENVIRONMENT-VALUE
                IF  COBWARE (1: 1) = '/'
                    MOVE OLD-DIRECTORY TO COBWARE
                ELSE
                     MOVE SPACES TO COBWARE
                     CALL "PC_READ_DRIVE"  USING OLD-DRIVE
                                                 PRINTER-STATUS
                          ON EXCEPTION
                             MOVE OLD-DIRECTORY TO COBWARE
                          NOT ON  EXCEPTION
                              STRING OLD-DRIVE ":\"
                                     OLD-DIRECTORY (1: I)
                                      DELIMITED BY SIZE
                              INTO COBWARE
                     END-CALL
                END-IF
                perform check-ware
                DISPLAY "COBOLWARE" UPON ENVIRONMENT-NAME
                DISPLAY  COBWARE UPON ENVIRONMENT-VALUE
           END-IF

           IF   COBWARE (1: 1) = "/"
                MOVE ":" TO SEPARA
                MOVE "/" TO BARRA
           ELSE
                MOVE ";" TO SEPARA
                MOVE "\" TO BARRA
                DISPLAY "TEMP" UPON ENVIRONMENT-NAME
                ACCEPT   TEMP  FROM ENVIRONMENT-VALUE
                String TEMP   DELIMITED BY SPACE
                       "\rpv" DELIMITED BY SIZE
                              INTO PATH-MD
                CALL "CBL_CREATE_DIR" USING PATH-MD
           END-IF

           STRING COBWARE   DELIMITED BY SPACE
                  BARRA     DELIMITED BY SIZE
                  MODO      DELIMITED BY SPACE
                  ".lbr"    DELIMITED BY SIZE
                  BARRA     DELIMITED BY SIZE
                  "CWSEND"  DELIMITED BY SIZE
                  INTO CWSEND

           IF   COBDIR = SPACES
                MOVE COBWARE TO COBDIR
           ELSE
                IF  JUNTA = 1
                    MOVE SPACES TO COBDIR2
                    PERFORM VARYING SizeDir FROM LENGTH COBDIR BY -1
                                    UNTIL SizeDir = 1
                                    OR (COBDIR (SizeDir:1) NOT = SPACE)
                            CONTINUE
                    END-PERFORM
                    PERFORM VARYING SizeCil FROM LENGTH COBWARE  BY -1
                                    UNTIL SizeCil = 1
                                    OR (COBWARE (SizeCil:1) NOT = SPACE)
                            CONTINUE
                    END-PERFORM
                    STRING COBWARE(1:SizeCil)
                           SEPARA
                           COBDIR(1:SizeDir)  DELIMITED BY SIZE
                           INTO COBDIR2
                    MOVE COBDIR2 TO COBDIR
                END-IF
           END-IF

           ACCEPT LINHA-COMANDO FROM COMMAND-LINE
           IF LINHA-COMANDO NOT = SPACES
              DISPLAY "CWCMDL" UPON ENVIRONMENT-NAME
              DISPLAY LINHA-COMANDO UPON ENVIRONMENT-VALUE
              PERFORM VARYING I FROM 1 BY 1
                     UNTIL I > (LENGTH OF LINHA-COMANDO - 4)
      *           IF   LINHA-COMANDO (I: 4) = "/nof"
      *           OR                          "/noF"
      *           OR                          "/nOf"
      *           OR                          "/nOF"
      *           OR                          "/Nof"
      *           OR                          "/NoF"
      *           OR                          "/NOf"
      *           OR                          "/NOF"
      *                MOVE 1 TO NOFRAME
      *                PERFORM UNTIL (I NOT < LENGTH OF LINHA-COMANDO)
      *                           OR LINHA-COMANDO (I: 1) = SPACE
      *                        MOVE SPACES TO LINHA-COMANDO (I: 1)
      *                        ADD 1 TO I
      *                END-PERFORM
      *                MOVE 0 TO I
      *           END-IF
                  IF LINHA-COMANDO (I: 3) = "/C:" OR "/c:"
                     IF I > 1
                        MOVE LINHA-COMANDO (1: I - 1) TO LINHA-COMANDO2
                     END-IF
                     MOVE I TO I2
                     MOVE 0 TO Y
                     ADD  3 TO I
                     PERFORM VARYING I FROM I BY 1
                             UNTIL I > LENGTH OF LINHA-COMANDO
                                OR LINHA-COMANDO (I: 1) = SPACE
                              ADD  1                   TO Y
                             MOVE LINHA-COMANDO (I:1) TO CIL-C (Y:1)
                     END-PERFORM
DUMBO                ADD 1 TO I
                     IF  I NOT > LENGTH OF LINHA-COMANDO
                         MOVE LINHA-COMANDO (I: )
                           TO LINHA-COMANDO2 (I2: )
                     END-IF
                     MOVE    LINHA-COMANDO2  TO LINHA-COMANDO
                     DISPLAY LINHA-COMANDO UPON COMMAND-LINE
                     MOVE   0 TO I
                  END-IF
                  IF LINHA-COMANDO (I: 3) = "/D:" OR "/d:"
                     IF I > 1
                         MOVE LINHA-COMANDO (1: I - 1) TO LINHA-COMANDO2
                     END-IF
                     MOVE I TO I2
                     MOVE 0 TO Y
                     ADD  3 TO I
                     PERFORM VARYING I FROM I BY 1
                             UNTIL I > LENGTH OF LINHA-COMANDO
                                OR LINHA-COMANDO (I: 1) = SPACE
                             ADD  1                   TO Y
                             MOVE LINHA-COMANDO (I:1) TO PASTA-D (Y:1)
                     END-PERFORM
DUMBO                ADD 1 TO I
                     IF  I NOT > LENGTH OF LINHA-COMANDO
                         MOVE LINHA-COMANDO (I: )
                           TO LINHA-COMANDO2 (I2: )
                     END-IF
                     MOVE    LINHA-COMANDO2  TO LINHA-COMANDO
                     DISPLAY LINHA-COMANDO UPON COMMAND-LINE
                     MOVE   0 TO I
                  END-IF
              END-PERFORM
              IF  LINHA-COMANDO NOT = SPACES
                  DISPLAY LINHA-COMANDO UPON COMMAND-LINE
              END-IF
           END-IF

           IF  CIL-C = SPACES
               DISPLAY "CIL5" UPON ENVIRONMENT-NAME
               ACCEPT CIL-C   FROM  ENVIRONMENT-VALUE
               IF  CIL-C = SPACES
                   DISPLAY "CIL" UPON ENVIRONMENT-NAME
                   ACCEPT CIL-C FROM  ENVIRONMENT-VALUE
               END-IF
           END-IF

           MOVE SPACES TO COBDIR2
                          SP2DIR

           PERFORM VARYING SizeDir FROM LENGTH COBDIR BY -1
                           UNTIL SizeDir = 1
                           OR (COBDIR (SizeDir:1) NOT = SPACE)
                   CONTINUE
           END-PERFORM
SP         IF  COBDIR(SizeDir:1) = SEPARA
SP             SUBTRACT 1 FROM SizeDir
SP         END-IF
           IF  CIL-C NOT = SPACES
ANA            DISPLAY "CIL" UPON ENVIRONMENT-NAME
ANA            DISPLAY CIL-C UPON ENVIRONMENT-VALUE
               PERFORM VARYING SizeCIL FROM LENGTH CIL-C  BY -1
                               UNTIL SizeCil = 1
                               OR (CIL-C (SizeCil:1) NOT = SPACE)
                       CONTINUE
               END-PERFORM
               STRING COBDIR(1:SizeDir)
                      SEPARA
                      CIL-C(1:SizeCil)
                      SEPARA
                      "cil" DELIMITED BY SIZE
                      INTO COBDIR2
           ELSE
               STRING COBDIR(1:SizeDir)
                      SEPARA
                      "cil"  DELIMITED BY SIZE
                      INTO COBDIR2
           END-IF
           MOVE COBDIR2 TO COBDIR
           IF  MFDIR NOT = SPACES
               CALL 'utils.dll'
               ON EXCEPTION
                   CONTINUE
               END-CALL
               move cobdir TO cobdir2
               move spaces to cobdir
               move 0      to y
               perform varying I From 1 By 1 UNTIL I > LENGTH COBDIR
                       if cobdir2(i:1) = '%' AND Flag = 1
                          MOVE 0 TO FLAG
                          ADD 2 TO I
                       END-IF
                       if cobdir2(i:1) = '%' AND Flag = 0
                          MOVE 1 TO FLAG
                       END-IF
                       if flag = 0
                          add 1 to y
                          move cobdir2(i:1) TO cobdir(y:1)
                       end-if
               end-perform
               MOVE MFDIR TO cobdir2
               PERFORM VARYING SizeCil FROM length of cobdir2 BY -1
                       UNTIL SizeCil = 1
                          OR ((cobdir2 (SizeCil: 1) not = space)
                          AND (cobdir2 (SizeCil: 1) not = '\'))
                             CONTINUE
               END-PERFORM
               add 1 to SizeCil
               move x'00' to cobdir2 (SizeCil: )
               move spaces to MFDIR
               move length of MFDIR to LenShort
               CALL WINAPI "GetShortPathNameA" using
                    by reference cobdir2
                    by reference MFDIR
                    by reference LenShort
               MOVE SPACES TO COBDIR2
               PERFORM VARYING SizeDir FROM LENGTH COBDIR
                                       BY -1
                          UNTIL SIZEDIR = 1
                          OR (COBDIR(SizeDir:1) <> SPACE
                           AND COBDIR(SizeDir:1) <> ';')
                       CONTINUE
               END-PERFORM
               PERFORM VARYING SizeCil FROM LENGTH MFDIR  BY -1
                               UNTIL SizeCil = 1
                               OR (MFDIR (SizeCil:1) NOT = SPACE)
                       CONTINUE
               END-PERFORM
               STRING COBDIR(1:SizeDir)
                      ';'
                      MFDIR(1:SizeCil)  DELIMITED BY SIZE
                      INTO COBDIR2
               MOVE COBDIR2 TO COBDIR
               MOVE SPACES  TO COBDIR2
               DISPLAY COBDIR UPON ENVIRONMENT-VALUE
           ELSE
               CALL 'utils.lbr'
               ON EXCEPTION
                   CONTINUE
               END-CALL
           END-IF

           CALL "CBL_CREATE_DIR" USING PATH-MD

           DISPLAY "COBPATH" UPON ENVIRONMENT-NAME
           DISPLAY COBDIR UPON ENVIRONMENT-VALUE

           MOVE "cil" TO PATH-MD
           IF   COBWARE (1: 1) NOT = "/"
                DISPLAY "COBDIR" UPON ENVIRONMENT-NAME
                DISPLAY COBDIR UPON ENVIRONMENT-VALUE
           END-IF

           IF   PASTA-D NOT = SPACES
                IF   PASTA-D (2: 1) = ":"
                     MOVE PASTA-D (1: 1) TO NEW-DRIVE
                     CALL "PC_SET_DRIVE"  USING NEW-DRIVE
                                      RETURNING RETURN-CODE
                     IF   RETURN-CODE = 0
                          MOVE PASTA-D (3: ) TO NEW-DIRECTORY
                          INSPECT NEW-DIRECTORY
                                  CONVERTING X"20" TO X"00"
                          CALL "CBL_CHANGE_DIR" USING NEW-DIRECTORY
                                              RETURNING RETURN-CODE
                     END-IF
                     IF   RETURN-CODE NOT = 0
                          MOVE SPACES TO CWSEND-MSG
                          INSPECT NEW-DIRECTORY
                                  CONVERTING X"00" TO SPACE
                          STRING 'Pasta "'        DELIMITED BY SIZE
                                 NEW-DRIVE ':'    DELIMITED BY SPACE
                                 NEW-DIRECTORY    DELIMITED BY SPACE
                                 '"_n∆o_existe.'  DELIMITED BY SPACE
                                             INTO CWSEND-MSG
                          CALL CWSEND USING PARAMETROS-CWSEND
                          STOP RUN
                     ELSE
                          CALL "CBL_CREATE_DIR" USING PATH-MD
                     END-IF
                ELSE
                     MOVE PASTA-D TO NEW-DIRECTORY
                     INSPECT NEW-DIRECTORY
                             CONVERTING X"20" TO X"00"
                     CALL "CBL_CHANGE_DIR" USING NEW-DIRECTORY
                                       RETURNING RETURN-CODE
                     IF   RETURN-CODE NOT = 0
                          INSPECT OLD-DIRECTORY
                                  CONVERTING X"00" TO SPACE
                          MOVE SPACES TO CWSEND-MSG
                          STRING 'Pasta "'        DELIMITED BY SIZE
                                 NEW-DIRECTORY    DELIMITED BY SPACE
                                 '"_n∆o_existe.'  DELIMITED BY SPACE
                                      INTO CWSEND-MSG
                          CALL CWSEND USING PARAMETROS-CWSEND
                          STOP RUN
                     ELSE
      *                   CALL "CBL_CREATE_DIR" USING PATH-MD
                          call "system" using
          "if ! test -d cil && ! test -d CIL; then mkdir cil; fi" x"00"
                     END-IF
                END-IF
           END-IF.

       FIM-ATALHO. EXIT.

       check-ware.

           if   cobware not = spaces
                perform varying i from length of cobware
                        by -1 until cobware(i: 1) not = space
                   continue
                end-perform
                if cobware(i: 1) = "\" or "/"
                   add  1   to i
                   move "." to cobware(i: 1)
                end-if
           end-if.

       fim-check-ware. exit.
