           IF   PAGE-UP-ON
                EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                          LINE 23 COLUMN 03 WIDTH 15
                       CAPTION " pgup-~Anterior "
                           KEY PAGE-UP TAB-OFF
                END-EXEC
           END-IF
           IF   PAGE-DOWN-ON
                EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                          LINE 23 COLUMN 18 WIDTH 14
                       CAPTION " pgdn-~Pr¢ximo "
                           KEY PAGE-DOWN TAB-OFF
                END-EXEC
           END-IF

           EVALUATE TRUE
               WHEN READY-OFF
                    CONTINUE
               WHEN ALTERACAO
                    EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                              LINE 23 COLUMN 32 WIDTH 7
                              CAPTION " a~Ltera "
                              KEY F2 TAB-OFF
                    END-EXEC
               WHEN EXCLUSAO
                    EXEC COBOLware OBJECT PUSH-BUTTON SMALL
                              LINE 23 COLUMN 32 WIDTH 8
                              CAPTION " ~Exclui "
                              KEY F2 TAB-OFF
                    END-EXEC
           END-EVALUATE
