
           PERFORM TEST AFTER UNTIL COMANDO = "C" OR "O"
               EVALUATE TRUE
                   WHEN ALTERACAO
                        EXEC COBOLware Send
                             Message "Confirma alteraá∆o ?"
                             Option 2
                             Caption(1) "   ~Ok___"
                             Caption(2) "~Cancelar"
                             OPTION-CHAR;COMANDO
                        END-EXEC
                   WHEN EXCLUSAO
                        EXEC COBOLware Send
                             Message "Confirma exclus∆o ?"
                             Option 2
                             Caption(1) "   ~Ok___"
                             Caption(2) "~Cancelar"
                             OPTION-CHAR;COMANDO
                        END-EXEC
                   WHEN INCLUSAO
                        EXEC COBOLware Send
                             Message "Confirma inclus∆o ?"
                             Option 2
                             Caption(1) "   ~Ok___"
                             Caption(2) "~Cancelar"
                             OPTION-CHAR;COMANDO
                        END-EXEC
               END-EVALUATE
           END-PERFORM.
