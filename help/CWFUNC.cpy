       01                                              VALUE SPACES.
           05 FUNCAO                       PIC  X(001).
              88 ALTERACAO                             VALUE "A".
              88 CONSULTA                              VALUE "C".
              88 EXCLUSAO                              VALUE "E".
              88 INCLUSAO                              VALUE "I".
              88 FINALIZAR                             VALUE "F" "V".
              88 PARAR                                 VALUE "F".
           05                             PIC  X(001).
              88 PAGE-UP-ON                            VALUE SPACE.
              88 PAGE-UP-OFF                           VALUE 'U'.
           05                             PIC  X(001).
              88 PAGE-DOWN-ON                          VALUE SPACE.
              88 PAGE-DOWN-OFF                         VALUE 'D'.
           05                             PIC  X(001).
              88 READY-ON                              VALUE 'R'.
              88 READY-OFF                             VALUE SPACE.
           05 COMANDO                     PIC  X(001).
              88 ABORTAR                               VALUE "C".
              88 EFETIVAR                              VALUE "O".
