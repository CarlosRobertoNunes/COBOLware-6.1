           MOVE LENGTH OF SP2-MS-DATA TO SP2-MS-VAR-LEN
           MOVE 4030   TO SP2-GD-VAR-LEN
           MOVE 4000   TO SP2-GD-ID-LEN
           MOVE 4000   TO SP2-RD-BASE-LEN
           MOVE +80    TO SP2-SET-VBX
           MOVE 300    TO SP2-MS-VAR-LEN
                          SP2-BF-LEN
           MOVE 60     TO SP2-FO-VAR-LEN
32767 *    MOVE 32756  TO SP2-MD-VAR-LEN
32767      MOVE 0      TO SP2-MD-VAR-LEN
           MOVE 34     TO SP2-MD-OPTV-LEN
                          SP2-MD-TEXT-LEN
           MOVE LENGTH OF SP2-PD-DESCRIPTION  TO SP2-PD-DESC-LEN
           MOVE LENGTH OF SP2-PD-TITLE        TO SP2-PD-TITLE-LEN
           MOVE LENGTH OF SP2-PD-CURS-KEYS    TO SP2-PD-CURS-KEY-LEN
           MOVE LENGTH OF SP2-PD-CTRL-KEYS    TO SP2-PD-CTRL-KEY-LEN
           MOVE LENGTH OF SP2-PD-MSG-TEXT     TO SP2-PD-MSG-TEXT-LEN
           MOVE LENGTH OF SP2-PD-USER-DATA    TO SP2-PD-USER-LEN
           MOVE LENGTH OF SP2-PD-HELP-KEYWORD TO SP2-PD-HELP-LEN
           COMPUTE SP2-PD-VAR-LEN = LENGTH OF SP2-PD-DESCRIPTION
                                  + LENGTH OF SP2-PD-TITLE
                                  + LENGTH OF SP2-PD-CURS-KEYS
                                  + LENGTH OF SP2-PD-CTRL-KEYS
                                  + LENGTH OF SP2-PD-MSG-TEXT
                                  + LENGTH OF SP2-PD-USER-DATA
                                  + LENGTH OF SP2-PD-HELP-KEYWORD
