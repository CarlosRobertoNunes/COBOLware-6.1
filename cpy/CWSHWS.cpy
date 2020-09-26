       01  ATALHOS-DATA.
           05 LenShort         COMP-5 pic  9(004) VALUE 255.
           05 MFDIR                   PIC  X(255) VALUE SPACES.
           05 SP2DIR                  PIC  X(255) VALUE SPACES.
           05 TEMP                    PIC  X(255) VALUE SPACES.
           05 PATH-MD                 PIC  X(255) VALUE SPACES.
           05 RESULTADO        COMP-X PIC  9(002) VALUE 0.
           05 PARAMETRO.
              10 SW            COMP-X PIC  9(002) OCCURS 26.
           05 COBSW                   PIC  X(030) VALUE SPACES.
           05 MODO                    PIC  X(012) VALUE SPACES.
           05 CWSEND                  PIC  X(255) VALUE SPACES.
           05 NOFRAME                 PIC  9(001) VALUE 0.
           05 NEW-DRIVE               PIC  X(001) VALUE SPACE.
           05 NEW-DIRECTORY           PIC  X(050) VALUE SPACES.
           05 PASTA-D                 PIC  X(080) VALUE SPACES.
           05 COBWARE                 PIC  X(255) VALUE SPACES.
           05 LINHA-COMANDO2          PIC  X(255) VALUE SPACES.
           05 OLD-DRIVE               PIC  X(001) VALUE SPACE.
           05 OLD-DIRECTORY           PIC  X(050) VALUE SPACES.
           05 SIZE-OLD-DIR            PIC  9(002) COMP-X VALUE 50.
           05 COBDIR                  PIC  X(255) VALUE SPACES.
           05 COBDIR2                 PIC  X(255) VALUE SPACES.
           05 SEPARA                  PIC  X(001) VALUE SPACE.
           05 JUNTA                   PIC  X(001) VALUE SPACE.
           05 BARRA                   PIC  X(001) VALUE SPACE.
           05 CIL-C                   PIC  X(080) VALUE SPACES.
           05 flag                    PIC  9(003) VALUE 0.
           05 I2                      PIC  9(003) VALUE 0.
           05 SizeDir                 PIC  9(003) VALUE 0.
           05 SizeCil                 PIC  9(003) VALUE 0.
