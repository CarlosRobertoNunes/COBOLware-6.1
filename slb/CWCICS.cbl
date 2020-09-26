      $Set CALLFH"FHREDIR" NoOSVS
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWCICS.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  06/08/2008.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simulador das APIs do CICS                   *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT TDTS ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS TDTS-ITEM
                  ALTERNATE KEY IS TDTS-POINTER = TDTS-FLAG
                                                  TDTS-ITEM
                  RESERVE NO ALTERNATE AREA
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-TDTS.

           SELECT LOG ASSIGN    TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  RESERVE NO ALTERNATE AREA
                  FILE STATUS   IS FS-LOG
                  LOCK MODE     IS EXCLUSIVE.

           SELECT FCT ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RESERVE NO ALTERNATE AREA
                  RECORD  KEY   IS FCT-FILE
                  ALTERNATE KEY IS FCT-KSDS =
                                   FCT-FILESERVER
                                   FCT-BASEDSNAME
                  ALTERNATE KEY IS FCT-BASENAME   WITH DUPLICATES
                  ALTERNATE KEY IS FCT-BASEDSNAME WITH DUPLICATES
                  ALTERNATE KEY IS FCT-DSNAME
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-FCT.

           SELECT TCT ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RESERVE NO ALTERNATE AREA
                  RECORD  KEY   IS TCT-KEY
                  ALTERNATE KEY IS TERMINAL-ID =
                                   TCT-USERID
                                   TCT-TRMID
                  LOCK MODE     IS MANUAL
                  LOCK ON MULTIPLE RECORDS
                  FILE STATUS   IS FS-TCT.

           SELECT TASKS   ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RESERVE NO ALTERNATE AREA
                  RECORD  KEY   IS TASKS-TASK
                  LOCK MODE     IS MANUAL
                  LOCK ON MULTIPLE RECORD
                  FILE STATUS   IS FS-TASKS.

           SELECT PCT ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RESERVE NO ALTERNATE AREA
                  RECORD  KEY   IS PCT-KEY
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FS-PCT.

           SELECT STARTS  ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RESERVE NO ALTERNATE AREA
                  RECORD  KEY   IS STARTS-KEY
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FS-STARTS.

           SELECT TASKCH ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RESERVE NO ALTERNATE AREA
                  RECORD  KEY   IS TASKCH-TASK
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FS-TASKCH.

           SELECT PPT ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RESERVE NO ALTERNATE AREA
                  RECORD  KEY   IS PPT-PROGRAM
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FS-PPT.

      $Set IdxFormat"14" DataCompress"1" KeyCompress"7"

           SELECT ACCUM ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS ACCUM-NUMBER
                  RESERVE NO ALTERNATE AREA
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-ACCUM.

           SELECT BR ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS BR-ID
                  RESERVE NO ALTERNATE AREA
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-BR.

           SELECT flags ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS flag-ID
                  RESERVE NO ALTERNATE AREA
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-flags.

           SELECT LOCKEDS ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RESERVE NO ALTERNATE AREA
                  RECORD  KEY   IS LOCKEDS-KEY
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-LOCKEDS.

       DATA DIVISION.
       FILE SECTION.

       FD  TDTS
           LABEL RECORD IS STANDARD
           RECORD VARYING FROM 4 TO 20000 DEPENDING ON SZ-TDTS
           VALUE OF FILE-ID IS LB-TDTS.

       01  TDTS-REG.
           05 TDTS-FLAG           PIC  X(002) COMP-X.
           05 TDTS-ITEM           PIC S9(004) COMP-5.
           05 TDTS-DATA.
              10 TDTS-BYTE        PIC  X(001)
               OCCURS 1 TO 20000 DEPENDING ON CWCICS-LENGTH.

       FD  LOG
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-LOG.

       01  LOG-REG              PIC X(12).

       FD  FCT
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-FCT.

       01  FCT-RECORD.
           05 fct-INQUIRE-FILE.
              07 fct-ACCESSMETHOD          PIC  9(004) comp-5.
                 88 fct-cvda-REMOTE                VALUE     4.
                 88 fct-cvda-SFS                   VALUE     3.       *> +
                 88 fct-cvda-DB2                   VALUE   623.
              07 fct-ADD                   PIC  9(004) comp-5.
                 88 fct-cvda-ADD                   VALUE   291.       *> + Yes
                 88 fct-cvda-NOTADDABLE            VALUE    42.       *>   No
              07 FCT-BASENAME              PIC  x(032).
              07 FCT-BASEDSNAME            PIC  x(044).
              07 fct-BLOCKFORMAT           PIC  9(004) comp-5.
                 88 fct-cvda-UNBLOCKED             VALUE    17.       *> +
                 88 fct-cvda-BLOCKED               VALUE    16.
              07 FCT-BLOCKKEYLEN           PIC  9(008) comp-5.
              07 FCT-BLOCKSIZE             PIC  9(008) comp-5.
              07 fct-BROWSE                PIC  9(004) comp-5.
                 88 fct-cvda-BROWSABLE             VALUE    39.       *> + Yes
                 88 fct-cvda-NOTBROWSABLE          VALUE    40.       *>   No
              07 fct-DELETE                PIC  9(004) comp-5.
                 88 fct-cvda-DELETABLE             VALUE    43.       *> + Yes
                 88 fct-cvda-NOTDELETABLE          VALUE    44.       *>   No
              07 fct-DISPOSITION           PIC  9(004) comp-5.
                 88 fct-cvda-OLD                   VALUE    26.            Share
                 88 fct-cvda-SHARE                 VALUE    27.       *> + Old
              07 FCT-DSNAME                PIC  x(044).
              07 fct-EMPTYSTATUS           PIC  9(004) comp-5.
                 88 fct-cvda-EMPTYREQ              VALUE    31.
                 88 fct-cvda-NOTEMPTY              VALUE   211.       *> +
              07 fct-ENABLESTATUS          PIC  9(004) comp-5.
                 88 fct-cvda-DISABLED              VALUE    24.
                 88 fct-cvda-DISABLING             VALUE    25.
                 88 fct-cvda-ENABLED               VALUE    23.       *> +
                 88 fct-cvda-UNENABLING            VALUE    34.
              07 fct-EXCLUSIVE             PIC  9(004) comp-5.
                 88 fct-cvda-EXCTL                 VALUE    48.       *>   Yes
                 88 fct-cvda-NOEXCTL               VALUE    49.
                 88 fct-cvda-NOTAPPLIC             VALUE     1.       *> +
              07 FCT-FILE                  PIC  x(008).
              07 FCT-FILESERVER            PIC  x(032).
              07 fct-FWDRECSTATUS          PIC  9(004) comp-5.
                 88 fct-cvda-FWDRECOVABLE          VALUE   354.       *>   Yes
                 88 fct-cvda-NOTFWDRCVBLE          VALUE   361.       *> + No
              07 FCT-INDEXNAME             PIC  x(032).
              07 fct-JOURNALNUM            PIC  9(004) comp-5.
                 88 cvda-JOurnalNo                 value     0.       *>   No
              07 FCT-KEYLENGTH             PIC  9(008) comp-5.                *>   1 - 255
              07 FCT-KEYPOSITION           PIC  9(008) comp-5.
              07 fct-LSRPOOLID             PIC  9(008) comp-5.                *> + 1 - 8
                 88 cvda-LsrpoolidNone             value     0.       *>   None
              07 FCT-MAXNUMRECS            PIC  9(008) comp-5.
              07 fct-OBJECT                PIC  9(004) comp-5.
                 88 fct-cvda-BASE                  VALUE    10.       *> +
                 88 fct-cvda-PATH                  VALUE    11.
              07 fct-OPENSTATUS            PIC  9(004) comp-5.
                 88 fct-cvda-CLOSED                VALUE    19.
                 88 fct-cvda-OPEN                  VALUE    18.
                 88 fct-cvda-CLOSING               VALUE    21.
              07 fct-READ                  PIC  9(004) comp-5.
                 88 fct-cvda-READABLE              VALUE    35.       *>   Yes
                 88 fct-cvda-NOTREADABLE           VALUE    36.       *>   No
              07 fct-RECORDFORMAT          PIC  9(004) comp-5.
                 88 fct-cvda-FIXED                 VALUE    12.       *>   F
                 88 fct-cvda-VARIABLE              VALUE    13.       *>   V
              07 FCT-RECORDSIZE            PIC  9(008) comp-5.                *>   1 - 32767
              07 fct-RECOVSTATUS           PIC  9(004) comp-5.
                 88 fct-cvda-NOTRECOVABLE          VALUE    30.       *> + None
                 88 fct-cvda-RECOVERABLE           VALUE    29.       *>   All
                 88 fct-cvda-BACKOUT               VALUE    29.       *>   Backoutonly
              07 fct-RELTYPE               PIC  9(004) comp-5.
                 88 fct-cvda-BLK                   VALUE    47.
                 88 fct-cvda-DEC                   VALUE    46.
                 88 fct-cvda-HEX                   VALUE    45.
                 88 fct-cvda-NOTAPPLIC2            VALUE     1.
              07 FCT-REMOTENAME            PIC  x(008).
              07 FCT-REMOTESYSTEM          PIC  x(004).
              07 FCT-STRINGS               PIC  9(008) comp-5.                *>   1 - 255
              07 fct-TABLE                 PIC  9(004) comp-5.
                 88 fct-cvda-CICSTABLE             VALUE   101.       *>   Cics
                 88 fct-cvda-NOTTABLE              VALUE   100.       *>   No
                 88 fct-cvda-CF                    VALUE   100.       *>   CF
                 88 fct-cvda-USERTABLE             VALUE   102.       *> + User
              07 fct-TYPE                  PIC  9(004) comp-5.
                 88 fct-cvda-ESDS                  VALUE     5.       *>   ESDS
                 88 fct-cvda-KEYED                 VALUE     8.       *>   ALTINDEX
                 88 fct-cvda-KSDS                  VALUE     6.       *>   KSDS
                 88 fct-cvda-RRDS                  VALUE     7.       *>   RRDS
              07 fct-UPDATE                PIC  9(004) comp-5.
                 88 fct-cvda-NOTUPDATABLE          VALUE    38.       *>   No
                 88 fct-cvda-UPDATABLE             VALUE    37.       *>   Yes
           05 FCT-NOINQUIRE-FILE           PIC  X(1400).

       FD  TCT
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-TCT.

       01  TCT-RECORD.
           05 TCT-KEY.
              10 TCT-TRMID.
                 15 TCT-TRMID-BYTE PIC X(02) OCCURS 2.
           05 TCT-USERID           PIC X(30).

       FD  TASKS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-TASKS.

       01  TASKS-RECORD.
           06 TASKS-ABSTIME                    PIC  9(008) comp-5.
           06 tasks-CMDSEC                     pic  9(004) comp-5.
              88 tasks-cvda-CMDSECNO                       VALUE  205.
              88 tasks-cvda-CMDSECYES                      VALUE  206.
           06 TASKS-CURRENTPROG                pic  x(008).
           06 tasks-DTB                        pic  9(004) comp-5.
              88 tasks-cvda-COMMIT                         VALUE  208.
              88 tasks-cvda-BACKOUT                        VALUE   29.
              88 tasks-cvda-WAIT                           VALUE  340.
           06 TASKS-DTIMEOUT                   pic  9(008) comp-5.
           06 tasks-DUMPING                    pic  9(004) comp-5.
              88 tasks-cvda-TRANDUMP                       VALUE  186.
              88 tasks-cvda-NOTRANDUMP                     VALUE  187.
           06 tasks-FACILITY                   PIC  X(004).
           06 tasks-FACILITYTYPE               pic  9(004) comp-5.
              88 tasks-cvda-TERM                           VALUE  234.
              88 tasks-cvda-DEST                           VALUE  235.
           06 tasks-ISOLATEST                  pic  9(004) comp-5.
              88 tasks-cvda-ISOLATE                        VALUE  658.
              88 tasks-cvda-NOISOLATE                      VALUE  657.
           06 TASKS-PRIORITY                   pic  9(008) comp-5.
           06 TASKS-PROCESSID                  pic  9(008) comp-5.
           06 TASKS-PROFILE                    pic  X(008).
           06 TASKS-PROGRAM                    pic  X(008).
           06 tasks-PURGEABILITY               pic  9(004) comp-5.
              88 tasks-cvda-PURGEABLE                      VALUE  160.
              88 tasks-cvda-NOTPURGEABLE                   VALUE  161.
           06 TASKS-REMOTENAME                 pic  X(004).
           06 TASKS-REMOTESYSTEM               PIC  x(004).
           06 tasks-RESSEC                     pic  9(004) comp-5.
              88 tasks-cvda-RESSECNO                       VALUE  202.
              88 tasks-cvda-RESSECYES                      VALUE  203.
           06 tasks-ROUTING                    pic  9(004) comp-5.
              88 tasks-cvda-DYNAMIC                        VALUE  178.
              88 tasks-cvda-STATIC                         VALUE  179.
           06 TASKS-RTIMEOUT                   pic  9(008) comp-5.
           06 TASKS-RUNAWAY                    pic  9(008) comp-5.
              88 tasks-cvda-DISPATCHABLE                   VALUE  228.
              88 tasks-cvda-RUNNING                        VALUE  229.
              88 tasks-cvda-SUSPENDED                      VALUE  231.
           06 tasks-RUNSTATUS                  pic  9(004) comp-5.
           06 tasks-SCRNSIZE                   pic  9(004) comp-5.
              88 tasks-cvda-ALTERNATE                      VALUE  197.
              88 tasks-cvda-DEFAULT                         VALUE  198.
           06 TASKS-STARTCODE                  pic  X(002).
           06 tasks-STORAGECLEAR               pic  9(004) comp-5.
              88 tasks-cvda-CLEAR                          VALUE  640.
              88 tasks-cvda-NOCLEAR                        VALUE  641.
           06 TASKS-SUSPENDVALUE               pic  X(008).
           06 TASKS-TASK                       pic s9(007) COMP-3.
           06 tasks-TASKDATAKEY                pic  9(004) comp-5.
              88 tasks-cvda-CICSDATAKEY                    VALUE  379.
              88 tasks-cvda-USERDATAKEY                    VALUE  380.
           06 tasks-TASKDATALOC                pic  9(004) comp-5.
              88 tasks-cvda-ANY                            VALUE  158.
              88 tasks-cvda-BELOW                          VALUE  159.
           06 TASKS-TCLASS                     pic  9(008) COMP-5.
           06 tasks-TRACING                    pic  9(004) comp-5.
              88 tasks-cvda-SPRSTRACE                      VALUE  175.
              88 tasks-cvda-STANTRACE                      VALUE  176.
              88 tasks-cvda-SPECTRACE                      VALUE  177.
           06 TASKS-TRANCLASS                  pic  X(008).
           06 TASKS-TRANPRIORITY               pic  9(008) COMP-5.
           06 TASKS-TRANSACTION                pic  X(004).
           06 TASKS-TRPROF                     pic  X(008).
           06 TASKS-TWASIZE                    pic  9(008) COMP-5.
           06 tasks-UOWSTATE                   pic  9(004) comp-5.
              88 tasks-cvda-INDOUBT                        VALUE  620.
              88 tasks-cvda-INFLIGHT                       VALUE  621.
              88 tasks-cvda-WAITFORGET                     VALUE  622.
           06 TASKS-USERID                     PIC  X(008).

       COPY PCT.
       COPY STARTS.

       FD  TASKCH
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-TASKCH.

       01  TASKCH-RECORD.
            05 TASKCH-TASK COMP-3 Pic s9(07).
            05 TASKCH-PURGED      Pic  9(01).
            05 TASKCH-PRIORITY    pic  9(008) comp-5.

       COPY PPT.

       FD  ACCUM
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-TS.

       01  ACCUM-REG.
           05 ACCUM-NUMBER       PIC S9(008) COMP-5.
           05 ACCUM-CHARACTERS   PIC X(2000).
           05 ACCUM-ATTRIBUTES   PIC X(2000).

       FD  BR
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-BR.

       01  BR-REG.
           05 BR-ID.
              06 BR-SYSID                     PIC  X(004).
              06 BR-REQID                     PIC  9(008) comp-5.
              06 BR-DATASET                   PIC  X(008).
           05 BR-RIDFLD                       PIC  X(255).
           05 BR-KEY                          PIC  X(255).
           05 BR-RIDFLD-RESET                 PIC  X(255).
           05 BR-KEY-RESET                    PIC  X(255).

       FD  flags
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-flags.

       01  flags-REG.
           05 flag-ID             PIC 9(008) comp-5.
           05 flag-data           PIC X(008).
           05 flag-AidKeys        PIC X(035).

       FD  LOCKEDS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-LOCKEDS.

       01  LOCKEDS-RECORD.
           05 LOCKEDS-KEY.
              10 LOCKEDS-SYSID    PIC X(004).
              10 LOCKEDS-DATASET  PIC X(008).
           05 LOCKEDS-RECORDS     PIC 9(008) comp-5.
           05 LOCKEDS-WRITE       PIC 9(008) comp-5.
           05 LOCKEDS-DELETE      PIC 9(008) comp-5.
           05 LOCKEDS-REWRITE     PIC 9(008) comp-5.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 ANIMATE            PIC  X(001) value SPACE.
           05 CWL         comp-x PIC  9(004) value 0.
           05 ISOLATEST          pic  9(004) comp-5.
              88 ISOLATE                            VALUE  658.
              88 NOISOLATE                          VALUE  657.
           05 VEZ                PIC  9(001)        VALUE 1.
           05                    PIC  9(001)        VALUE 0.
              88 close-all                          value 1.
              88 NO-close-all                       value 0.
           05 FLAG-SYNC          PIC  9(001)        VALUE 0.
           05 START-EIBAID       PIC  X(001)        VALUE LOW-VALUE.
           05 START-TECLA        PIC  9(002) COMP-X VALUE 0.
           05 CURRENTPROG        PIC  X(008)        VALUE SPACES.
           05 NEWNAME            PIC  X(255)        VALUE SPACES.
           05 OLD-DSNAME         PIC  X(255)        VALUE SPACES.
           05 OLD-FILESERVER     PIC  X(255)        VALUE SPACES.
           05 LB1                PIC  X(255)        VALUE SPACES.
           05 LB2                PIC  X(255)        VALUE SPACES.
           05 queue-lower        PIC  X(008)        VALUE SPACES.
           05 LAST-FILE          PIC  X(008)        VALUE SPACES.
           05 in-DATASET         PIC  X(008)        VALUE SPACES.
           05 in-FILE            PIC  X(008)        VALUE SPACES.
           05 QT                 PIC  X(002)        VALUE SPACE.
           05 CT                 PIC  9(001)        VALUE 0.
           05 KEYLENGTH          PIC  9(008) comp-5 VALUE 0.
           05 SAVELENGTH         PIC  9(008) comp-5 VALUE 0.
           05 KEYLENGTH-FCT      PIC  9(008) comp-5 VALUE 0.
           05 SAVESET            POINTER.
           05 CENTURY            PIC  9(002) VALUE 20.
           05 FS-FCT                         VALUE "00".
              06 FS1-FCT         PIC  9(001).
              06 FS2-FCT         PIC  9(002) comp-x.
           05 LB-FCT             PIC  X(255) VALUE SPACES.
           05 LB-FCT-ANT         PIC  X(255) VALUE SPACES.
           05 FS-TCT             PIC  X(002) VALUE "00".
           05 LB-TCT             PIC  X(255) VALUE "cicsTCT".
           05 FS-TASKS           PIC  X(002) VALUE "00".
           05 LB-TASKS           PIC  X(255) VALUE "cicsTASKS".
           05 FS-PCT             PIC  X(002) VALUE '00'.
           05 LB-PCT             PIC  X(255) VALUE "cicsPCT".
           05 FS-PPT             PIC  X(002) VALUE '00'.
           05 LB-PPT             PIC  X(255) VALUE "cicsPPT".
           05 SZ-STARTS          pic  9(004) comp-5.
           05 FS-STARTS          PIC  X(002) VALUE "00".
           05 LB-STARTS          PIC  X(255) VALUE SPACES.
           05 FS-TASKCH          PIC  X(002) VALUE "00".
           05 LB-TASKCH          PIC  X(255) VALUE "cicsCHANGE".
           05 DIRSEP             PIC  X(001) VALUE SPACES.
           05 UNIX               PIC  X(001) VALUE SPACES.
           05 PATH               PIC X(2000) VALUE SPACES.
      *    05 START-CHARACTERS   PIC X(2000) VALUE SPACES.
      *    05 START-ATTRIBUTES   PIC X(2000) VALUE SPACES.
           05 TECLA              PIC  9(003) VALUE 0. COPY CWKEYS.
           05 TECLA-EDIT         PIC  9(003) VALUE 0. COPY CWEDIT.
           05 CARACTER           PIC  X(001) VALUE SPACE.
           05 OPENED-ACCUM       PIC  9(001) VALUE 0.
           05 OPENED-FCT         PIC  9(001) VALUE 0.
           05 COUNT-ACCUM        PIC S9(008) COMP-5 VALUE 0.
           05 CURRENT-ACCUM      PIC S9(008) COMP-5 VALUE 0.
           05 NTEST              PIC S9(001) VALUE 0.
           05 NEGAT              PIC  9(001) VALUE 0.
           05 VINTE              PIC S9(018) VALUE 0.
           05 REDEFINES VINTE.
               10 DVINTE         PIC  9(001) OCCURS 18.
           05 DATEFORM           PIC  X(008) VALUE 'DDMMYYYY'.
           05 DATEMASK           PIC  X(008) VALUE 'DDMMYYYY'.
           05 FULLMASK           PIC  X(010) VALUE SPACES.
           05 TXT                PIC  X(020) VALUE spaces.
           05 TRMID              PIC  X(004) VALUE spaces.
           05 TASKN       COMP-3 Pic s9(007) VALUE 0.
           05 RSRCE              PIC  X(008) VALUE spaces.
           05 ABCODE             PIC  X(004) VALUE spaces.
           05 FCI         comp-x PIC  9(002) value 0.
           05 EXTDS              PIC  X(001) VALUE X"00".
           05 MAPCOLUMN          PIC  9(002) value 0.
           05 MAPHEIGHT          PIC  9(002) value 0.
           05 MAPLINE            PIC  9(002) value 0.
           05 MAPWIDTH           PIC  9(002) value 0.
           05 LAST-PROGRAM       PIC  X(008) value "COBWARE".

           05 APPLID             PIC  X(008) value "COBWARE".
           05 NETNAME            PIC  X(008) value "COBWARE".
           05 NEXTTRANSID        PIC  X(004) value spaces.
           05 OPERKEYS           PIC  X(008) value spaces.
           05 OPID               PIC  X(003) value spaces.
           05 OPSECURITY         PIC  X(003) value spaces.
           05 PRINSYSID          PIC  X(004) value spaces.
           05 QNAME              PIC  X(008) value spaces.
           05 SIGDATA            PIC  X(004) value spaces.
           05 SYSID              PIC  X(004) value 'DATA'.
           05 TCTUALENG   comp-x PIC  9(004) value 0.

           05 I           COMP-5 PIC S9(004) VALUE 1.
           05 Y           COMP-5 PIC S9(004) VALUE 1.
           05 P           COMP-5 PIC S9(004) VALUE 1.
           05 TP          COMP-5 PIC S9(004) VALUE 1.
           05 IX          COMP-5 PIC S9(004) VALUE 1.
           05 LY          COMP-5 PIC S9(004) VALUE 0.
           05 flag        COMP-5 PIC  X(004) VALUE 1.
           05 STATUS-CODE COMP-X PIC  9(004) VALUE 0.
           05 BARRA-T            PIC  X(001) VALUE "/".
           05 FFSERVER           PIC  X(015) VALUE spaces.
           05 FS-TDTS            PIC  X(002) VALUE "00".
           05 LB-TDTS            PIC  X(255) VALUE SPACES.
           05 FS-LOG             PIC  X(002) VALUE "00".
           05 LB-LOG             PIC  X(255) VALUE SPACES.
           05 FS-BR              PIC  X(002) VALUE "00".
           05 LB-BR              PIC  X(255) VALUE "cicsBR$".
           05 FS-flags           PIC  X(002) VALUE "00".
           05 LB-flags           PIC  X(255) VALUE "cicsFLAGS$".
           05 FS-LOCKEDS         PIC  X(002) VALUE "00".
           05 LB-LOCKEDS         PIC  X(255) VALUE "cicsLOCKS$".
           05 SZ-TDTS            PIC S9(004) COMP-5.
           05 FS-ACCUM           PIC  X(002) VALUE "00".
           05 LB-ACCUM           PIC  X(255) VALUE "cicsACCUM$".
           05 SCREEN-WIDTH       PIC S9(004) COMP-5.
           05 tentou             PIC S9(004) COMP-5.
           05 MSG                PIC  X(080).
           05 MAP                PIC  X(008).
           05 DS                 PIC  X(008) VALUE SPACES.
           05 FN                 PIC  X(002) VALUE SPACES.
              88 cwcics-asktime-abstime      VALUE X"4A02".
              88 cwcics-deleteq-td           VALUE X"0806".
              88 cwcics-deleteq-ts           VALUE X"0A06".
              88 cwcics-dfheib               VALUE X"0000".
              88 cwcics-addtask              VALUE X"FFFF".
              88 cwcics-inquire-file         VALUE X"4C02".
              88 cwcics-inquire-task         VALUE X"5E02".
              88 cwcics-readq-td             VALUE X"0804".
              88 cwcics-readq-ts             VALUE X"0A04".
              88 cwcics-receive-map          VALUE X"1802".
              88 cwcics-send-map             VALUE X"1804".
              88 cwcics-send-text            VALUE X"1806".
              88 cwcics-writeq-td            VALUE X"0802".
              88 cwcics-writeq-ts            VALUE X"0A02".
              88 cwcics-handle-abend         VALUE X"0E0E".
              88 cwcics-handle-aid           VALUE X"0206".
              88 cwcics-handle-condition     VALUE X"0204".
              88 cwcics-ignore-condition     VALUE X"020A".
              88 cwcics-SetFile              VALUE X"4C04".
              88 cwcics-SetTask              VALUE X"5E04".
           05 FN-N               PIC  9(004) COMP-X.
           05 CWCHAIN            PIC  X(008) VALUE SPACES.
           05 X91-RESULT         PIC  9(002) COMP-X VALUE 0.
           05 X91-FUNCTION       PIC  9(002) COMP-X VALUE 16.
           05 X91-PARAMETER      PIC  9(002) COMP-X VALUE 0.
           05 CICS-CURSOR.
              10 CICS-LIN        PIC  9(002).
              10 CICS-COL        PIC  9(002).
           05 CBL-READ-WRITE-SCR-CHARS-ATTR.
              10 SCREEN-POSITION.
                 15 ROW-NUMBER    PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-NUMBER PIC  9(002) COMP-X VALUE 0.
              10 CHARACTER-BUFFER PIC X(2000) VALUE SPACES.
              10 ATTRIBUTE-BUFFER PIC X(2000) VALUE SPACES.
              10 STRING-LENGTH    PIC  9(004) COMP-X VALUE 0.
           05 CWUSERID            PIC  X(030) VALUE SPACES.
           05 DEC-HEX             PIC  9(004) comp-x VALUE 0.
           05 REDEFINES DEC-HEX.
              10 DEC-BYTE OCCURS 2 PIC 9(2) COMP-X.
           05 HEX-TABLE.
              10 PIC X(32) VALUE "000102030405060708090A0B0C0D0E0F".
              10 PIC X(32) VALUE "101112131415161718191A1B1C1D1E1F".
              10 PIC X(32) VALUE "202122232425262728292A2B2C2D2E2F".
              10 PIC X(32) VALUE "303132333435363738393A3B3C3D3E3F".
              10 PIC X(32) VALUE "404142434445464748494A4B4C4D4E4F".
              10 PIC X(32) VALUE "505152535455565758595A5B5C5D5E5F".
              10 PIC X(32) VALUE "606162636465666768696A6B6C6D6E6F".
              10 PIC X(32) VALUE "707172737475767778797A7B7C7D7E7F".
              10 PIC X(32) VALUE "808182838485868788898A8B8C8D8E8F".
              10 PIC X(32) VALUE "909192939495969798999A9B9C9D9E9F".
              10 PIC X(32) VALUE "A0A1A2A3A4A5A6A7A8A9AAABACADAEAF".
              10 PIC X(32) VALUE "B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF".
              10 PIC X(32) VALUE "C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF".
              10 PIC X(32) VALUE "D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF".
              10 PIC X(32) VALUE "E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF".
              10 PIC X(32) VALUE "F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF".
           05 REDEFINES HEX-TABLE.
              10 HEX-BYTE OCCURS 255 PIC X(2).

       01  COMMANDS         PIC 9(2).
           88 CICS-SEND    VALUE 01.
           88 CICS-RECEIVE VALUE 02.

       01  AREA-ASKTIME.
           05 TASKDATE            PIC 9(006) VALUE 0.
           05 DAYCOUNT            PIC 9(008) VALUE 0.
           05 DIAS                PIC 9(003) VALUE 0.
           05 HORA                PIC 9(006) VALUE 0.
           05 OPTIME              PIC 9(016) VALUE 0.
           05 REDEFINES OPTIME.
              06 HOJE             PIC 9(008).
              06 AGORA            PIC X(008).
              06 REDEFINES AGORA.
                 07 HHMMSS        PIC 9(006).
                 07 CENTESIMOS    PIC 9(002).
           05 ABSTIME             PIC 9(015) VALUE 0.
           05 REDEFINES ABSTIME.
              06 SEGUNDOS         PIC 9(012).
              06 CENTESIMOS-ABS   PIC 9(002).
              06                  PIC 9(001).
           05 FORMATTIME.
              10 HH               PIC 9(002) VALUE 0.
              10 MIN              PIC 9(002) VALUE 0.
              10 SS               PIC 9(002) VALUE 0.
              10 TIMESEP          PIC  X(01) VALUE ":".
              10 YX               PIC  9(02) VALUE 4.
              10 LETRAS.
                 11               PIC  X(11) VALUE 'YYYYMMDDijk'.
                 11 DATESEP       PIC  X(01) VALUE "/".
              10 LETRAX           PIC  X(08) VALUE 'efghcdab'.
              10 DATESTRING.
                 11 YYYYMMDD.
                    12 YYYY       PIC 9(004) VALUE 0.
                    12 MM         PIC 9(002) VALUE 0.
                    12 DD         PIC 9(002) VALUE 0.
                 11 DDD           PIC 9(003) VALUE 0.
                 11               PIC X(001) VALUE '/'.
              10 W-TP-ANO         PIC  9(001).
                 88 ANO-BISSEXTO             VALUE  0.
              10 W-NADA           PIC  9(004).
              10 T-DMAX VALUE "312831303130313130313031".
                 15 DIA-MAX OCCURS 12 PIC  9(002).
              10 exts value 'intgntDLL'.
                 11 EXT PIC X(3) OCCURS 3.
              10 filename              pic x(200).
              10 file-details.
                 11 file-size          pic x(8) comp-x.
                 11 file-date.
                    12 file-day        pic x comp-x.
                    12 file-month      pic x comp-x.
                    12 file-year       pic x(2) comp-x.
                 11 file-time.
                    12 file-hours      pic x comp-x.
                    12 file-minutes    pic x comp-x.
                    12 file-seconds    pic x comp-x.
                    12 file-hundredths pic x comp-x.
       COPY CWACOR.
       COPY DFHAID.

       01  TABELA-FN.
           05 PIC X(02) VALUE X"0202".
           05 PIC X(20) VALUE "ADDRESS".
           05 PIC X(02) VALUE X"0204".
           05 PIC X(20) VALUE "HANDLE CONDITION".
           05 PIC X(02) VALUE X"0206".
           05 PIC X(20) VALUE "HANDLE AID".
           05 PIC X(02) VALUE X"0208".
           05 PIC X(20) VALUE "ASSIGN".
           05 PIC X(02) VALUE X"020A".
           05 PIC X(20) VALUE "IGNORE CONDITION".
           05 PIC X(02) VALUE X"020C".
           05 PIC X(20) VALUE "PUSH HANDLE".
           05 PIC X(02) VALUE X"020E".
           05 PIC X(20) VALUE "POP HANDLE".
           05 PIC X(02) VALUE X"0402".
           05 PIC X(20) VALUE "RECEIVE".
           05 PIC X(02) VALUE X"0404".
           05 PIC X(20) VALUE "SEND".
           05 PIC X(02) VALUE X"0406".
           05 PIC X(20) VALUE "CONVERSE".
           05 PIC X(02) VALUE X"0418".
           05 PIC X(20) VALUE "ISSUE ERASEAUP".
           05 PIC X(02) VALUE X"041E".
           05 PIC X(20) VALUE "ISSUE SIGNAL".
           05 PIC X(02) VALUE X"0420".
           05 PIC X(20) VALUE "ALLOCATE".
           05 PIC X(02) VALUE X"0422".
           05 PIC X(20) VALUE "FREE".
           05 PIC X(02) VALUE X"042C".
           05 PIC X(20) VALUE "WAIT CONVID".
           05 PIC X(02) VALUE X"042E".
           05 PIC X(20) VALUE "EXTRACT PROCESS".
           05 PIC X(02) VALUE X"0430".
           05 PIC X(20) VALUE "ISSUE ABEND".
           05 PIC X(02) VALUE X"0432".
           05 PIC X(20) VALUE "CONNECT PROCESS".
           05 PIC X(02) VALUE X"0434".
           05 PIC X(20) VALUE "ISSUE CONFIRMATION".
           05 PIC X(02) VALUE X"0436".
           05 PIC X(20) VALUE "ISSUE ERROR".
           05 PIC X(02) VALUE X"043E".
           05 PIC X(20) VALUE "EXTRACT ATTRIBUTES".
           05 PIC X(02) VALUE X"0602".
           05 PIC X(20) VALUE "READ".
           05 PIC X(02) VALUE X"0604".
           05 PIC X(20) VALUE "WRITE".
           05 PIC X(02) VALUE X"0606".
           05 PIC X(20) VALUE "REWRITE".
           05 PIC X(02) VALUE X"0608".
           05 PIC X(20) VALUE "DELETE".
           05 PIC X(02) VALUE X"060A".
           05 PIC X(20) VALUE "UNLOCK".
           05 PIC X(02) VALUE X"060C".
           05 PIC X(20) VALUE "STARTBR".
           05 PIC X(02) VALUE X"060E".
           05 PIC X(20) VALUE "READNEXT".
           05 PIC X(02) VALUE X"0610".
           05 PIC X(20) VALUE "READPREV".
           05 PIC X(02) VALUE X"0612".
           05 PIC X(20) VALUE "ENDBR".
           05 PIC X(02) VALUE X"0614".
           05 PIC X(20) VALUE "RESETBR".
           05 PIC X(02) VALUE X"0802".
           05 PIC X(20) VALUE "WRITEQ TD".
           05 PIC X(02) VALUE X"0804".
           05 PIC X(20) VALUE "READQ TD".
           05 PIC X(02) VALUE X"0806".
           05 PIC X(20) VALUE "DELETEQ TD".
           05 PIC X(02) VALUE X"0A02".
           05 PIC X(20) VALUE "WRITEQ TS".
           05 PIC X(02) VALUE X"0A04".
           05 PIC X(20) VALUE "READQ TS".
           05 PIC X(02) VALUE X"0A06".
           05 PIC X(20) VALUE "DELETEQ TS".
           05 PIC X(02) VALUE X"0C02".
           05 PIC X(20) VALUE "GETMAIN".
           05 PIC X(02) VALUE X"0C04".
           05 PIC X(20) VALUE "FREEMAIN".
           05 PIC X(02) VALUE X"0E02".
           05 PIC X(20) VALUE "LINK".
           05 PIC X(02) VALUE X"0E04".
           05 PIC X(20) VALUE "XCTL".
           05 PIC X(02) VALUE X"0E06".
           05 PIC X(20) VALUE "LOAD".
           05 PIC X(02) VALUE X"0E08".
           05 PIC X(20) VALUE "RETURN".
           05 PIC X(02) VALUE X"0E0A".
           05 PIC X(20) VALUE "RELEASE".
           05 PIC X(02) VALUE X"0E0C".
           05 PIC X(20) VALUE "ABEND".
           05 PIC X(02) VALUE X"0E0E".
           05 PIC X(20) VALUE "HANDLE ABEND".
           05 PIC X(02) VALUE X"1002".
           05 PIC X(20) VALUE "ASKTIME".
           05 PIC X(02) VALUE X"1004".
           05 PIC X(20) VALUE "DELAY".
           05 PIC X(02) VALUE X"1006".
           05 PIC X(20) VALUE "POST".
           05 PIC X(02) VALUE X"1008".
           05 PIC X(20) VALUE "START".
           05 PIC X(02) VALUE X"100A".
           05 PIC X(20) VALUE "RETRIEVE".
           05 PIC X(02) VALUE X"100C".
           05 PIC X(20) VALUE "CANCEL".
           05 PIC X(02) VALUE X"1202".
           05 PIC X(20) VALUE "WAIT EVENT".
           05 PIC X(02) VALUE X"1204".
           05 PIC X(20) VALUE "ENQ".
           05 PIC X(02) VALUE X"1206".
           05 PIC X(20) VALUE "DEQ".
           05 PIC X(02) VALUE X"1208".
           05 PIC X(20) VALUE "SUSPEND".
           05 PIC X(02) VALUE X"1402".
           05 PIC X(20) VALUE "WRITE JOURNALNUM".
           05 PIC X(02) VALUE X"1404".
           05 PIC X(20) VALUE "WAIT JOURNALNUM".
           05 PIC X(02) VALUE X"1602".
           05 PIC X(20) VALUE "SYNCPOINT".
           05 PIC X(02) VALUE X"1802".
           05 PIC X(20) VALUE "RECEIVE MAP".
           05 PIC X(02) VALUE X"1804".
           05 PIC X(20) VALUE "SEND MAP".
           05 PIC X(02) VALUE X"1806".
           05 PIC X(20) VALUE "SEND TEXT".
           05 PIC X(02) VALUE X"1812".
           05 PIC X(20) VALUE "SEND CONTROL".
           05 PIC X(02) VALUE X"2002".
           05 PIC X(20) VALUE "BIF DEEDIT".
           05 PIC X(02) VALUE X"4204".
           05 PIC X(20) VALUE "INQUIRE AUTINSTMODEL".
           05 PIC X(02) VALUE X"4210".
           05 PIC X(20) VALUE "DISCARD AUTINSTMODEL".
           05 PIC X(02) VALUE X"4802".
           05 PIC X(20) VALUE "ENTER TRACENUM".
           05 PIC X(02) VALUE X"4A02".
           05 PIC X(20) VALUE "ASKTIME ABSTIME".
           05 PIC X(02) VALUE X"4A04".
           05 PIC X(20) VALUE "FORMATTIME".
           05 PIC X(02) VALUE X"4C02".
           05 PIC X(20) VALUE "INQUIRE FILE".
           05 PIC X(02) VALUE X"4C04".
           05 PIC X(20) VALUE "SET FILE".
           05 PIC X(02) VALUE X"4C10".
           05 PIC X(20) VALUE "DISCARD FILE".
           05 PIC X(02) VALUE X"4E02".
           05 PIC X(20) VALUE "INQUIRE PROGRAM".
           05 PIC X(02) VALUE X"4E04".
           05 PIC X(20) VALUE "SET PROGRAM".
           05 PIC X(02) VALUE X"4E10".
           05 PIC X(20) VALUE "DISCARD PROGRAM".
           05 PIC X(02) VALUE X"5002".
           05 PIC X(20) VALUE "INQUIRE TRANSACTION".
           05 PIC X(02) VALUE X"5004".
           05 PIC X(20) VALUE "SET TRANSACTION".
           05 PIC X(02) VALUE X"5010".
           05 PIC X(20) VALUE "DISCARD TRANSACTION".
           05 PIC X(02) VALUE X"5202".
           05 PIC X(20) VALUE "INQUIRE TERMINAL".
           05 PIC X(02) VALUE X"5204".
           05 PIC X(20) VALUE "SET TERMINAL".
           05 PIC X(02) VALUE X"5402".
           05 PIC X(20) VALUE "INQUIRE SYSTEM".
           05 PIC X(02) VALUE X"5404".
           05 PIC X(20) VALUE "SET SYSTEM".
           05 PIC X(02) VALUE X"5602".
           05 PIC X(20) VALUE "SPOOLOPEN OUTPUT".
           05 PIC X(02) VALUE X"5606".
           05 PIC X(20) VALUE "SPOOLWRITE".
           05 PIC X(02) VALUE X"5610".
           05 PIC X(20) VALUE "SPOOLCLOSE".
           05 PIC X(02) VALUE X"5802".
           05 PIC X(20) VALUE "INQUIRE CONNECTION".
           05 PIC X(02) VALUE X"5804".
           05 PIC X(20) VALUE "SET CONNECTION".
           05 PIC X(02) VALUE X"5C02".
           05 PIC X(20) VALUE "INQUIRE TDQUEUE".
           05 PIC X(02) VALUE X"5C04".
           05 PIC X(20) VALUE "SET TDQUEUE".
           05 PIC X(02) VALUE X"5E02".
           05 PIC X(20) VALUE "INQUIRE TASK".
           05 PIC X(02) VALUE X"5E04".
           05 PIC X(20) VALUE "SET TASK".
           05 PIC X(02) VALUE X"6002".
           05 PIC X(20) VALUE "INQUIRE JOURNALNUM".
           05 PIC X(02) VALUE X"6004".
           05 PIC X(20) VALUE "SET JOURNALNUM".
           05 PIC X(02) VALUE X"7602".
           05 PIC X(20) VALUE "PERFORM SHUTDOWN".
           05 PIC X(02) VALUE X"7802".
           05 PIC X(20) VALUE "INQUIRE TRACEDEST".
           05 PIC X(02) VALUE X"7804".
           05 PIC X(20) VALUE "SET TRACEDEST".
           05 PIC X(02) VALUE X"7E02".
           05 PIC X(20) VALUE "DUMP TRANSACTION".
           05 PIC X(02) VALUE X"FFFF".
           05 PIC X(20) VALUE "IVALID COMMAND".
       01  REDEFINES TABELA-FN.
           05 OCCURS 98.
              10 FN-CODE PIC X(02).
              10 FN-TEXT PIC X(20).

       01  TABELA-ABEND.
           05 PIC X(16) VALUE "AEI0PGMIDERR".
           05 PIC X(16) VALUE "AEI1TRANSIDERR".
           05 PIC X(16) VALUE "AEI2ENDDATA".
           05 PIC X(16) VALUE "AEI3INVTSREQ".
           05 PIC X(16) VALUE "AEI4EXPIRED".
           05 PIC X(16) VALUE "AEI8TSIOERR".
           05 PIC X(16) VALUE "AEI9MAPFAIL".
           05 PIC X(16) VALUE "AEIDEOF".
           05 PIC X(16) VALUE "AEIEEODS".
           05 PIC X(16) VALUE "AEIJNOSTART".
           05 PIC X(16) VALUE "AEIKTERMIDERR".
           05 PIC X(16) VALUE "AEILFILENOTFOUND".
           05 PIC X(16) VALUE "AEILDSIDERR".
           05 PIC X(16) VALUE "AEIMNOTFND".
           05 PIC X(16) VALUE "AEINDUPREC".
           05 PIC X(16) VALUE "AEIODUPKEY".
           05 PIC X(16) VALUE "AEIPINVREQ".
           05 PIC X(16) VALUE "AEIQIOERR".
           05 PIC X(16) VALUE "AEIRNOSPACE".
           05 PIC X(16) VALUE "AEISNOTOPEN".
           05 PIC X(16) VALUE "AEITENDFILE".
           05 PIC X(16) VALUE "AEIUILLOGIC".
           05 PIC X(16) VALUE "AEIVLENGERR".
           05 PIC X(16) VALUE "AEIWQZERO".
           05 PIC X(16) VALUE "AEIZITEMERR".
           05 PIC X(16) VALUE "AEX0TCIDERR".
           05 PIC X(16) VALUE "AEX1DSNNOTFOUND".
           05 PIC X(16) VALUE "AEX2LOADING".
           05 PIC X(16) VALUE "AEX3MODELIDERR".
           05 PIC X(16) VALUE "AEX4UOWNOTFOUND".
           05 PIC X(16) VALUE "AEX5PARTNERIDERR".
           05 PIC X(16) VALUE "AEX6PROFILEIDERR".
           05 PIC X(16) VALUE "AEX7NETNAMEIDERR".
           05 PIC X(16) VALUE "AEX8LOCKED".
           05 PIC X(16) VALUE "AEX9RECORDBUSY".
           05 PIC X(16) VALUE "AEXCRESIDERR".
           05 PIC X(16) VALUE "AEXFESCERROR".
           05 PIC X(16) VALUE "AEXGUOWLNOTFOUND".
           05 PIC X(16) VALUE "AEXITERMERR".
           05 PIC X(16) VALUE "AEXJROLLEDBACK".
           05 PIC X(16) VALUE "AEXKEND".
           05 PIC X(16) VALUE "AEXLDISABLED".
           05 PIC X(16) VALUE "AEXVVOLIDERR".
           05 PIC X(16) VALUE "AEXWSUPPRESSED".
           05 PIC X(16) VALUE "AEXXTASKIDERR".
           05 PIC X(16) VALUE "AEY0INVEXITREQ".
           05 PIC X(16) VALUE "AEY1INVPARTNSET".
           05 PIC X(16) VALUE "AEY2INVPARTN".
           05 PIC X(16) VALUE "AEY3PARTNFAIL".
           05 PIC X(16) VALUE "AEY7NOTAUTH".
           05 PIC X(16) VALUE "AEYAINVERRTERM".
           05 PIC X(16) VALUE "AEYBINVMPSZ".
           05 PIC X(16) VALUE "AEYCIGREQID".
           05 PIC X(16) VALUE "AEYEINVLDC".
           05 PIC X(16) VALUE "AEYGJIDERR".
           05 PIC X(16) VALUE "AEYHQIDERR".
           05 PIC X(16) VALUE "AEYJDSSTAT".
           05 PIC X(16) VALUE "AEYKSELNERR".
           05 PIC X(16) VALUE "AEYLFUNCERR".
           05 PIC X(16) VALUE "AEYMUNEXPIN".
           05 PIC X(16) VALUE "AEYNNOPASSBKRD".
           05 PIC X(16) VALUE "AEYONOPASSBKWR".
           05 PIC X(16) VALUE "AEYPSEGIDERR".
           05 PIC X(16) VALUE "AEYQSYSIDERR".
           05 PIC X(16) VALUE "AEYRISCINVREQ".
           05 PIC X(16) VALUE "AEYTENVDEFERR".
           05 PIC X(16) VALUE "AEYUIGREQCD".
           05 PIC X(16) VALUE "AEYVSESSIONERR".
           05 PIC X(16) VALUE "AEYXUSERIDERR".
           05 PIC X(16) VALUE "AEYYNOTALLOC".
           05 PIC X(16) VALUE "AEYZCBIDERR".
           05 PIC X(16) VALUE "AEZECHANGED".
           05 PIC X(16) VALUE "AEZFPROCESSBUSY".
           05 PIC X(16) VALUE "AEZGACTIVITYBUSY".
           05 PIC X(16) VALUE "AEZHPROCESSERR".
           05 PIC X(16) VALUE "AEZIACTIVITYERR".
           05 PIC X(16) VALUE "AEZJCONTAINERERR".
           05 PIC X(16) VALUE "AEZKEVENTERR".
           05 PIC X(16) VALUE "AEZLTOKENERR".
           05 PIC X(16) VALUE "AEZMNOTFINISHED".
           05 PIC X(16) VALUE "AEZNPOOLERR".
           05 PIC X(16) VALUE "AEZOTIMERERR".
           05 PIC X(16) VALUE "AEZPSYMBOLERR".
           05 PIC X(16) VALUE "AEZQTEMPLATERR".
           05 PIC X(16) VALUE "ASCPNOSTG".
           05 PIC X(16) VALUE "----ERROR".
           05 PIC X(16) VALUE "A17GNOJBUFSP".
           05 PIC X(16) VALUE "B000QBUSY".
       01  REDEFINES TABELA-ABEND.
           05 OCCURS 88.
              10 ABCODET PIC X(4).
              10 ABERROR PIC X(12).

       01  CWALEN     PIC 9(008) COMP-X VALUE 0.
       01  COMMAREA.
           04 PIC X OCCURS 0 TO 32767 DEPENDING ON CWALEN.

       LINKAGE SECTION.

       COPY CWCICS.
       01  DFHEIB-LK   PIC X.
       01  CWA         PIC X.
       01  ALOCADA     PIC X.
       01  BUFFER      PIC X.

       PROCEDURE DIVISION USING PARAMETROS-CWCICS DFHEIB-LK.

       000-INICIO.

           IF VEZ = 1
              MOVE 2 TO VEZ
              PERFORM 001-INITIATE-CICS THRU 001-99-FIM.

           MOVE CWCICS-FUNCTION TO FN
           MOVE 0               TO cics-resp2

           IF cwcics-addtask
              set NO-close-all to true
              MOVE 0 TO TASKN
              PERFORM 002-CREATE-TASK THRU 002-99-FIM
              EXIT PROGRAM
           END-IF

           IF  (NOT cwcics-dfheib)
           AND (NOT CWCICS-START)
           AND TASKN = 0
               EXEC COBOLware Send
                    Message
             'Programa COBOL/CICS deve ser executado via transa‡Æo CICS'
               END-EXEC
               SET CICS-purge TO TRUE
               set close-all to true
               EXIT PROGRAM
           END-IF

           EVALUATE TRUE
               WHEN cwcics-LENGTH > 32767
                    SET CWCICS-LENGERR TO TRUE
               WHEN cwcics-dfheib
                    PERFORM 010-DFHEIB THRU 010-99-FIM
               WHEN CWCICS-ABEND
                    PERFORM VARYING I FROM 1 BY 1 UNTIL I > 88
                            OR CWCICS-ABCODE = ABCODET(I)
                    END-PERFORM
                    MOVE SPACES TO MSG
                    IF I > 88
                       STRING "CICS abend: " DELIMITED BY SIZE
                              CWCICS-ABCODE DELIMITED BY SIZE
                         INTO MSG
                    ELSE
                       MOVE CWCICS-ABCODE TO MSG
                       STRING "CICS abend: " DELIMITED BY SIZE
                              CWCICS-ABCODE DELIMITED BY SIZE
                              ',"' DELIMITED BY SIZE
                              ABERROR(I) DELIMITED BY SPACE
                              '"' DELIMITED BY SIZE
                         INTO MSG
                    END-IF
                    EXEC COBOLware Send
                         Message MSG
                    END-EXEC
                    IF cvda-CANCEL
                       SET CICS-PURGE TO TRUE
                       set close-all to true
                    END-IF
                    IF cvda-RESET
                       SET CICS-PURGE TO TRUE
                    END-IF
               WHEN CWCICS-ADDRESS
                    CONTINUE
      *        WHEN CWCICS-ALLOCATE
               WHEN CWCICS-ASKTIME
                    IF   CWCICS-ABSTIME NOT = 0
                         SET cwcics-asktime-abstime TO TRUE
                    END-IF
                    PERFORM 090-ABSTIME THRU 090-99-FIM
               WHEN CWCICS-ASSIGN
                    MOVE        ABCODE            TO CWCICS-ABCODE
                    MOVE        APPLID            TO CWCICS-APPLID
                    MOVE        X"00"             TO CWCICS-BTRANS
                    MOVE        X"FF"             TO CWCICS-COLOR
                    MOVE        CWALEN           TO CWCICS-CWALENG
                    IF          CWCICS-EXTDS     NOT = low-value
                       MOVE     EXTDS             TO CWCICS-EXTDS
                       IF       EXTDS              = SPACE
                           SET  CWCICS-INVREQ     TO TRUE
                       END-IF
                    END-IF
                    IF          CWCICS-FACILITY NOT = LOW-VALUES
                           SET  CWCICS-INVREQ     TO TRUE
                    END-IF
                    MOVE        1                 TO FCI
                    IF          CWA       NOT = LOW-VALUES
                       ADD      16                TO FCI
                    END-IF
                    MOVE        FCI(1:1)          TO CWCICS-FCI
                    MOVE        ZERO              TO CWCICS-GCHARS
                    MOVE        ZERO              TO CWCICS-GCODES
                    MOVE        X"FF"             TO CWCICS-HILIGHT
                    MOVE        X"00"             TO CWCICS-KATAKANA
                    IF          CWCICS-MAPCOLUMN NOT = low-values
                       MOVE     MAPCOLUMN         TO CWCICS-MAPCOLUMN
                       IF       MAPCOLUMN          = ZERO
                           SET  CWCICS-INVREQ     TO TRUE
                       END-IF
                    END-IF
                    IF          CWCICS-MAPHEIGHT NOT = low-values
                       MOVE     MAPHEIGHT         TO CWCICS-MAPHEIGHT
                       IF       MAPHEIGHT          = ZERO
                           SET  CWCICS-INVREQ     TO TRUE
                       END-IF
                    END-IF
                    IF         CWCICS-MAPLINE    NOT = low-values
                       MOVE    MAPLINE            TO CWCICS-MAPLINE
                       IF      MAPLINE             = ZERO
                          SET  CWCICS-INVREQ      TO TRUE
                       END-IF
                    END-IF
                    IF         CWCICS-MAPWIDTH   NOT = low-values
                       MOVE    MAPWIDTH           TO CWCICS-MAPWIDTH
                       IF      MAPWIDTH            = ZERO
                          SET  CWCICS-INVREQ     TO TRUE
                       END-IF
                    END-IF
                    MOVE       X"00"              TO CWCICS-MSRCONTROL
                    MOVE       X"00"              TO CWCICS-MSRCONTROL
                    MOVE       NETNAME            TO CWCICS-NETNAME
                    MOVE       NEXTTRANSID        TO CWCICS-NEXTTRANSID
                    MOVE       LOW-VALUES         TO CWCICS-ODBCHNDLLIST
                    MOVE       LOW-VALUES         TO CWCICS-OPCLASS
                    IF         CWCICS-OPERKEYS   NOT = low-values
                       MOVE    OPERKEYS           TO CWCICS-OPERKEYS
                       IF      OPERKEYS           = SPACES
                           SET CWCICS-INVREQ      TO TRUE
                       END-IF
                    END-IF
                    IF         CWCICS-OPID       NOT = low-values
                       MOVE    OPID               TO CWCICS-OPID
                       IF      OPID               = SPACES
                           SET CWCICS-INVREQ      TO TRUE
                       END-IF
                    END-IF
                    IF         CWCICS-OPSECURITY NOT = low-values
                       MOVE    OPSECURITY         TO CWCICS-OPSECURITY
                       IF      OPSECURITY          = SPACES
                           SET CWCICS-INVREQ      TO TRUE
                       END-IF
                    END-IF
                    MOVE        X"00"             TO CWCICS-OUTLINE
                    IF         CWCICS-PRINSYSID  NOT = low-values
                       MOVE    PRINSYSID          TO CWCICS-PRINSYSID
                       IF      PRINSYSID           = SPACES
                           SET CWCICS-INVREQ      TO TRUE
                       END-IF
                    END-IF
                    MOVE        LAST-PROGRAM      TO CWCICS-PROGRAM
                    MOVE        X"00"             TO CWCICS-PS
                    IF         CWCICS-QNAME      NOT = low-values
                       MOVE    QNAME              TO CWCICS-QNAME
                       IF      QNAME               = SPACES
                           SET CWCICS-INVREQ      TO TRUE
                       END-IF
                    END-IF
                    MOVE        X"00"             TO CWCICS-RESTART
                    MOVE        24                TO CWCICS-SCRNHT
                    MOVE        80                TO CWCICS-SCRNWD
                    IF         CWCICS-SIGDATA    NOT = low-values
                       MOVE    SIGDATA            TO CWCICS-SIGDATA
                       IF      SIGDATA             = SPACES
                           SET CWCICS-INVREQ      TO TRUE
                       END-IF
                    END-IF
                    MOVE        X"00"             TO CWCICS-SOSI
                    MOVE        "TD"              TO CWCICS-STARTCODE
                    MOVE        SYSID             TO CWCICS-SYSID
                    IF         CWCICS-TCTUALENG  NOT = low-values
                       MOVE    TCTUALENG          TO CWCICS-TCTUALENG
                       IF      TCTUALENG           = SPACES
                           SET CWCICS-INVREQ      TO TRUE
                       END-IF
                    END-IF
                    MOVE       X"91"              TO CWCICS-TERMCODE
                    MOVE       ZERO               TO CWCICS-TWALENG
                    MOVE       X"00"              TO CWCICS-UNATTEND
                    EXEC COBOLware GetSystem
                         USER (CWCICS-USERID)
                    END-EXEC
                    DISPLAY    "USERID"         UPON ENVIRONMENT-NAME
                    ACCEPT     CWCICS-USERID    FROM ENVIRONMENT-VALUE
                    MOVE       X"00"              TO CWCICS-VALIDATION
               WHEN CWCICS-BIF
                    IF  CWCICS-LENGTH < 1
                    OR  CWCICS-LENGTH > 18
                        SET CWCICS-LENGERR TO TRUE
                    ELSE
                        MOVE LENGTH OF VINTE TO Y
                        MOVE 0      TO NTEST
                        MOVE SPACES TO VINTE(1:)
                        PERFORM VARYING I FROM LENGTH OF VINTE BY -1
                                          UNTIL I = 0
                             IF CWCICS-FIELD (I: 2) = 'DB' OR '- '
                             AND VINTE = SPACES
                                 MOVE 1 TO NEGAT
                             END-IF
                             MOVE CWCICS-FIELD (I: 1)
                               TO NTEST (1:1)
                             IF NTEST NUMERIC
                                MOVE NTEST TO DVINTE (Y)
                                SUBTRACT 1 FROM Y
                                IF NTEST NEGATIVE
                                   MOVE 1 TO NEGAT
                                END-IF
                             END-IF
                        END-PERFORM
                        INSPECT VINTE(1:18) CONVERTING SPACES TO ZERO
                        IF NEGAT = 1
                           COMPUTE VINTE = VINTE - 1
                        END-IF
                        COMPUTE I = 18 - CWCICS-LENGTH + 1
                        MOVE VINTE (I: ) TO CWCICS-FIELD
                    END-IF
               WHEN CWCICS-CANCEL
                    IF  CWL NOT = 0
                        SET CWCICS-SET TO ADDRESS OF CWA
                        CALL "CBL_FREE_MEM" USING BY VALUE CWCICS-SET
                                                 RETURNING STATUS-CODE
                        MOVE 0 TO CWL
                    END-IF
      *        WHEN CWCICS-CONNECT PROCESS
      *        WHEN CWCICS-CONVERSE
               WHEN CWCICS-DELAY
                    CONTINUE
               WHEN CWCICS-DELETE
                    PERFORM 020-FCT THRU 020-99-FIM
               WHEN CWCICS-DELETEQ
                    PERFORM 030-SELECT-QUEUE THRU 030-99-FIM
                    IF cvda-TD
                       SET cwcics-deleteq-td TO TRUE
                    END-IF
                    IF cvda-TS
                       SET cwcics-deleteq-ts TO TRUE
                    END-IF
                    MOVE 0 TO TENTOU
                    PERFORM TEST AFTER UNTIL FS-TDTS < "10"
                         DELETE FILE TDTS
                         IF  FS-TDTS = '30' OR '35'
                             MOVE "00" TO FS-TDTS
                         END-IF
                         IF FS-TDTS > '09'
                            IF FS-TDTS = '9A'
                               ADD 1 TO TENTOU
                               IF  TENTOU > 9999
                                   EXEC COBOLware ISAMerr
                                        STATUS FS-TDTS
                                        LABEL  LB-TDTS
                                   END-EXEC
                                   MOVE 0 TO TENTOU
                               END-IF
                            ELSE
                               SET CWCICS-IOERR TO TRUE
                               EXIT PERFORM
                            END-IF
                         END-IF
                    END-PERFORM
      *        WHEN CWCICS-DEQ
      *        WHEN CWCICS-DISCARD AUTINSTMODEL
      *        WHEN CWCICS-DISCARD FILE
      *        WHEN CWCICS-DISCARD PROGRAM
      *        WHEN CWCICS-DISCARD TRANSACTION
      *        WHEN CWCICS-DUMP TRANSACTION
               WHEN CWCICS-ENDBR
                    IF   CWCICS-SYSID       = LOW-VALUES
                         MOVE CWCICS-SYSID TO BR-SYSID
                    ELSE
                         MOVE SYSID        TO BR-SYSID
                    END-IF
                    MOVE CWCICS-DATASET    TO BR-DATASET
                    MOVE CWCICS-REQID      TO BR-REQID
                    READ BR
                         INVALID KEY
                           WRITE BR-REG
                         NOT INVALID KEY
                             DELETE BR RECORD
                    END-READ
      *        WHEN CWCICS-ENQ
      *        WHEN CWCICS-ENTER TRACENUM
      *        WHEN CWCICS-EXTRACT ATTRIBUTES
      *        WHEN CWCICS-EXTRACT PROCESS
               WHEN CWCICS-FORMATTIME
                    IF CWCICS-ABSTIME (1:) = low-values
                       INITIALIZE CICS-FORMATTIME
                       SET CWCICS-ERROR TO TRUE
                    ELSE
                       MOVE CWCICS-ABSTIME TO ABSTIME
                       IF  CENTESIMOS-ABS > 49
                           ADD 1 TO SEGUNDOS
                       END-IF
                       IF   CWCICS-DATESEP = LOW-VALUES
                            MOVE '/'            TO DATESEP
                       ELSE
                            MOVE CWCICS-DATESEP TO DATESEP
                       END-IF
                       IF   CWCICS-TIMESEP = LOW-VALUES
                            MOVE ':'            TO TIMESEP
                       ELSE
                            MOVE CWCICS-TIMESEP TO TIMESEP
                       END-IF
                       MOVE 0 TO DAYCOUNT
                       PERFORM VARYING YYYY FROM 1900 BY 1
                                            UNTIL YYYY = 0
                               DIVIDE YYYY
                                   BY 4 GIVING W-NADA REMAINDER W-TP-ANO
                               IF   ANO-BISSEXTO
                                    MOVE 366 TO DDD
                               ELSE
                                    MOVE 365 TO DDD
                               END-IF
                               IF SEGUNDOS < (DDD * 86400)
                                  EXIT PERFORM
                               END-IF
                               COMPUTE SEGUNDOS = SEGUNDOS
                                                - (DDD * 86400)
                               ADD      DDD   TO DAYCOUNT
                       END-PERFORM
                       MOVE 0 TO DDD
                       DIVIDE YYYY
                           BY 4 GIVING W-NADA REMAINDER W-TP-ANO
                       IF   ANO-BISSEXTO
                            MOVE 29 TO DIA-MAX (2)
                       ELSE
                            MOVE 28 TO DIA-MAX (2)
                       END-IF
                       PERFORM VARYING MM FROM 1 BY 1
                                         UNTIL MM > 12
                               IF SEGUNDOS < DIA-MAX (MM) * 86400
                                  EXIT PERFORM
                               END-IF
                               ADD      DIA-MAX (MM) TO DDD
                                                        DAYCOUNT
                               COMPUTE SEGUNDOS = SEGUNDOS -
                                                 (DIA-MAX (MM) * 86400)
                       END-PERFORM
                       PERFORM VARYING DD FROM 1 BY 1 UNTIL DD > 31
                               IF SEGUNDOS < 86400
                                  EXIT PERFORM
                               END-IF
                               ADD 1 TO DDD
                                        DAYCOUNT
                               SUBTRACT 86400 FROM SEGUNDOS
                       END-PERFORM
                       COMPUTE HH = SEGUNDOS / 3600
                       COMPUTE SEGUNDOS = SEGUNDOS - (HH * 3600)
                       COMPUTE MIN = SEGUNDOS / 60
                       COMPUTE SS = SEGUNDOS - (MIN * 60)
                       INITIALIZE CICS-FORMATTIME
                       MOVE DATEMASK     TO CWCICS-DATE
                       MOVE FULLMASK     TO CWCICS-FULLDATE
                       MOVE "ab/cd/gh"   TO CWCICS-DDMMYY
                       MOVE "ab/cd/efgh" TO CWCICS-DDMMYYYY
                       MOVE "cd/ab/gh"   TO CWCICS-MMDDYY
                       MOVE "cd/ab/efgh" TO CWCICS-MMDDYYYY
                       MOVE "gh/ijk"     TO CWCICS-YYDDD
                       MOVE "gh/ab/cd"   TO CWCICS-YYDDMM
                       MOVE "gh/cd/ab"   TO CWCICS-YYMMDD
                       MOVE "efgh/ijk"   TO CWCICS-YYYYDDD
                       MOVE "efgh/ab/cd" TO CWCICS-YYYYDDMM
                       MOVE "efgh/cd/ab" TO CWCICS-YYYYMMDD
                       INSPECT CICS-FORMATTIME
                       CONVERTING LETRAS TO DATESTRING
                       MOVE DAYCOUNT     TO CWCICS-DAYCOUNT
                       MOVE DD           TO CWCICS-DAYOFMONTH
                       MOVE MM           TO CWCICS-MONTHOFYEAR
                       MOVE DATESEP      TO CWCICS-DATESEP
                       MOVE TIMESEP      TO CWCICS-TIMESEP
                       STRING HH TIMESEP MIN TIMESEP SS
                              DELIMITED BY SIZE
                                       INTO CWCICS-TIME
                       EXEC COBOLware Time Week
                            DATE YYYYMMDD
                            WEEK-NUM (CWCICS-DAYOFWEEK)
                       END-EXEC
                       IF   CWCICS-DAYOFWEEK = 0
                            MOVE 6       TO CWCICS-DAYOFWEEK
                       ELSE
                            SUBTRACT 1 FROM CWCICS-DAYOFWEEK
                       END-IF
                    END-IF
      *        WHEN CWCICS-FREE
               WHEN CWCICS-FREEMAIN
                    CALL "CBL_FREE_MEM" USING BY VALUE CWCICS-SET
                                        RETURNING STATUS-CODE
                    IF STATUS-CODE NOT = 0
                       MOVE 1 TO cics-resp2
                    END-IF
               WHEN CWCICS-GETMAIN
                    IF   CWCICS-FLENGTH =  0
                         SET CWCICS-LENGERR TO TRUE
                    ELSE
                         CALL "CBL_ALLOC_MEM" USING    CWCICS-SET
                                              BY VALUE CWCICS-FLENGTH
                                                       flag
                                             RETURNING STATUS-CODE
                          IF STATUS-CODE NOT = 0
                             SET CWCICS-NOSTG TO TRUE
                          ELSE
                             IF CWCICS-INITIMG NOT = X"01"
                                SET ADDRESS OF ALOCADA
                                 TO CWCICS-SET
                               PERFORM VARYING I FROM 1 BY 1
                                         UNTIL I > CWCICS-FLENGTH
                                   MOVE CWCICS-INITIMG
                                     TO ALOCADA(I:1)
                               END-PERFORM
                             END-IF
                          END-IF
                    END-IF
               WHEN CWCICS-HANDLE
                    IF  cvda-ABEND
                        SET cwcics-handle-abend   TO TRUE
                    END-IF
                    IF  cvda-AID
                        SET cwcics-handle-aid TO TRUE
                        MOVE CICS-AidKeys     TO flag-AidKeys
                    END-IF
                    IF  cvda-CONDITION
                        SET cwcics-handle-condition TO TRUE
                    END-IF
                    move cics-flags to flag-data
                    rewrite flags-reg
               WHEN CWCICS-IGNORE
                    move cics-flags to flag-data
                    rewrite flags-reg
      *        WHEN CWCICS-INQUIRE AUTINSTMODEL
      *        WHEN CWCICS-INQUIRE CONNECTION
               WHEN CWCICS-INQUIRE
                AND CWCICS-FILE not = LOW-VALUES
                    SET  cwcics-inquire-file TO TRUE
                    MOVE CWCICS-FILE         TO CWCICS-DATASET
                    PERFORM 020-FCT        THRU 020-99-FIM
      *        WHEN CWCICS-INQUIRE JOURNALNUM
      *        WHEN CWCICS-INQUIRE PROGRAM
      *        WHEN CWCICS-INQUIRE SYSTEM
               WHEN CWCICS-INQUIRE
                AND CWCICS-TASK NOT = 0
                    SET cwcics-inquire-task TO TRUE
                    PERFORM 100-INQUIRE-TASK THRU 100-99-FIM
      *        WHEN CWCICS-INQUIRE TDQUEUE
      *        WHEN CWCICS-INQUIRE TERMINAL
      *        WHEN CWCICS-INQUIRE TRACEDEST
      *        WHEN CWCICS-INQUIRE TRANSACTION
      *        WHEN CWCICS-ISSUE ABEND
      *        WHEN CWCICS-ISSUE CONFIRMATION
      *        WHEN CWCICS-ISSUE ERASEAUP
      *        WHEN CWCICS-ISSUE ERROR
      *        WHEN CWCICS-ISSUE PREPARE
      *        WHEN CWCICS-ISSUE SIGNAL
               WHEN CWCICS-PUSH
                 OR CWCICS-LINK
                    IF CWCICS-LINK
                       PERFORM 070-SET-CWA THRU 070-99-FIM
                    END-IF
                    IF   flags-reg (3:) = LOW-VALUES
                         SET CWCICS-INVREQ TO TRUE
                    ELSE
                         add 1 to flag-id
                         write flags-reg
                    END-IF
               WHEN CWCICS-LOAD
                    SET CWCICS-PGMIDERR TO TRUE
                    PERFORM 080-SEARCH-PATH THRU 080-99-FIM
      *        WHEN CWCICS-PERFORM SHUTDOWN
               WHEN CWCICS-POP
                 OR CWCICS-RETURN
                    IF   flag-ID < 2
                         SET CWCICS-INVREQ TO TRUE
                    ELSE
                         subtract 1 from flag-id
                         delete flags record
                         read flags
                    END-IF
      *             IF CWCICS-RETURN
      *                IF START-CHARACTERS NOT = SPACES
      *                   CALL "CBL_WRITE_SCR_CHATTRS" USING X"0000"
      *                                               START-CHARACTERS
      *                                               START-ATTRIBUTES
      *                                               X"07D0"
      *                   MOVE SPACES TO START-CHARACTERS
      *                END-IF
      *             END-IF
               WHEN CWCICS-POST
                    CONTINUE
               WHEN CWCICS-READ
                    IF CWCICS-FILE NOT = low-values
                       MOVE CWCICS-FILE TO CWCICS-DATASET
                    END-IF
                    IF CWCICS-READNEXT
                    OR CWCICS-READPREV
                       IF   CWCICS-SYSID       = LOW-VALUES
                            MOVE CWCICS-SYSID TO BR-SYSID
                       ELSE
                            MOVE SYSID        TO BR-SYSID
                       END-IF
                       MOVE CWCICS-DATASET TO BR-DATASET
                       MOVE CWCICS-REQID   TO BR-REQID
                       READ BR
                            INVALID KEY
                              SET CWCICS-INVREQ TO TRUE
                              MOVE 1 TO cics-resp2
                            NOT INVALID KEY
                                MOVE BR-RIDFLD TO STARTBR-RIDFLD
                                MOVE BR-KEY    TO STARTBR-KEY
                                PERFORM 020-FCT THRU 020-99-FIM
                       END-READ
                    ELSE
                       IF cvda-NEXT OR cvda-PREV
                          SET CWCICS-INVREQ TO TRUE
                       ELSE
                          PERFORM 020-FCT THRU 020-99-FIM
                       END-IF
                    END-IF
               WHEN CWCICS-READQ
                    PERFORM 030-SELECT-QUEUE THRU 030-99-FIM
                    IF CWCICS-ABCODE = low-values
                       PERFORM 034-GET-QUEUE THRU 034-99-FIM
                    END-IF
               WHEN CWCICS-RECEIVE
                AND (CWCICS-MAPSET NOT = SPACES)
                AND (CWCICS-MAPSET NOT = LOW-VALUES)
                AND (CWCICS-MAP    NOT = SPACES)
                AND (CWCICS-MAP    NOT = LOW-VALUES)
                    set cwcics-receive-map to true
                    set cvda-TERMINAL      to true
                    set cvda-RECEIVE       to true
                    PERFORM 040-BMS THRU 040-99-FIM
                    IF NOT cvda-ASIS
                       INSPECT CWCICS-FROM
                               CONVERTING MINUSCULAS TO MAIUSCULAS
                    END-IF
               WHEN CWCICS-RECEIVE
                    MOVE LOW-VALUES TO CWCICS-FROM
                    IF  CWALEN NOT = 0
                        MOVE CWALEN TO CWCICS-LENGTH
                        MOVE COMMAREA   (1:CWALEN)
                          TO CWCICS-FROM(1:CWALEN)
                        MOVE LOW-VALUES TO COMMAREA (1:CWALEN)
                        MOVE 0          TO CWALEN
                    ELSE
                        MOVE LOW-VALUES TO CWCICS-FROM
                        CALL "CW3278" USING "G" START-EIBAID
                                                CWCICS-FROM
                                                "XXXX"
                                                START-TECLA
                        MOVE START-TECLA TO CWCICS-AID
                        IF NOT cvda-ASIS
                           INSPECT CWCICS-FROM
                                   CONVERTING MINUSCULAS TO MAIUSCULAS
                        END-IF
                    END-IF
               WHEN CWCICS-RELEASE
                    CANCEL CWCICS-PROGRAM
               WHEN CWCICS-RESETBR
                    IF   CWCICS-SYSID       = LOW-VALUES
                         MOVE CWCICS-SYSID TO BR-SYSID
                    ELSE
                         MOVE SYSID        TO BR-SYSID
                    END-IF
                    MOVE CWCICS-DATASET TO BR-DATASET
                    MOVE CWCICS-REQID   TO BR-REQID
                    READ BR
                         INVALID KEY
                           WRITE BR-REG
                    END-READ
                    MOVE BR-RIDFLD-RESET TO BR-RIDFLD
                    MOVE BR-KEY-RESET    TO BR-KEY
                    REWRITE BR-REG
               WHEN CWCICS-RETRIEVE
                    MOVE SPACES TO CWCICS-FROM
                    IF CWCICS-QUEUE = LOW-VALUES
                       IF CWALEN NOT = 0
                          IF CWCICS-LENGTH > CWALEN
                             SET CWCICS-LENGERR TO TRUE
                          ELSE
                             IF  CWCICS-SET NOT = NULL
                                 SET ADDRESS OF BUFFER TO CWCICS-SET
                                 MOVE COMMAREA(1:CWALEN)
                                   TO BUFFER  (1:CWCICS-LENGTH)
                             ELSE
                                 MOVE COMMAREA   (1:CWALEN)
                                   TO CWCICS-FROM(1:CWCICS-LENGTH)
                             END-IF
                             MOVE 0 TO CWALEN
                          END-IF
                       ELSE
                          SET CWCICS-ENVDEFERR TO TRUE
                       END-IF
                    ELSE
                       SET cvda-TS TO TRUE
                       PERFORM 030-SELECT-QUEUE THRU 030-99-FIM
                       IF CWCICS-ABCODE = low-values
                          PERFORM 034-GET-QUEUE THRU 034-99-FIM
                          IF  CWCICS-SET NOT = NULL
                              SET ADDRESS OF BUFFER TO CWCICS-SET
                              MOVE CWCICS-INTO(1:CWCICS-LENGTH)
                                TO BUFFER     (1:CWCICS-LENGTH)
                          END-IF
                       END-IF
                    END-IF
               WHEN CWCICS-REWRITE
                    PERFORM 020-FCT THRU 020-99-FIM
               WHEN CWCICS-SEND
                    evaluate true
                        when cvda-PAGE
                             PERFORM 060-PAGE THRU 060-99-FIM
                        when (CWCICS-MAPSET NOT = SPACES)
                         AND (CWCICS-MAPSET NOT = LOW-VALUES)
                         AND (CWCICS-MAP    NOT = SPACES)
                         AND (CWCICS-MAP    NOT = LOW-VALUES)
                         AND NOT cvda-CONNECTION
                             SET cwcics-send-map TO TRUE
                             IF  cvda-ALARM
                                 CALL X"E5"
                             END-IF
                             set cvda-send to true
                             PERFORM 040-BMS THRU 040-99-FIM
                             IF   cvda-ACCUM
                                  PERFORM 050-ACCUM THRU 050-99-FIM
                             END-IF
                    end-evaluate
      *        WHEN CWCICS-SEND
      *         AND cvda-CONNECTION
               WHEN CWCICS-SEND
                AND NOT cvda-CONNECTION
                    SET cwcics-send-text TO TRUE
                    IF   CWCICS-LENGTH(1:) = low-values
                    OR   CWCICS-LENGTH < 75
                         EXEC COBOLware Send
                              Message CWCICS-FROM
                         END-EXEC
                         MOVE 1 TO EIBCPOSN
                                   CWCICS-CURSOR
                    ELSE
                         IF   cvda-ERASE
                              DISPLAY (1, 1) ERASE
                         END-IF
                         call "CBL_SET_CSR_POS" USING X"0000"
                         IF   CWCICS-CURSOR NOT = 0
                              CALL "CBL_WRITE_SCR_TTY"
                                             USING CWCICS-FROM
                                                   CWCICS-LENGTH
                                         RETURNING STATUS-CODE
                              MOVE CWCICS-LENGTH TO EIBCPOSN
                                                    CWCICS-CURSOR
                         END-IF
                         STOP ' '
                    END-IF
                    IF  cvda-ALARM
                        CALL X"E5"
                    END-IF
                    MOVE LOW-VALUES TO CWCICS-ABCODE
      *        WHEN CWCICS-SET CONNECTION
               WHEN( CICS-SetFile
                 OR (CICS-Set AND cvda-FILE)
                 AND (CWCICS-FILE NOT = LOW-VALUES))
                    SET     CWCICS-SetFile TO TRUE
                    MOVE    CWCICS-DATASET TO in-DATASET
                    MOVE    CWCICS-FILE    TO in-FILE
                    PERFORM 020-FCT      THRU 020-99-FIM
                    MOVE    in-DATASET     TO CWCICS-DATASET
                    MOVE    in-FILE        TO CWCICS-FILE
                    IF CWCICS-FILENOTFOUND
                       MOVE 18 TO CICS-RESP2
                    END-IF
      *        WHEN CWCICS-SET JOURNALNUM
      *        WHEN CWCICS-SET PROGRAM
      *        WHEN CWCICS-SET SYSTEM
TTT            WHEN   CICS-SET
                AND (CWCICS-TASK NOT = 0)
                    MOVE CWCICS-TASK TO TASKCH-TASK TASKS-TASK
                    READ TASKS  IGNORE LOCK
                    READ TASKCH IGNORE LOCK
                    EVALUATE TRUE
                        WHEN FS-TASKS  > '09'
                          OR FS-TASKCH > '09'
                             SET CWCICS-TASKIDERR TO TRUE
                             MOVE 1 TO CICS-RESP2
                        WHEN set-PURGETYPE not = 0
                        AND (NOT cvda-PURGE)
                        AND (NOT cvda-FORCEPURGE)
                            SET CWCICS-INVREQ TO TRUE
                            MOVE 3 TO cics-resp2
                        WHEN CWCICS-PRIORITY >  255
                            SET CWCICS-INVREQ TO TRUE
                            MOVE 4 TO cics-resp2
                        WHEN cvda-PURGE
                         AND tasks-cvda-BACKOUT
                             SET CWCICS-INVREQ TO TRUE
                             MOVE 5 TO cics-resp2
                        WHEN OTHER
                           IF CWCICS-PRIORITY NOT = 0
                              MOVE CWCICS-PRIORITY TO TASKCH-PRIORITY
                           END-IF
                           IF  cvda-FORCEPURGE
                           OR  cvda-PURGE
                               MOVE 1 TO TASKCH-PURGED
                           END-IF
                           REWRITE TASKCH-RECORD
                    END-EVALUATE
                    MOVE TASKN TO TASKS-TASK
                    READ TASKS WITH LOCK
      *        WHEN CWCICS-SET TDQUEUE
      *        WHEN CWCICS-SET TERMINAL
      *        WHEN CWCICS-SET TRACEDEST
      *        WHEN CWCICS-SET TRANSACTION
               WHEN CWCICS-START
                    INITIALIZE STARTS-RECORD
                    IF CWCICS-TERMID = LOW-VALUES
                    OR CWCICS-TERMID = TRMID
                    OR CWCICS-TERMID = SPACES
                       MOVE TRMID TO TCT-TRMID
                    ELSE
                       MOVE CWCICS-TERMID TO TCT-TRMID
                       READ TCT IGNORE LOCK
                       IF FS-TCT > '09'
                          SET CWCICS-TERMIDERR TO TRUE
                       END-IF
                    END-IF
                    IF NOT CWCICS-TERMIDERR
                       OPEN INPUT PCT
                       MOVE CWCICS-TRANSID TO PCT-TRANSACTION
                       MOVE 0              TO PCT-STEP
                       READ PCT
                       IF FS-PCT > '09'
                          SET CWCICS-TRANSIDERR TO TRUE
                          CLOSE PCT
                       ELSE
                          CLOSE PCT
                          IF CWCICS-SYSID = LOW-VALUES
                             MOVE SYSID TO CWCICS-SYSID
                          END-IF
                          MOVE SPACES TO LB-STARTS
                          STRING CWCICS-SYSID    DELIMITED BY SPACE
                                     DIRSEP      DELIMITED BY SIZE
                                     'cicsSTART(' DELIMITED BY SIZE
                                     TCT-TRMID   DELIMITED BY SPACE
                                       ')'      DELIMITED BY SIZE
                                INTO LB-STARTS
                          OPEN I-O STARTS
                          MOVE HIGH-VALUES      TO STARTS-ENTRY(1:)
                          START STARTS KEY NOT GREATER STARTS-KEY
                          READ STARTS PREVIOUS RECORD IGNORE LOCK
                          IF FS-STARTS < '10'
                              ADD 1 TO STARTS-ENTRY
                          ELSE
                              MOVE 1         TO STARTS-ENTRY
                          END-IF
                          IF cvda-AFTER
                             set starts-AFTER   to true
                          end-if
                          IF cvda-AT
                             set starts-AT      to true
                          end-if
                          IF cvda-NOCHECK
                             set starts-NOCHECK to true
                          end-if
                          IF cvda-PROTECT
                             set starts-PROTECT to true
                          end-if
                          MOVE CWCICS-HOURS    TO STARTS-HOURS
                          MOVE CWCICS-INTERVAL TO STARTS-INTERVAL
                          MOVE CWCICS-MINUTES  TO STARTS-MINUTES
                          MOVE CWCICS-REQID    TO STARTS-REQID
                          MOVE CWCICS-RTERMID  TO STARTS-RTERMID
                          MOVE CWCICS-RTRANSID TO STARTS-RTRANSID
                          MOVE CWCICS-SECONDS  TO STARTS-SECONDS
                          MOVE CWCICS-TRANSID  TO STARTS-TRANSID
                          MOVE CWCICS-LENGTH   TO STARTS-LENGTH
                          MOVE   CICS-EIBAID   TO STARTS-EIBAID
                          MOVE CWCICS-AID      TO STARTS-TECLA
                          IF   CWCICS-STARTCODE = LOW-VALUES
                               IF  STARTS-LENGTH = 0
                                   MOVE 'S ' TO STARTS-STARTCODE
                               ELSE
                                   MOVE 'SD' TO STARTS-STARTCODE
                               END-IF
                          ELSE
                               MOVE CWCICS-STARTCODE TO STARTS-STARTCODE
                          END-IF
                          PERFORM TEST AFTER UNTIL FS-STARTS NOT = '22'
                               IF CWCICS-QUEUE = LOW-VALUES
                                  COMPUTE SZ-STARTS = 56 + STARTS-LENGTH
                                  IF STARTS-LENGTH > 0
                                  MOVE CWCICS-FROM(1:STARTS-LENGTH)
                                    TO STARTS-FROM(1:STARTS-LENGTH)
                                  END-IF
                               ELSE
                                  MOVE 56 TO SZ-STARTS
                               END-IF
                               WRITE STARTS-RECORD
                               ADD  1 TO STARTS-ENTRY
                          END-PERFORM
                          IF FS-STARTS > '09'
                             EXEC COBOLware ISAMerr
                                  STATUS FS-STARTS
                                  LABEL  LB-STARTS
                             END-EXEC
                          ELSE
                               IF CWCICS-QUEUE NOT = LOW-VALUES
                                  SET cvda-TS    TO TRUE
                                  PERFORM 030-SELECT-QUEUE
                                     THRU 030-99-FIM
                                  IF CWCICS-ABCODE = LOW-VALUES
                                     PERFORM 035-PUT-QUEUE
                                        THRU 035-99-FIM
                                  END-IF
                                  IF CWCICS-ABCODE NOT = LOW-VALUES
                                     DELETE STARTS RECORD
                                  END-IF
                               END-IF
                          END-IF
                          CLOSE STARTS
                       END-IF
                    END-IF
               WHEN CWCICS-STARTBR
                    PERFORM 020-FCT THRU 020-99-FIM
               WHEN CWCICS-SYNCPOINT
                    PERFORM 110-SYNCPOINT THRU 110-99-FIM
               WHEN CWCICS-UNLOCK
                    PERFORM 020-FCT THRU 020-99-FIM
      *        WHEN CWCICS-WAIT CONVID
      *        WHEN CWCICS-WAIT EVENT
      *        WHEN CWCICS-WAIT JOURNALNUM
               WHEN CWCICS-WRITE
                AND cvda-OPERATOR
                    IF   CWCICS-TEXTLENGTH > 120
                    OR   CWCICS-TEXTLENGTH = 0
                         SET  CWCICS-INVREQ TO TRUE
                         MOVE 1             TO cics-resp2
                    ELSE
                         MOVE "/dev/tty0" TO LB-LOG
                         PERFORM TEST AFTER UNTIL FS-LOG NOT = '9A'
                                 OPEN EXTEND LOG
                                 IF FS-LOG < '10'
                                    WRITE LOG-REG FROM CWCICS-FROM
                                 END-IF
                         END-PERFORM
                         CLOSE LOG
                         MOVE "cics.log"  TO LB-LOG
                         PERFORM TEST AFTER UNTIL FS-LOG NOT = '9A'
                                 OPEN EXTEND LOG
                                 IF FS-LOG < '10'
                                    WRITE LOG-REG FROM CWCICS-FROM
                                 END-IF
                         END-PERFORM
                         CLOSE LOG
                    END-IF
               WHEN CWCICS-WRITE
                AND NOT cvda-JOURNALNUM
                AND NOT cvda-OPERATOR
                    PERFORM 020-FCT THRU 020-99-FIM
      *        WHEN CWCICS-WRITE JOURNALNUM
               WHEN CWCICS-WRITEQ
                    PERFORM 030-SELECT-QUEUE THRU 030-99-FIM
                    IF cvda-TD
                       SET cwcics-writeq-td TO TRUE
                    END-IF
                    IF cvda-TS
                       SET cwcics-writeq-ts TO TRUE
                    END-IF
                    IF CWCICS-ABCODE = low-values
                       PERFORM 035-PUT-QUEUE THRU 035-99-FIM
                    END-IF
                    CLOSE TDTS
               WHEN CWCICS-XCTL
                    OPEN INPUT PPT
                    MOVE    CWCICS-PROGRAM TO PPT-PROGRAM
                    READ PPT IGNORE LOCK
                    IF  FS-PPT > '03'
                        SET CWCICS-PGMIDERR TO TRUE
                    ELSE
                        PERFORM 080-SEARCH-PATH THRU 080-99-FIM
                    END-IF
                    IF  CWCICS-ABCODE = LOW-VALUES
                        PERFORM 070-SET-CWA    THRU 070-99-FIM
                        DISPLAY "CWCHAIN"      UPON ENVIRONMENT-NAME
                        MOVE    CWCICS-PROGRAM   TO LAST-PROGRAM
                                                    TASKS-CURRENTPROG
                        DISPLAY CWCICS-PROGRAM UPON ENVIRONMENT-VALUE
                        DISPLAY "CURRENTPROG"  UPON ENVIRONMENT-NAME
                        DISPLAY CWCICS-PROGRAM UPON ENVIRONMENT-VALUE
                        MOVE  CWCICS-LENGTH TO CWALEN
                        IF    CWALEN NOT = 0
                              MOVE CWCICS-COMMAREA(1:CWALEN)
                                TO COMMAREA       (1:CWALEN)
                        END-IF
                        CALL "CWCHAIN" USING "SET"
                                             COMMAREA
                                             CWALEN
                        REWRITE TASKS-RECORD
                    END-IF
                    CLOSE PPT
                    EXIT PROGRAM
               WHEN OTHER
                    MOVE SPACES TO MSG
                    MOVE FN-N   TO TXT
                    PERFORM VARYING I FROM 1 BY 1
                             UNTIL I > 97
                            IF FN = FN-CODE(I)
                               MOVE FN-TEXT(I) TO TXT
                               EXIT PERFORM
                            END-IF
                    END-PERFORM
                    PERFORM VARYING I FROM 20 BY -1 UNTIL I = 1
                                       OR TXT(I:1) NOT = SPACE
                            CONTINUE
                    END-PERFORM
                    STRING "Comando CICS " DELIMITED BY SIZE
                              TXT(1:I)
                              DELIMITED BY SPACE
                           " nÆo implementado" DELIMITED BY SIZE
                    INTO MSG
                    EXEC COBOLware Send
                         Message MSG
                    END-EXEC
                    MOVE "CICS" TO CWCICS-ABCODE
           END-EVALUATE

           IF   NOT cwcics-dfheib
                MOVE CWCICS-ABCODE   TO ABCODE
                move    flag-data    to cics-flags
                rewrite flags-reg
           END-IF


           IF TASKN NOT = 0
              MOVE TASKN TO TASKCH-TASK
              PERFORM TEST AFTER UNTIL FS-TASKCH NOT = '9D'
                      READ TASKCH
              END-PERFORM
              IF FS-TASKCH = '23'
                 MOVE 0 TO TASKCH-PURGED
                 WRITE TASKCH-RECORD
              ELSE
                 IF FS-TASKCH > '09'
                    EXEC COBOLware ISAMerr
                         STATUS FS-TASKCH
                         LABEL  LB-TASKCH
                    END-EXEC
                 ELSE
                     IF TASKCH-PURGED = 1
                     OR CICS-PURGE
                        set close-all to true
                        DELETE TASKCH RECORD
                        IF FLAG-SYNC = 1
                           SET cvda-ROLLBACK TO TRUE
                           PERFORM 110-SYNCPOINT THRU 110-99-FIM
                        END-IF
                        CLOSE ACCUM BR flags LOCKEDS TASKS TASKCH
                        MOVE 0 TO TASKCH-PURGED
                        SET CICS-PURGE TO TRUE
                        OPEN I-O ACCUM BR flags LOCKEDS
                        MOVE 0 TO TASKN
                        MOVE 1 TO VEZ
                     ELSE
                        IF TASKCH-PRIORITY NOT = 0
                           MOVE TASKCH-PRIORITY TO TASKS-PRIORITY
                           MOVE 0               TO TASKCH-PRIORITY
                           REWRITE TASKCH-RECORD
                           REWRITE TASKS-RECORD
                        END-IF
                     END-IF
                 END-IF
              END-IF
           END-IF

           if close-all
           and (not CWCICS-START)
              set cics-purge to true
           end-if.

       000-99-FIM. EXIT PROGRAM.

       001-INITIATE-CICS.

           DISPLAY "YYYY"        UPON ENVIRONMENT-NAME
           ACCEPT   CENTURY(1:2) FROM ENVIRONMENT-VALUE

           EXEC COBOLware System
                OS-CODE UNIX
           END-EXEC

           IF  UNIX = 1
               DISPLAY "COBPATH" UPON ENVIRONMENT-NAME
               MOVE    "/"         TO DIRSEP
           ELSE
               DISPLAY "COBDIR"  UPON ENVIRONMENT-NAME
               MOVE    "\"         TO DIRSEP
           END-IF

           ACCEPT PATH FROM ENVIRONMENT-VALUE

           IF  UNIX = 1
               INSPECT PATH CONVERTING ":" TO ";"
           END-IF

           DISPLAY "DATEFORM"   UPON ENVIRONMENT-NAME
           ACCEPT  DATEFORM     FROM ENVIRONMENT-VALUE
           INSPECT DATEFORM CONVERTING 'ymda' TO 'YMDY'
           MOVE 0 TO Y
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 8
              IF DATEFORM (I: 1) NOT =  SPACE
                 ADD 1 TO Y
                 IF DATEFORM (I: 1) = 'Y'
                    MOVE 'Y' TO FULLMASK(Y:1)
                    MOVE I   TO LY
                 ELSE
                    PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 10
                            IF DATEFORM (I: 1) =  LETRAS (IX:1)
                               MOVE LETRAX(IX:1)
                                 TO LETRAS(IX:1) FULLMASK(Y:1)
                                    DATEMASK(I:1)
                               EXIT PERFORM
                            END-IF
                    END-PERFORM
                 END-IF
                 IF Y < 10
                    COMPUTE IX = I + 1
                    IF (DATEFORM (IX: 1) NOT = DATEFORM (I: 1))
                    AND (DATEFORM (IX: 1) NOT = ' ')
                        ADD  1  TO Y
                        MOVE '/'TO FULLMASK(Y:1)
                    END-IF
                 END-IF
              END-IF
           END-PERFORM
           PERFORM VARYING I FROM 10 BY -1 UNTIL I = 0
                 IF FULLMASK (I: 1) = 'Y'
                    MOVE LETRAX(YX:1)
                      TO LETRAS(YX:1) FULLMASK(I:1)
                   SUBTRACT 1 FROM YX
                   IF LY > 0
                      MOVE FULLMASK(I:1) TO DATEMASK(LY:1)
                      SUBTRACT 1 FROM LY
                   END-IF
                 END-IF
           END-PERFORM
           DISPLAY "FFSERVER"  UPON ENVIRONMENT-NAME
           ACCEPT   FFSERVER   FROM ENVIRONMENT-VALUE

           IF   FFSERVER NOT = SPACES
                MOVE "-" TO barra-t
           END-IF

           DISPLAY "APPLID"     UPON ENVIRONMENT-NAME
           ACCEPT  APPLID       FROM ENVIRONMENT-VALUE
           DISPLAY "NETNAME"    UPON ENVIRONMENT-NAME
           ACCEPT  NETNAME      FROM ENVIRONMENT-VALUE
           DISPLAY "OPID"       UPON ENVIRONMENT-NAME
           ACCEPT  OPID         FROM ENVIRONMENT-VALUE
           DISPLAY "OPSECURITY" UPON ENVIRONMENT-NAME
           ACCEPT  OPSECURITY   FROM ENVIRONMENT-VALUE
           DISPLAY "PRINSYSID"  UPON ENVIRONMENT-NAME
           ACCEPT  PRINSYSID    FROM ENVIRONMENT-VALUE
           DISPLAY "QNAME"      UPON ENVIRONMENT-NAME
           ACCEPT  QNAME        FROM ENVIRONMENT-VALUE
           DISPLAY "SIGDATA"    UPON ENVIRONMENT-NAME
           ACCEPT  SIGDATA      FROM ENVIRONMENT-VALUE
           DISPLAY "SYSID"      UPON ENVIRONMENT-NAME
           ACCEPT  SYSID        FROM ENVIRONMENT-VALUE
           CALL "FS_CREATE_DIR" USING SYSID
           OPEN I-O BR flags TCT LOCKEDS TASKS TASKCH
           move low-values to flags-reg
           move 1          TO flag-ID
           write flags-reg
           read flags

           EXEC COBOLware GetSystem
                USER;CWUSERID
                PROGRAM;CURRENTPROG
           END-EXEC
           MOVE CWUSERID    TO TCT-USERID
           MOVE LOW-VALUES  TO TCT-TRMID
           START TCT KEY NOT LESS TERMINAL-ID
           PERFORM TEST AFTER UNTIL FS-TCT NOT = '9D'
                   READ TCT NEXT RECORD LOCK
                   IF FS-TCT = '9D'
                      START TCT KEY > TERMINAL-ID
                      IF  (FS-TCT NOT = '10')
                      AND (FS-TCT NOT = '23')
                         MOVE '9D' TO FS-TCT
                      END-IF
                   END-IF
           END-PERFORM

           IF  FS-TCT > '09'
           OR (TCT-USERID NOT = CWUSERID)
               PERFORM TEST AFTER UNTIL FS-TCT = '00'
                       PERFORM 090-ABSTIME THRU 090-99-FIM
                       COMPUTE DEC-HEX = CWCICS-ABSTIME / 10
                       PERFORM VARYING I FROM 1 BY 1 UNTIL I > 2
                               COMPUTE Y = DEC-BYTE(I) + 1
                               MOVE HEX-BYTE (Y) TO TCT-TRMID-BYTE (I)
                       END-PERFORM
                       MOVE CWUSERID TO TCT-USERID
                       READ TCT IGNORE LOCK
                       IF FS-TCT < '10'
                          MOVE '9D' TO FS-TCT
                       ELSE
                          WRITE TCT-RECORD
                          IF FS-TCT = '00'
                             READ TCT WITH LOCK
                          END-IF
                       END-IF
               END-PERFORM
           ELSE
               PERFORM 090-ABSTIME THRU 090-99-FIM
           END-IF
           MOVE TCT-TRMID   TO TRMID
           DISPLAY "TRMID" UPON ENVIRONMENT-NAME
           DISPLAY TRMID   UPON ENVIRONMENT-VALUE
           DISPLAY "ANIM"    UPON ENVIRONMENT-NAME
           ACCEPT  ANIMATE   FROM ENVIRONMENT-VALUE
           IF ANIMATE = '1'
              PERFORM 002-CREATE-TASK THRU 002-99-FIM
           END-IF.

       001-99-FIM. EXIT.

       002-CREATE-TASK.

           MOVE   CICS-EIBAID TO START-EIBAID
           MOVE CWCICS-AID    TO START-TECLA
           MOVE CWCICS-LENGTH TO CWALEN
           IF  CWCICS-LENGTH NOT = 0
               MOVE CWCICS-FROM TO COMMAREA(1:CWALEN)
           END-IF
           UNLOCK TASKS
           PERFORM TEST AFTER UNTIL FS-TASKS < '10'
              MOVE HIGH-VALUES TO TASKS-TASK(1:)
              START TASKS KEY NOT GREATER TASKS-TASK
              IF FS-TASKS = '23' OR '10'
                 MOVE 1 TO TASKN
              ELSE
                 READ TASKS PREVIOUS RECORD IGNORE LOCK
                 COMPUTE TASKN = TASKS-TASK + 1
              END-IF
              INITIALIZE TASKS-RECORD
              MOVE TASKN TO TASKS-TASK
              WRITE TASKS-RECORD
              READ TASKS WITH LOCK
              IF FS-TASKS > '09'
                  EXEC COBOLware ISAMerr
                       STATUS FS-TASKS
                       LABEL  LB-TASKS
                  END-EXEC
              END-IF
           END-PERFORM

           MOVE TASKN       TO DEC-HEX

           SET tasks-cvda-CMDSECNO TO TRUE

           DISPLAY "CURRENTPROG" UPON ENVIRONMENT-NAME
           ACCEPT TASKS-CURRENTPROG FROM ENVIRONMENT-VALUE

           SET  tasks-cvda-BACKOUT  TO TRUE
           MOVE TRMID TO TASKS-FACILITY

           SET tasks-cvda-TERM TO TRUE

           MOVE ISOLATEST  TO set-ISOLATEST.

           DISPLAY 'TRANSPGM' UPON ENVIRONMENT-NAME
           ACCEPT  TASKS-PROGRAM FROM ENVIRONMENT-VALUE

           SET  tasks-cvda-PURGEABLE    TO TRUE
           MOVE 1                       TO TASKS-PRIORITY
           MOVE 1                       TO TASKS-PROCESSID
           MOVE LOW-VALUES              TO TASKS-REMOTENAME
           MOVE LOW-VALUES              TO TASKS-REMOTESYSTEM
           MOVE ABSTIME                 TO TASKS-ABSTIME
           SET  tasks-cvda-RESSECNO     TO TRUE
           SET  tasks-cvda-STATIC       TO TRUE
           SET  tasks-cvda-RUNNING      TO TRUE
           MOVE 0                       TO TASKS-RTIMEOUT
           SET  tasks-cvda-DEFAULT      TO TRUE
           MOVE CWCICS-STARTCODE        TO TASKS-STARTCODE
           SET  tasks-cvda-NOCLEAR      TO TRUE
           MOVE LOW-VALUES              TO TASKS-SUSPENDVALUE
           SET  tasks-cvda-CICSDATAKEY  TO TRUE
           SET  tasks-cvda-BELOW        TO TRUE
           SET  tasks-cvda-SPRSTRACE    TO TRUE
           MOVE "DFHTCL00"              TO TASKS-TRANCLASS
           MOVE 0                       TO TASKS-TRANPRIORITY
           DISPLAY "TRNID"            UPON ENVIRONMENT-NAME
           ACCEPT TASKS-TRANSACTION   FROM ENVIRONMENT-VALUE
           MOVE LOW-VALUES              TO TASKS-TRPROF
           MOVE 1024000                 TO TASKS-TWASIZE
           SET  tasks-cvda-INFLIGHT     TO TRUE
           MOVE CWuserid                TO TASKS-userid
           REWRITE TASKS-RECORD.

       002-99-FIM. EXIT.

       010-DFHEIB.

           CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER

           IF   X91-PARAMETER < 2
                GO TO 010-99-FIM
           END-IF
           IF START-EIBAID NOT = LOW-VALUES
              MOVE START-TECLA TO CWCICS-AID
              MOVE LOW-VALUES  TO START-EIBAID
              MOVE 0           TO START-TECLA
           END-IF
           INITIALIZE DFHEIB
           EVALUATE TRUE
               WHEN CVDA-ENTER     MOVE DFHENTER TO EIBAID
               WHEN CVDA-CLEAR     MOVE DFHCLEAR TO EIBAID
               WHEN CVDA-CLRPARTN  MOVE DFHCLRP  TO EIBAID
               WHEN CVDA-LIGHTPEN  MOVE DFHPEN   TO EIBAID
               WHEN CVDA-OPERID    MOVE DFHOPID  TO EIBAID
      *        WHEN                MOVE DFHMSRE  TO EIBAID
      *        WHEN                MOVE DFHSTRF  TO EIBAID
               WHEN CVDA-TRIGGER   MOVE DFHTRIG  TO EIBAID
               WHEN CVDA-PA1       MOVE DFHPA1   TO EIBAID
               WHEN CVDA-PA2       MOVE DFHPA2   TO EIBAID
               WHEN CVDA-PA3       MOVE DFHPA3   TO EIBAID
               WHEN CVDA-PF1       MOVE DFHPF1   TO EIBAID
               WHEN CVDA-PF2       MOVE DFHPF2   TO EIBAID
               WHEN CVDA-PF3       MOVE DFHPF3   TO EIBAID
               WHEN CVDA-PF4       MOVE DFHPF4   TO EIBAID
               WHEN CVDA-PF5       MOVE DFHPF5   TO EIBAID
               WHEN CVDA-PF6       MOVE DFHPF6   TO EIBAID
               WHEN CVDA-PF7       MOVE DFHPF7   TO EIBAID
               WHEN CVDA-PF8       MOVE DFHPF8   TO EIBAID
               WHEN CVDA-PF9       MOVE DFHPF9   TO EIBAID
               WHEN CVDA-PF10      MOVE DFHPF10  TO EIBAID
               WHEN CVDA-PF11      MOVE DFHPF11  TO EIBAID
               WHEN CVDA-PF12      MOVE DFHPF12  TO EIBAID
               WHEN CVDA-PF13      MOVE DFHPF13  TO EIBAID
               WHEN CVDA-PF14      MOVE DFHPF14  TO EIBAID
               WHEN CVDA-PF15      MOVE DFHPF15  TO EIBAID
               WHEN CVDA-PF16      MOVE DFHPF16  TO EIBAID
               WHEN CVDA-PF17      MOVE DFHPF17  TO EIBAID
               WHEN CVDA-PF18      MOVE DFHPF18  TO EIBAID
               WHEN CVDA-PF19      MOVE DFHPF19  TO EIBAID
               WHEN CVDA-PF20      MOVE DFHPF20  TO EIBAID
               WHEN CVDA-PF21      MOVE DFHPF21  TO EIBAID
               WHEN CVDA-PF22      MOVE DFHPF22  TO EIBAID
               WHEN CVDA-PF23      MOVE DFHPF23  TO EIBAID
               WHEN CVDA-PF24      MOVE DFHPF24  TO EIBAID
               WHEN OTHER          MOVE DFHNULL  TO EIBAID
           END-EVALUATE
           MOVE X'FF'                                 TO EIBATT
           MOVE CWALEN                                TO EIBCALEN
           MOVE X'FF'                                 TO EIBCOMPL
           MOVE X'FF'                                 TO EIBCONF
           DISPLAY "CICS-CURSOR" UPON ENVIRONMENT-NAME
           ACCEPT   CICS-CURSOR  FROM ENVIRONMENT-VALUE
           IF (CICS-CURSOR NOT NUMERIC)
           OR  CICS-CURSOR = 0
               MOVE 0 TO EIBCPOSN
           ELSE
               SUBTRACT 1 FROM CICS-LIN
               COMPUTE EIBCPOSN = (CICS-LIN * 80)
                                +  CICS-COL
           END-IF
           MOVE TASKDATE                              TO EIBDATE
           MOVE DS                                    TO EIBDS
           MOVE X'FF'                                 TO EIBEOC
           MOVE X'FF'                                 TO EIBERR
           MOVE LOW-VALUES                            TO EIBERRCD
           MOVE LOW-VALUES                            TO EIBFMH
           MOVE FN                                    TO EIBFN
           MOVE LOW-VALUES                            TO EIBFREE
           MOVE LOW-VALUES                            TO EIBNODAT
           MOVE CWCICS-ABCODE                         TO EIBRCODE
           MOVE LOW-VALUES                            TO EIBRECV
           MOVE LOW-VALUES                            TO EIBREQID
           EVALUATE TRUE
            WHEN CWCICS-ERROR        MOVE 01 TO EIBRESP *>     AEIA
            WHEN CWCICS-TERMIDERR    MOVE 11 TO EIBRESP *>     AEIK
            WHEN CWCICS-FILENOTFOUND MOVE 12 TO EIBRESP *>     AEIL
            WHEN CWCICS-NOTFND       MOVE 13 TO EIBRESP *>     AEIM
            WHEN CWCICS-DUPREC       MOVE 14 TO EIBRESP *>     AEIN
            WHEN CWCICS-DUPKEY       MOVE 15 TO EIBRESP *>     AEIO
            WHEN CWCICS-INVREQ       MOVE 16 TO EIBRESP *>     AEIP
            WHEN CWCICS-IOERR        MOVE 17 TO EIBRESP *>     AEIQ
            WHEN CWCICS-NOSPACE      MOVE 18 TO EIBRESP *>     AEIR
            WHEN CWCICS-NOTOPEN      MOVE 19 TO EIBRESP *>     AEIS
            WHEN CWCICS-ENDFILE      MOVE 20 TO EIBRESP *>     AEIT
            WHEN CWCICS-ILLOGIC      MOVE 21 TO EIBRESP *>     AEIU
            WHEN CWCICS-LENGERR      MOVE 22 TO EIBRESP *>     AEIV
            WHEN CWCICS-QZERO        MOVE 23 TO EIBRESP *>     AEIW
            WHEN CWCICS-ITEMERR      MOVE 26 TO EIBRESP *>     AEIZ
            WHEN CWCICS-PGMIDERR     MOVE 27 TO EIBRESP *>     AEI0
            WHEN CWCICS-TRANSIDERR   MOVE 28 TO EIBRESP *>     AEI1
            WHEN CWCICS-ENDDATA      MOVE 29 TO EIBRESP *>     AEI2
            WHEN CWCICS-MAPFAIL      MOVE 36 TO EIBRESP *>     AEI9
            WHEN CWCICS-ROLLEDBACK   MOVE 82 TO EIBRESP *>     AEXJ
            WHEN CWCICS-END          MOVE 83 TO EIBRESP *>     AEXK
            WHEN CWCICS-DISABLED     MOVE 84 TO EIBRESP *>     AEXL
            WHEN CWCICS-TASKIDERR    MOVE 91 TO EIBRESP *>     AEXX
            WHEN CWCICS-MODELIDERR   MOVE 95 TO EIBRESP *>     AEX3
            WHEN CWCICS-INVMPSZ      MOVE 38 TO EIBRESP *>     AEYB
            WHEN CWCICS-JIDERR       MOVE 43 TO EIBRESP *>     AEYG
            WHEN CWCICS-QIDERR       MOVE 44 TO EIBRESP *>     AEYH
            WHEN CWCICS-SYSIDERR     MOVE 53 TO EIBRESP *>     AEYQ
            WHEN CWCICS-ISCINVREQ    MOVE 54 TO EIBRESP *>     AEYR
            WHEN CWCICS-ENVDEFERR    MOVE 56 TO EIBRESP *>     AEYT
            WHEN CWCICS-NOTALLOC     MOVE 61 TO EIBRESP *>     AEYY
            WHEN CWCICS-NOTAUTH      MOVE 70 TO EIBRESP *>     AEY7
            WHEN CWCICS-NOSTG        MOVE 42 TO EIBRESP *>     ASCP
            WHEN CWCICS-TERMERR      MOVE 81 TO EIBRESP *>     ATNI
            WHEN CWCICS-NOJBUFSP     MOVE 45 TO EIBRESP *>     A17G
           END-EVALUATE
           MOVE cics-resp2                            TO EIBRESP2
           MOVE LOW-VALUES                            TO EIBRLDBK
           MOVE RSRCE                                 TO EIBRSRCE
           MOVE SPACES                                TO RSRCE
           MOVE X'00'                                 TO EIBSIG
           MOVE X'00'                                 TO EIBSYNC
           MOVE X'00'                                 TO EIBSYNRB
           MOVE TASKN                                 TO EIBTASKN
           MOVE HHMMSS                                TO EIBTIME
           MOVE TRMID                                 TO EIBTRMID
           MOVE DFHEIB TO DFHEIB-LK(1:LENGTH OF DFHEIB)
           DISPLAY "TRNID" UPON ENVIRONMENT-NAME
           ACCEPT   EIBTRNID  FROM ENVIRONMENT-VALUE.

       010-99-FIM. EXIT.

       020-FCT.

           ACCEPT HOJE  FROM DATE
           MOVE   CENTURY TO HOJE(1:2)
           ACCEPT AGORA FROM TIME

           IF CWCICS-FILE NOT = LOW-VALUES
              MOVE CWCICS-FILE TO CWCICS-DATASET
           END-IF

           MOVE SPACES TO LB-FCT
           IF  CWCICS-SYSID NOT = LOW-VALUES
               STRING CWCICS-SYSID DELIMITED BY SPACE
                      '/cicsFCT'   DELIMITED BY SIZE
                              INTO LB-FCT
           ELSE
               STRING SYSID        DELIMITED BY SPACE
                      '/cicsFCT'   DELIMITED BY SIZE
                              INTO LB-FCT
           END-IF

           IF  LB-FCT NOT = LB-FCT-ANT
               IF  OPENED-FCT = 1
                   CLOSE FCT
                   MOVE 0 TO OPENED-FCT
               END-IF
               MOVE LB-FCT TO LB-FCT-ANT
           END-IF

           IF OPENED-FCT = 0
           OR (not cvda-MASSINSERT)
           OR CWCICS-DATASET NOT = FCT-FILE
              IF OPENED-FCT = 0
                 PERFORM TEST AFTER UNTIL FS-FCT < '10'
                         OPEN I-O FCT
                         IF  FS-FCT > '09'
                             EXEC COBOLware ISAMerr
                                  STATUS FS-FCT
                                  LABEL  LB-FCT
                             END-EXEC
                         END-IF
                 END-PERFORM
                 MOVE 1 TO OPENED-FCT
                 IF CWCICS-SetFile
                    PERFORM 021-SetFile THRU 021-99-FIM
                    go to 020-CLOSE-FCT
                 END-IF
              END-IF
           END-IF

           IF  cwcics-inquire-file
           AND cvda-start
               MOVE CWCICS-DATASET TO FCT-FILE LAST-FILE
               START FCT KEY NOT LESS FCT-FILE
               READ FCT NEXT RECORD
               IF FS-FCT > '09'
                  SET CWCICS-END TO TRUE
               ELSE
                  MOVE FCT-FILE TO CWCICS-DATASET
               END-IF
           END-IF

           IF  cwcics-inquire-file
           AND cvda-next
           AND(LAST-FILE NOT =  SPACES)
               IF FS-FCT < '10'
                  READ FCT NEXT RECORD
                  IF FS-FCT > '09'
                     SET  CWCICS-END TO TRUE
                     MOVE 2          TO cics-resp2
                  ELSE
                     MOVE FCT-FILE   TO CWCICS-DATASET
                  END-IF
               END-IF
           END-IF

           MOVE CWCICS-DATASET   TO FCT-FILE
           MOVE CWCICS-KEYLENGTH TO KEYLENGTH
           READ FCT INTO CICS-FCT-RECORD
           INVALID KEY
                   MOVE LOW-VALUES TO CICS-FCT-RECORD
                   CALL CWCICS-DATASET USING PARAMETROS-CWCICS
                        ON EXCEPTION
                           SET CWCICS-FILENOTFOUND TO TRUE
                           go to 020-CLOSE-FCT
                   END-CALL
                   SET cvda-OPEN TO TRUE
                   WRITE FCT-RECORD FROM CICS-FCT-RECORD
                   IF  FS-FCT = '22'
                       MOVE SPACES TO MSG
                       STRING "Duplicidade de nome na FCT "
                                             DELIMITED BY SIZE
                              CWCICS-DATASET DELIMITED BY SPACE
                              "("            DELIMITED BY SIZE
                              FCT-DSNAME     DELIMITED BY SPACE
                              ")"            DELIMITED BY SIZE
                        INTO MSG
                        EXEC COBOLware Send
                             Message MSG
                        END-EXEC
                        SET CWCICS-INVREQ TO TRUE
                        go to 020-CLOSE-FCT
                   ELSE
                       IF  FS-FCT > '09'
                           EXEC COBOLware ISAMerr
                                STATUS FS-FCT
                                LABEL  LB-FCT
                           END-EXEC
                       END-IF
                   END-IF
           END-READ

           IF  cwcics-inquire-file
               go to 020-CLOSE-FCT
           END-IF

           MOVE FCT-KEYLENGTH TO KEYLENGTH-FCT
           IF   cvda-DISABLED
                set CWCICS-DISABLED to true
                go to 020-CLOSE-FCT
           END-IF

           IF   Not cvda-OPEN
                set CWCICS-NOTOPEN to true
                go to 020-CLOSE-FCT
           END-IF

           EVALUATE TRUE
               WHEN CWCICS-READ
                 OR CWCICS-WRITE
                 OR CWCICS-REWRITE
                 OR CWCICS-ENDBR
                    IF (CWCICS-LENGTH < FCT-minreclen
                    AND (CWCICS-WRITE OR CWCICS-REWRITE))
                        IF  CWCICS-SYSID = LOW-VALUES
                            SET CWCICS-LENGERR TO TRUE
                            go to 020-CLOSE-FCT
                        ELSE
                            MOVE CWCICS-RECORDSIZE TO CWCICS-LENGTH
                        END-IF
                    END-IF
                    IF  cvda-UPDATE
                        If cvda-NOTUPDATABLE
                           set CWCICS-INVREQ to true
                           go to 020-CLOSE-FCT
                        END-IF
                        add  1      to FCT-LockCount
                        move OpTime to FCT-LockTime
                    END-IF
                    IF  KEYLENGTH > KEYLENGTH-FCT
                        set CWCICS-INVREQ to true
                        go to 020-CLOSE-FCT
                    END-IF
                    IF  CWCICS-READ
                    AND (cvda-KEYED OR cvda-KSDS)
                        IF  KEYLENGTH NOT = 0
                            IF (NOT cvda-GENERIC)
                               set CWCICS-INVREQ to true
                               go to 020-CLOSE-FCT
                            ELSE
                                IF  KEYLENGTH < FCT-KEYLENGTH
                                    COMPUTE I = KEYLENGTH + 1
                                    MOVE LOW-VALUES
                                      TO CWCICS-RIDFLD (I:)
                                END-IF
                            END-IF
                        END-IF
                    END-IF
                    EVALUATE TRUE
                        WHEN CWCICS-READPREV
                             If cvda-NOTREADABLE
                                set CWCICS-INVREQ to true
                                go to 020-CLOSE-FCT
                             END-IF
                             SET  cvda-PREV TO TRUE
                             add  1         to FCT-ReadPrevCount
                             move OpTime    to FCT-ReadPrevTime
                        WHEN CWCICS-READNEXT
                             If cvda-NOTREADABLE
                                set CWCICS-INVREQ to true
                                go to 020-CLOSE-FCT
                             END-IF
                             SET  cvda-NEXT TO TRUE
                             add  1         to FCT-ReadNextCount
                             move OpTime    to FCT-ReadNextTime
                        WHEN CWCICS-READ
                             If cvda-NOTREADABLE
                                set CWCICS-INVREQ to true
                                go to 020-CLOSE-FCT
                             END-IF
                             add  1       to FCT-ReadCount
                             move OpTime  to FCT-ReadTime
                        WHEN CWCICS-WRITE
                             If cvda-NOTADDABLE
                                set CWCICS-INVREQ to true
                                go to 020-CLOSE-FCT
                             END-IF
                             Add  1      TO FCT-AddCount
                             move OpTime to FCT-AddTime
                        WHEN CWCICS-REWRITE
                             If cvda-NOTUPDATABLE
                                set CWCICS-INVREQ to true
                                go to 020-CLOSE-FCT
                             END-IF
                             Add 1       TO FCT-UpdateCount
                             move OpTime to FCT-UpdateTime
                        WHEN CWCICS-STARTBR
                             If cvda-NOTBROWSABLE
                                set CWCICS-INVREQ to true
                                go to 020-CLOSE-FCT
                             END-IF
                             ADD  1      TO FCT-StartCount
                             move OpTime to FCT-StartTime
                     END-EVALUATE
                WHEN CWCICS-UNLOCK
                     Add  1      TO FCT-UnlockCount
                     move OpTime to FCT-UnlockTime
           END-EVALUATE

           MOVE CWCICS-LENGTH TO SAVELENGTH
           IF   CWCICS-SET NOT = NULL
                SET ADDRESS OF BUFFER TO CWCICS-SET
                CALL CWCICS-DATASET USING PARAMETROS-CWCICS
                                          BUFFER(1:CWCICS-LENGTH)
                     ON EXCEPTION
                        SET CWCICS-FILENOTFOUND TO TRUE
                        IF  cwcics-inquire-file
                            MOVE 1 TO cics-resp2
                        END-IF
                        go to 020-CLOSE-FCT
                END-CALL
                SET SAVESET TO NULL
           ELSE
                CALL CWCICS-DATASET USING PARAMETROS-CWCICS
                                          CWCICS-COMMAREA
                     ON EXCEPTION
                        SET CWCICS-FILENOTFOUND TO TRUE
                        go to 020-CLOSE-FCT
                END-CALL
           END-IF

           ADD CWCICS-NUMREC TO FCT-DeleteCount

           IF   CWCICS-ABCODE = LOW-VALUES
           OR  (CWCICS-DUPKEY AND CWCICS-READ)
           OR  (CWCICS-DUPKEY AND CWCICS-READNEXT)
           OR  (CWCICS-DUPKEY AND CWCICS-READPREV)
           OR  (CWCICS-DUPKEY AND CWCICS-STARTBR)
           OR  (CWCICS-DELETE AND CWCICS-NUMREC > 0)
                move OpTime to FCT-LastTime
                MOVE KEYLENGTH-FCT TO CWCICS-KEYLENGTH
                REWRITE FCT-RECORD FROM CICS-FCT-RECORD
                MOVE KEYLENGTH     TO CWCICS-KEYLENGTH
                IF  cvda-UPDATE
                    MOVE CWCICS-SYSID   TO LOCKEDS-SYSID
                    MOVE CWCICS-DATASET TO LOCKEDS-DATASET
                    READ LOCKEDS
                    IF FS-LOCKEDS = '00'
                       ADD 1 TO  LOCKEDS-RECORDS
                       REWRITE  LOCKEDS-RECORD
                    ELSE
                       MOVE 1 TO LOCKEDS-RECORDS
                       MOVE 0 TO LOCKEDS-WRITE
                                 LOCKEDS-DELETE
                                 LOCKEDS-REWRITE
                       WRITE  LOCKEDS-RECORD
                    END-IF
                ELSE
                    IF CWCICS-WRITE
                    OR CWCICS-DELETE
                    OR CWCICS-REWRITE
                       MOVE CWCICS-DATASET TO LOCKEDS-DATASET
                       READ LOCKEDS
                       IF FS-LOCKEDS = '00'
                       AND LOCKEDS-RECORDS > 0
                           SUBTRACT 1 FROM  LOCKEDS-RECORDS
                           IF  LOCKEDS-RECORDS = 0
                              CALL CWCICS-DATASET USING X"FFFF"
                           END-IF
                           IF FLAG-SYNC = 1
                              SET tasks-cvda-BACKOUT to true
                              rewrite TASKS-RECORD
                              EVALUATE TRUE
                                  WHEN CWCICS-WRITE
                                       ADD 1 TO LOCKEDS-WRITE
                                  WHEN CWCICS-DELETE
                                       ADD 1 TO LOCKEDS-DELETE
                                  WHEN CWCICS-REWRITE
                                       ADD 1 TO LOCKEDS-REWRITE
                              END-EVALUATE
                           ELSE
                              COMMIT
                           END-IF
                           REWRITE LOCKEDS-RECORD
                       END-IF
                    END-IF
                END-IF
                IF CWCICS-STARTBR
                OR CWCICS-READNEXT
                OR CWCICS-READPREV
                   IF   CWCICS-SYSID       = LOW-VALUES
                        MOVE CWCICS-SYSID TO BR-SYSID
                   ELSE
                        MOVE SYSID        TO BR-SYSID
                   END-IF
                   MOVE CWCICS-DATASET TO BR-DATASET
                   MOVE CWCICS-REQID   TO BR-REQID
                   READ BR
                        INVALID KEY
                          WRITE BR-REG
                   END-READ
                   MOVE STARTBR-RIDFLD     TO BR-RIDFLD
                   MOVE STARTBR-KEY        TO BR-KEY
                   IF CWCICS-STARTBR
                      MOVE STARTBR-RIDFLD  TO BR-RIDFLD-RESET
                      MOVE STARTBR-KEY     TO BR-KEY-RESET
                      MOVE LOW-VALUES      TO BR-KEY
                   END-IF
                   REWRITE BR-REG
                END-IF
           END-IF.

       020-CLOSE-FCT.

           IF (CWCICS-LENGTH > SAVELENGTH
           AND CWCICS-READ)
               SET CWCICS-LENGERR TO TRUE
           END-IF

           IF  OPENED-FCT = 2
           AND (NOT CWCICS-WRITE)
                MOVE 1 TO OPENED-FCT
           ELSE
                IF  cvda-MASSINSERT
                   MOVE 2 TO OPENED-FCT
                END-IF
           END-IF

           IF  OPENED-FCT = 1
               CLOSE FCT
               MOVE 0 TO OPENED-FCT
           END-IF.

       020-99-FIM. EXIT.

       021-SetFile.

           IF  (CWCICS-DSNAME   NOT = LOW-VALUES)
           AND (CWCICS-BASENAME NOT = LOW-VALUES)
           AND (CWCICS-DSNAME   NOT = CWCICS-BASENAME)
               SET CWCICS-INVREQ TO TRUE
           END-IF

           IF  CWCICS-BASENAME NOT = LOW-VALUES
               MOVE CWCICS-BASENAME TO CWCICS-DSNAME
                                       FCT-DSNAME
               MOVE LOW-VALUES      TO CWCICS-BASENAME
               READ FCT
               IF FS-FCT NOT = '00'
                  SET CWCICS-FILENOTFOUND TO TRUE
                  MOVE 18 TO cics-resp2
               ELSE
                  MOVE FCT-FILE       TO CWCICS-DATASET
               END-IF
           END-IF

           MOVE CWCICS-DATASET TO FCT-FILE
           READ FCT
           IF  FS-FCT = '00'
               MOVE SPACES TO FS-LOG
               IF  CWCICS-SYSID = LOW-VALUES
                   MOVE SYSID TO CWCICS-SYSID
               END-IF
               IF fct-cvda-KEYED
                  STRING CWCICS-SYSID   DELIMITED BY SPACE
                         '/'            DELIMITED BY SIZE
                         FCT-FILESERVER DELIMITED BY SPACE
                         FCT-BASEDSNAME DELIMITED BY SPACE
                    INTO LB-LOG
               ELSE
                  STRING CWCICS-SYSID   DELIMITED BY SPACE
                         '/'            DELIMITED BY SIZE
                         FCT-FILESERVER DELIMITED BY SPACE
                         FCT-DSNAME     DELIMITED BY SPACE
                    INTO LB-LOG
               END-IF
               OPEN INPUT LOG
           END-IF

           EVALUATE TRUE
               WHEN FS-FCT = '23'
                 OR FS-LOG = '30' OR "35"
                    SET CWCICS-FILENOTFOUND TO TRUE
                    MOVE 18 TO cics-resp2
               WHEN FS-FCT > '09'
                    SET CWCICS-IOERR TO TRUE
      *             MOVE FS1-FCT TO
                    MOVE FS2-FCT TO cics-resp2
               WHEN (NOT fct-cvda-CLOSED)
                AND (NOT cvda-CLOSED)
                    SET  CWCICS-INVREQ TO TRUE
                    MOVE 2  TO cics-resp2
               WHEN NOT fct-cvda-DISABLED
                    SET  CWCICS-INVREQ TO TRUE
                    MOVE 3  TO cics-resp2
               WHEN (set-ADD not = 0)
                AND (NOT cvda-ADD)
                AND (NOT cvda-NOTADDABLE)
                    SET  CWCICS-INVREQ TO TRUE
                    MOVE 4  TO cics-resp2
               WHEN (set-BROWSE not = 0)
                AND (NOT cvda-BROWSABLE)
                AND (NOT cvda-NOTBROWSABLE)
                    SET  CWCICS-INVREQ TO TRUE
                    MOVE 5  TO cics-resp2
               WHEN (NOT cvda-WAIT)
                AND (NOT cvda-FORCE)
                AND (NOT cvda-NOWAIT)
                    SET  CWCICS-INVREQ TO TRUE
                    MOVE 6  TO cics-resp2 *>BUSY has an invalid CVDA value
               WHEN (set-DELETE not = 0)
                AND (NOT cvda-DELETABLE)
                AND (NOT cvda-NOTDELETABLE)
                    SET  CWCICS-INVREQ TO TRUE
                    MOVE 7  TO cics-resp2
               WHEN (set-DISPOSITION not = 0)
                AND (NOT cvda-OLD)
                AND (NOT cvda-SHARE)
                    SET  CWCICS-INVREQ TO TRUE
                    MOVE 8  TO cics-resp2
               WHEN (set-EMPTYSTATUS not = 0)
                AND (NOT cvda-EMPTYREQ)
                AND (NOT cvda-NOTEMPTY)
                    SET  CWCICS-INVREQ TO TRUE
                    MOVE 10 TO cics-resp2
               WHEN set-LSRPOOLID > 8
                    SET  CWCICS-INVREQ TO TRUE
                    MOVE 11 TO cics-resp2
               WHEN (set-READ not = 0)
                AND (NOT cvda-READABLE)
                AND (NOT cvda-NOTREADABLE)
                    SET  CWCICS-INVREQ TO TRUE
                    MOVE 12 TO cics-resp2
               WHEN CWCICS-STRINGS > 255
                    SET  CWCICS-INVREQ TO TRUE
                    MOVE 13 TO cics-resp2
               WHEN (set-UPDATE not = 0)
                AND (NOT cvda-NOTUPDATABLE)
                AND (NOT cvda-UPDATABLE)
                    SET  CWCICS-INVREQ TO TRUE
                    MOVE 14 TO cics-resp2
               WHEN cvda-OPEN
                AND fct-cvda-OPEN
                    SET  CWCICS-INVREQ TO TRUE
                    MOVE 15 TO cics-resp2
               WHEN (set-OPENSTATUS NOT = 0)
                AND (NOT cvda-CLOSED)
                AND (NOT cvda-OPEN)
                    SET  CWCICS-INVREQ TO TRUE
                    MOVE 16 TO cics-resp2
               WHEN (set-ENABLESTATUS NOT = 0)
                AND (NOT cvda-DISABLED)
                AND (NOT cvda-DISABLING)
                AND (NOT cvda-ENABLED)
                AND (NOT cvda-UNENABLING)
                    SET  CWCICS-INVREQ TO TRUE
                    MOVE 17 TO cics-resp2
               WHEN FS-LOG = '9A'
                    MOVE 21 TO cics-resp2
               WHEN cvda-ENABLED
                AND (fct-cvda-DISABLING
                 OR  fct-cvda-UNENABLING)
                    SET  CWCICS-INVREQ TO TRUE
                    MOVE 22 TO cics-resp2
               WHEN set-EXCLUSIVE NOT = 0
                AND (NOT cvda-EXCTL)
                AND (NOT cvda-NOEXCTL)
                AND (NOT cvda-NOTAPPLIC)
                    SET  CWCICS-INVREQ TO TRUE
                    MOVE 23 TO cics-resp2
               WHEN (set-TABLE NOT = 0)
                AND (NOT cvda-CICSTABLE)
                AND (NOT cvda-NOTTABLE)
                AND (NOT cvda-CF)
                AND (NOT cvda-USERTABLE)
                    SET  CWCICS-INVREQ TO TRUE
                    MOVE 29 TO cics-resp2
               WHEN CWCICS-MAXNUMRECS > 99999999
                    MOVE 30 TO cics-resp2
           END-EVALUATE

           IF set-ADD not = 0
              if set-OPENSTATUS NOT = 0
                 SET  CWCICS-INVREQ TO TRUE
                 MOVE 16 TO cics-resp2
              ELSE
                 MOVE set-ADD           TO fct-ADD
              END-IF
           END-IF

           IF set-BROWSE not = 0
              if set-OPENSTATUS NOT = 0
                 SET  CWCICS-INVREQ TO TRUE
                 MOVE 16 TO cics-resp2
              ELSE
                 MOVE set-BROWSE        TO fct-BROWSE
              END-IF
           END-IF

           IF set-DELETE not = 0
              if set-OPENSTATUS NOT = 0
                 SET  CWCICS-INVREQ TO TRUE
                 MOVE 16 TO cics-resp2
              ELSE
                 MOVE set-DELETE        TO fct-DELETE
              END-IF
           END-IF

           IF set-DISPOSITION not = 0
              if set-OPENSTATUS NOT = 0
                 SET  CWCICS-INVREQ TO TRUE
                 MOVE 16 TO cics-resp2
              ELSE
                 MOVE set-DISPOSITION   TO fct-DISPOSITION
              END-IF
           END-IF

           IF set-EMPTYSTATUS not = 0
              if set-OPENSTATUS NOT = 0
                 SET  CWCICS-INVREQ TO TRUE
                 MOVE 16 TO cics-resp2
              ELSE
                 MOVE set-EMPTYSTATUS   TO fct-EMPTYSTATUS
              END-IF
           END-IF

           IF set-LSRPOOLID not = 0
              if set-OPENSTATUS NOT = 0
                 SET  CWCICS-INVREQ TO TRUE
                 MOVE 16 TO cics-resp2
              ELSE
                 MOVE set-LSRPOOLID     TO fct-LSRPOOLID
              END-IF
           END-IF

           IF set-READ not = 0
              if set-OPENSTATUS NOT = 0
                 SET  CWCICS-INVREQ TO TRUE
                 MOVE 16 TO cics-resp2
              ELSE
                 MOVE set-READ          TO fct-READ
              END-IF
           END-IF

           IF CWCICS-STRINGS not = 0
              if set-OPENSTATUS NOT = 0
                 SET  CWCICS-INVREQ TO TRUE
                 MOVE 16 TO cics-resp2
              ELSE
                 MOVE CWCICS-STRINGS    TO FCT-STRINGS
              END-IF
           END-IF

           IF set-UPDATE not = 0
              if set-OPENSTATUS NOT = 0
                 SET  CWCICS-INVREQ TO TRUE
                 MOVE 16 TO cics-resp2
              ELSE
                 MOVE set-UPDATE        TO fct-UPDATE
              END-IF
           END-IF

           IF set-OPENSTATUS NOT = 0
              If set-ENABLESTATUS NOT = 0
                 SET  CWCICS-INVREQ TO TRUE
                 MOVE 16 TO cics-resp2
              ELSE
                 MOVE set-OPENSTATUS TO fct-OPENSTATUS
              END-IF
           END-IF

           IF set-ENABLESTATUS NOT = 0
              If set-OPENSTATUS NOT = 0
                 SET  CWCICS-INVREQ TO TRUE
                 MOVE 16 TO cics-resp2
              ELSE
                 MOVE set-ENABLESTATUS  TO fct-ENABLESTATUS
              END-IF
           END-IF

           IF set-EXCLUSIVE NOT = 0
              if set-OPENSTATUS NOT = 0
                 SET  CWCICS-INVREQ TO TRUE
                 MOVE 16 TO cics-resp2
              ELSE
                 MOVE set-EXCLUSIVE     TO fct-EXCLUSIVE
              END-IF
           END-IF

           IF set-TABLE NOT = 0
              if set-OPENSTATUS NOT = 0
                 SET  CWCICS-INVREQ TO TRUE
                 MOVE 16 TO cics-resp2
              ELSE
                 MOVE set-TABLE         TO fct-TABLE
              END-IF
           END-IF

           IF CWCICS-MAXNUMRECS not = 0
              if set-OPENSTATUS NOT = 0
                 SET  CWCICS-INVREQ TO TRUE
                 MOVE 16 TO cics-resp2
              ELSE
                 MOVE CWCICS-MAXNUMRECS TO FCT-MAXNUMRECS
              END-IF
           END-IF

           IF CWCICS-ABCODE NOT = low-values
              CLOSE LOG
              GO TO 021-99-FIM
           END-IF

           IF ((CWCICS-DSNAME NOT = LOW-VALUES)
           AND (CWCICS-DSNAME NOT = FCT-DSNAME))
           OR ((CWCICS-FILESERVER NOT = LOW-VALUES)
           AND (CWCICS-FILESERVER NOT = FCT-FILESERVER))
               IF CWCICS-FILESERVER = LOW-VALUES
                  MOVE SPACES TO CWCICS-FILESERVER
               END-IF
               IF fct-cvda-KEYED
                  IF FS-FCT < '09'
                     MOVE CWCICS-DSNAME     TO FCT-DSNAME
                     MOVE CWCICS-FILESERVER TO FCT-FILESERVER
                     REWRITE FCT-RECORD
                  END-IF
               ELSE
                  MOVE FCT-DSNAME     TO OLD-DSNAME
                  MOVE FCT-FILESERVER TO OLD-FILESERVER
                  MOVE SPACES TO NEWNAME
                  STRING CWCICS-SYSID   DELIMITED BY SPACE
                         '/'            DELIMITED BY SIZE
                         CWCICS-FILESERVER DELIMITED BY SPACE
                         CWCICS-DSNAME     DELIMITED BY SPACE
                    INTO NEWNAME
                  CLOSE LOG
                  MOVE LB-LOG  TO LB1
                  MOVE NEWNAME TO LB2
                  CALL "FS_RENAME_FILE" USING LB-LOG NEWNAME
                  IF RETURN-CODE NOT = 0
                     SET  CWCICS-INVREQ TO TRUE
                     GO TO 021-99-FIM
                  END-IF
                  MOVE NEWNAME TO LB-LOG
                  OPEN INPUT LOG
                  IF fct-cvda-KSDS
                     MOVE SPACE TO LB-LOG NEWNAME
                     STRING CWCICS-SYSID   DELIMITED BY SPACE
                            '/'            DELIMITED BY SIZE
                            FCT-FILESERVER DELIMITED BY SPACE
                            FCT-DSNAME     DELIMITED BY SPACE
                            '.idx'         DELIMITED BY SIZE
                        INTO LB-LOG
                     STRING CWCICS-SYSID   DELIMITED BY SPACE
                            '/'            DELIMITED BY SIZE
                            CWCICS-FILESERVER DELIMITED BY SPACE
                            CWCICS-DSNAME     DELIMITED BY SPACE
                            '.idx'            DELIMITED BY SIZE
                        INTO NEWNAME
                     CALL "FS_RENAME_FILE" USING LB-LOG NEWNAME
                     IF RETURN-CODE NOT = 0
                        STRING CWCICS-SYSID   DELIMITED BY SPACE
                               '\'            DELIMITED BY SIZE
                               FCT-FILESERVER DELIMITED BY SPACE
                               FCT-DSNAME     DELIMITED BY SPACE
                           INTO LB-LOG
                        PERFORM VARYING I FROM LENGTH LB-LOG
                                    BY -1 UNTIL I = 1
                                OR LB-LOG(I:1) = '.'
                                CONTINUE
                        END-PERFORM
                        IF  LB-LOG(I:1) NOT = '.'
                            PERFORM VARYING I FROM 1 BY 1
                                      UNTIL I = LENGTH LB-LOG
                                OR LB-LOG(I:1) = SPACE
                            END-PERFORM
                        END-IF
                        MOVE '.idx' to lb-log(I:)
                        STRING CWCICS-SYSID      DELIMITED BY SPACE
                               '\'               DELIMITED BY SIZE
                               CWCICS-FILESERVER DELIMITED BY SPACE
                               CWCICS-DSNAME     DELIMITED BY SPACE
                           INTO NEWNAME
                        PERFORM VARYING I FROM LENGTH NEWNAME BY -1
                                  UNTIL I = 1
                                OR NEWNAME(I:1) = '.'
                                CONTINUE
                        END-PERFORM
                        IF  NEWNAME(I:1) NOT = '.'
                            PERFORM VARYING I FROM 1 BY 1
                                      UNTIL I = LENGTH NEWNAME
                                OR NEWNAME(I:1) = SPACE
                            END-PERFORM
                        END-IF
                        MOVE '.idx' to newname(I:)
                        CALL "FS_RENAME_FILE" USING LB-LOG NEWNAME
                        IF RETURN-CODE NOT = 0
                           SET  CWCICS-INVREQ TO TRUE
                           CALL "FS_RENAME_FILE" USING LB2 LB1
                           GO TO 021-99-FIM
                        END-IF
                     END-IF
                     PERFORM TEST AFTER UNTIL FS-FCT > '09'
                        MOVE OLD-DSNAME     TO FCT-BASEDSNAME
                        MOVE OLD-FILESERVER TO FCT-FILESERVER
                        READ FCT KEY FCT-KSDS
                        IF FS-FCT < '09'
                           MOVE CWCICS-DSNAME     TO FCT-BASEDSNAME
                                                     FCT-BASENAME
                           MOVE CWCICS-FILESERVER TO FCT-FILESERVER
                           REWRITE FCT-RECORD
                        END-IF
                     END-PERFORM
                     MOVE CWCICS-DATASET TO FCT-FILE
                     READ FCT
                     MOVE SPACES TO FCT-BASEDSNAME
                     REWRITE FCT-RECORD
                  END-IF
               END-IF
               MOVE CWCICS-DATASET TO FCT-FILE
               READ FCT
           END-IF

           REWRITE FCT-RECORD
           CLOSE LOG.

       021-99-FIM. EXIT.

       030-SELECT-QUEUE.

           MOVE CWCICS-QUEUE TO RSRCE

           IF  cvda-TD
               MOVE 'TD'         TO QT
           ELSE
               MOVE 'TD'         TO QT
           END-IF

           IF CWCICS-SYSID = LOW-VALUES
              MOVE SYSID TO CWCICS-SYSID
           END-IF

           MOVE SPACES TO LB-TDTS

           MOVE   CWCICS-QUEUE TO queue-lower
           INSPECT queue-lower CONVERTING MAIUSCULAS TO MINUSCULAS
           STRING CWCICS-SYSID DELIMITED BY SPACE
                  barra-t      DELIMITED BY SIZE
                  'cics'       DELIMITED BY SIZE
                  QT           DELIMITED BY SIZE
                  '-'          DELIMITED BY SIZE
                  queue-lower  DELIMITED BY SPACE
             INTO LB-TDTS

           IF  NOT CWCICS-DELETEQ
               MOVE 0 TO TENTOU
               PERFORM TEST AFTER UNTIL FS-TDTS < "10"
                    OPEN I-O TDTS
                    IF  FS-TDTS = '9%'
                        SET CWCICS-QBUSY TO TRUE
                        EXIT PERFORM
                    END-IF
                    IF FS-TDTS = '05'
                    AND CWCICS-READQ
                        DELETE FILE TDTS
                        SET CWCICS-QIDERR TO TRUE
                        EXIT PERFORM
                    END-IF
                    IF FS-TDTS > '09'
                       IF FS-TDTS = '9A'
                          ADD 1 TO TENTOU
                          IF  cvda-NOSUSPEND
                          AND (CWCICS-SYSID NOT = SYSID)
                              SET CWCICS-QBUSY TO TRUE
                              EXIT PERFORM
                          END-IF
                          IF  TENTOU > 9999
                              EXEC COBOLware ISAMerr
                                   STATUS FS-TDTS
                                   LABEL  LB-TDTS
                              END-EXEC
                              MOVE 0 TO TENTOU
                          END-IF
                       ELSE
                          SET CWCICS-IOERR TO TRUE
                          EXIT PERFORM
                       END-IF
                    END-IF
               END-PERFORM
           END-IF.

       030-99-FIM. EXIT.

       034-GET-QUEUE.

           IF  cvda-TD
               IF NOT CWCICS-RETRIEVE
                  SET cwcics-readq-td TO TRUE
               END-IF
               MOVE LOW-VALUES TO TDTS-REG(1:)
               START TDTS KEY NOT < TDTS-ITEM
               READ TDTS NEXT
               IF  FS-TDTS > '09'
                   SET CWCICS-QZERO TO TRUE
               ELSE
                   MOVE TDTS-DATA TO CWCICS-FROM
                   MOVE TDTS-ITEM TO CWCICS-ITEM
               END-IF
               DELETE TDTS RECORD
           END-IF

           IF  cvda-TS
               IF NOT CWCICS-RETRIEVE
                  SET cwcics-readq-ts TO TRUE
               END-IF
               IF CWCICS-ITEM = 0
               OR cvda-NEXT
               OR CWCICS-RETRIEVE
                  MOVE 1            TO TDTS-FLAG
                  MOVE 0            TO TDTS-ITEM
                  START TDTS KEY NOT LESS TDTS-POINTER
                  READ TDTS NEXT RECORD
                  IF FS-TDTS < '10'
                  AND TDTS-FLAG = 1
                     MOVE 0 TO TDTS-FLAG
                     REWRITE TDTS-REG
                  ELSE
                     SET CWCICS-QZERO TO TRUE
                  END-IF
               ELSE
                  MOVE CWCICS-ITEM  TO TDTS-ITEM
                  READ TDTS
               END-IF
               IF FS-TDTS < '10'
                  MOVE TDTS-DATA    TO CWCICS-INTO
                  IF CWCICS-RETRIEVE
                     DELETE TDTS RECORD
                  END-IF
               ELSE
                  IF FS-TDTS = '23'
                  OR FS-TDTS = '10'
                     SET CWCICS-ITEMERR TO TRUE
                  ELSE
                     SET CWCICS-IOERR   TO TRUE
                  END-IF
               END-IF
               IF CWCICS-ABCODE = low-values
               AND (NOT CWCICS-RETRIEVE)
                  MOVE HIGH-VALUES TO TDTS-ITEM(1:)
                  START TDTS KEY NOT GREATER TDTS-ITEM
                  IF FS-TDTS = '00'
                     READ TDTS PREVIOUS RECORD
                          IGNORE LOCK
                     IF FS-TDTS = '00'
                        MOVE TDTS-ITEM TO CWCICS-NUMITEMS
                     END-IF
                  END-IF
               END-IF
           END-IF

           CLOSE TDTS.

       034-99-FIM. EXIT.

       035-PUT-QUEUE.

           IF CWCICS-LENGTH < 1
              SET CWCICS-INVREQ TO TRUE
           ELSE
              COMPUTE SZ-TDTS = 4 + CWCICS-LENGTH
              IF NOT cvda-REWRITE
                 MOVE CWCICS-ITEM TO TDTS-ITEM
                 READ TDTS WAIT
                      INVALID KEY
                              SET CWCICS-QIDERR TO TRUE
                      NOT INVALID KEY
                          MOVE CWCICS-FROM TO TDTS-DATA
                          REWRITE TDTS-REG
                          IF FS-TDTS > "09"
                            SET CWCICS-ITEMERR TO TRUE
                          END-IF
                 END-READ
              ELSE
                 MOVE HIGH-VALUES TO TDTS-ITEM(1:)
                 START TDTS KEY NOT GREATER TDTS-ITEM
                 IF FS-TDTS > '09'
                    MOVE 1 TO TDTS-ITEM
                 ELSE
                    READ TDTS PREVIOUS RECORD
                    ADD  1 TO TDTS-ITEM
                 END-IF
                 PERFORM TEST AFTER UNTIL FS-TDTS NOT = '22'
                    MOVE CWCICS-FROM TO TDTS-DATA
                    COMPUTE SZ-TDTS = 4 + CWCICS-LENGTH
                    MOVE 1 TO TDTS-FLAG
                    WRITE TDTS-REG
                    EVALUATE TRUE
                        WHEN FS-TDTS < '10'
                             MOVE TDTS-ITEM TO CWCICS-ITEM
                        WHEN FS-TDTS = '22'
                             ADD  1 TO TDTS-ITEM
                        WHEN OTHER
                             SET CWCICS-IOERR TO TRUE
                    END-EVALUATE
                 END-PERFORM
                 IF FS-TDTS = X'3907'
                    SET CWCICS-NOSPACE TO TRUE
                 END-IF
              END-IF
           END-IF.

       035-99-FIM. EXIT.

       040-BMS.

           MOVE CWCICS-MAP      TO MAP RSRCE
           MOVE flag-AidKeys    TO CICS-AidKeys
      *    IF  CWCICS-SET NOT = NULL
      *        SET ? TO CWCICS-SET
      *    END-IF
           CALL CWCICS-MAPSET USING PARAMETROS-CWCICS
                ON EXCEPTION
                   SET CWCICS-PGMIDERR TO TRUE
                NOT ON EXCEPTION
                    IF NOT CWCICS-MAPFAIL
                       MOVE LOW-VALUES TO CWCICS-ABCODE
                       IF CWCICS-RECEIVE
                          CALL "CWAKEY" USING CWCICS-AID
                                    LENGTH OF CWCICS-AID
                          MOVE CWCICS-AID TO TECLA
                       ELSE
                          MOVE CWCICS-MAPCOLUMN TO MAPCOLUMN
                          MOVE CWCICS-MAPHEIGHT TO MAPHEIGHT
                          MOVE CWCICS-MAPLINE   TO MAPLINE
                          MOVE CWCICS-MAPWIDTH  TO MAPWIDTH
                       END-IF
                    END-IF
           END-CALL.

       040-99-FIM. EXIT.

       050-ACCUM.

           IF   OPENED-ACCUM = 0
                OPEN I-O ACCUM
                MOVE 0 TO COUNT-ACCUM
                MOVE 1 TO OPENED-ACCUM
           END-IF

           ADD  1           TO COUNT-ACCUM
           MOVE COUNT-ACCUM TO ACCUM-NUMBER
                               CURRENT-ACCUM


           CALL "CBL_READ_SCR_CHATTRS" USING X"0000"
                                             ACCUM-CHARACTERS
                                             ACCUM-ATTRIBUTES
                                             X"07D0"
           WRITE ACCUM-REG.

       050-99-FIM. EXIT.

       060-PAGE.

           IF   OPENED-ACCUM = 0
                SET CWCICS-INVREQ TO TRUE
                GO TO 060-99-FIM
           END-IF

           MOVE COUNT-ACCUM TO CURRENT-ACCUM

           PERFORM TEST AFTER UNTIL TECLA NOT = 0
                   MOVE CURRENT-ACCUM            TO ACCUM-NUMBER
                   READ ACCUM
                   CALL "CBL_WRITE_SCR_CHATTRS" USING X"0000"
                                                      ACCUM-CHARACTERS
                                                      ACCUM-ATTRIBUTES
                                                      X"07D0"
                   CALL "CWKBDC" USING "0000" CARACTER TECLA-EDIT
                   CALL "CWAKEY" USING CWCICS-AID
                             LENGTH OF CWCICS-AID
                   MOVE CWCICS-AID TO TECLA
                   EVALUATE TRUE
                       WHEN EDIT-CONTROL-PAGE-DOWN
                            IF   CURRENT-ACCUM = COUNT-ACCUM
                                 CALL X"E5"
                            ELSE
                                 MOVE COUNT-ACCUM TO CURRENT-ACCUM
                            END-IF
                            MOVE 0 TO TECLA
                       WHEN EDIT-CONTROL-PAGE-UP
                            IF   CURRENT-ACCUM = 1
                                 CALL X"E5"
                            ELSE
                                 MOVE 1           TO CURRENT-ACCUM
                            END-IF
                            MOVE 0 TO TECLA
                       WHEN EDIT-PAGE-UP
                            IF   CURRENT-ACCUM = 1
                                 CALL X"E5"
                            ELSE
                                 SUBTRACT 1 FROM CURRENT-ACCUM
                            END-IF
                            MOVE 0 TO TECLA
                       WHEN EDIT-PAGE-DOWN
                            IF   CURRENT-ACCUM = COUNT-ACCUM
                                 CALL X"E5"
                            ELSE
                                 ADD 1 TO CURRENT-ACCUM
                            END-IF
                            MOVE 0 TO TECLA
                   END-EVALUATE
           END-PERFORM

           IF   cvda-RELEASE
                CLOSE ACCUM
                MOVE 0 TO OPENED-ACCUM
           END-IF.

       060-99-FIM. EXIT.

       070-SET-CWA.

           MOVE CWCICS-PROGRAM TO RSRCE

           IF  (CWCICS-INPUTMSGLEN NOT = 0)
               IF  CWL NOT = 0
                   SET CWCICS-SET TO ADDRESS OF CWA
                   CALL "CBL_FREE_MEM" USING BY VALUE CWCICS-SET
                                            RETURNING STATUS-CODE
               END-IF
               CALL "CBL_ALLOC_MEM" USING    CWCICS-SET
                                    BY VALUE CWCICS-FLENGTH
                                             FLAG
                                   RETURNING STATUS-CODE
               SET  ADDRESS OF CWA     TO CWCICS-SET
               MOVE CWCICS-INPUTMSGLEN TO CWL
               MOVE CWCICS-INPUTMSG    TO CWA(1:CWALEN)
           END-IF.

       070-99-FIM. EXIT.

       080-SEARCH-PATH.

            PERFORM VARYING TP FROM LENGTH OF PATH BY -1 UNTIL TP = 0
                    IF PATH(TP:1) NOT = SPACE
                       EXIT PERFORM
                    END-IF
            END-PERFORM
            MOVE 0 TO Y
            PERFORM VARYING P FROM 1 BY 1 UNTIL P > TP
                                             OR  (NOT CWCICS-PGMIDERR)
               IF   PATH(P:1) = ";"
               OR   P = TP
                    PERFORM VARYING I FROM 1 BY 1
                         UNTIL I > 4
                            OR  (NOT CWCICS-PGMIDERR)
                       INITIALIZE file-details filename CWCICS-Phase
                       STRING CWCICS-PROGRAM   DELIMITED BY SPACE
                              '.'              DELIMITED BY SIZE
                              EXT(I)           DELIMITED BY SIZE
                              INTO CWCICS-Phase
                       STRING CWCICS-PhasePath DELIMITED BY SPACE
                             DIRSEP           DELIMITED BY SIZE
                             CWCICS-Phase     DELIMITED BY SPACE
                             INTO FileName
                       call "CBL_CHECK_FILE_EXIST" using filename
                                                         file-details
                                               returning status-code
                       if file-year not = 0
                          move low-values    to CWCICS-ABCODE
                          move file-size     to cwcics-PhaseSize
                          move file-day      to cwcics-PhaseDay
                          move file-month    to cwcics-PhaseMonth
                          move file-year     to cwcics-PhaseYear
                          move file-hours    to cwcics-PhaseHours
                          move file-minutes  to cwcics-PhaseMinutes
                          move file-seconds  to cwcics-PhaseSeconds
                          move file-hundredths
                                         to CWCICS-PhaseHundredths
                       end-if
                    END-PERFORM
                    MOVE 0         TO Y
                    IF CWCICS-PGMIDERR
                       MOVE SPACES    TO CWCICS-PhasePath
                                         CWCICS-Phase
                    END-IF
                ELSE
                    ADD  1         TO Y
                    MOVE PATH(P:1) TO CWCICS-PhasePath(Y:1)
                END-IF
            END-PERFORM.

       080-99-FIM. EXIT.

       090-ABSTIME.

           EXEC COBOLware Time TODAY
                DATE-FINAL (HOJE)
                TIME-FINAL (HORA)
           END-EXEC
           EXEC COBOLware Time INTERVAL
                DATE 19000101
                TIME 0
                DATE-FINAL (HOJE)
                TIME-FINAL (HORA)
                TOTAL-TIME (SEGUNDOS)
           END-EXEC
           ACCEPT AGORA FROM TIME
           MOVE CENTESIMOS TO CENTESIMOS-ABS
           EXEC COBOLware Time Today
                DATE-FINAL (HOJE)
           END-EXEC
           MOVE     HOJE    TO DAYCOUNT
           SUBTRACT 10000 FROM DAYCOUNT
           MOVE     '1231'  TO DAYCOUNT(5:4)
           EXEC COBOLware Time Interval
                DATE       (DAYCOUNT)
                DATE-FINAL (HOJE)
                DAYS-FINAL (DIAS)
           END-EXEC
           MOVE ABSTIME   TO CWCICS-ABSTIME
           IF EIBDATE(1:) = LOW-VALUES
              MOVE CENTURY(2:1) TO CT(1:1)
              ADD  2            TO CT
              MOVE CT           TO TASKDATE (1:1)
              MOVE HOJE (3:2)   TO TASKDATE (2:2)
              MOVE DIAS         TO TASKDATE (4:3)
              MOVE TASKDATE     TO EIBDATE
           END-IF.

       090-99-FIM. EXIT.

       100-INQUIRE-TASK.

           MOVE CWCICS-TASK TO TASKS-TASK
           READ TASKS
           IF FS-TASKS = '23'
              SET CWCICS-TASKIDERR TO TRUE
              MOVE 1 TO CICS-RESP2
              GO TO 100-99-FIM
           ELSE
              IF FS-TASKS > '09'
              AND (FS-TASKS NOT = '9D')
                  GO TO 100-99-FIM
              END-IF
           END-IF

           MOVE tasks-CMDSEC       TO    set-CMDSEC
           MOVE TASKS-CURRENTPROG  TO CWCICS-CURRENTPROG
           MOVE tasks-DTB          TO    set-DTB
           MOVE TASKS-DTIMEOUT     TO CWCICS-DTIMEOUT
           MOVE tasks-DUMPING      TO    set-DUMPING
           MOVE tasks-FACILITY     TO CWCICS-FACILITY
           MOVE tasks-FACILITYTYPE TO    set-FACILITYTYPE
           MOVE tasks-ISOLATEST    TO    set-ISOLATEST
           MOVE TASKS-PRIORITY     TO CWCICS-PRIORITY
           MOVE TASKS-PROCESSID    TO CWCICS-PROCESSID
           MOVE TASKS-PROFILE      TO CWCICS-PROFILE
           MOVE TASKS-PROGRAM      TO CWCICS-PROGRAM
           MOVE tasks-PURGEABILITY TO    set-PURGEABILITY
           MOVE TASKS-REMOTENAME   TO CWCICS-REMOTENAME
           MOVE tasks-RESSEC       TO    set-RESSEC
           MOVE tasks-ROUTING      TO    set-ROUTING
           MOVE TASKS-RTIMEOUT     TO CWCICS-RTIMEOUT
           PERFORM 090-ABSTIME THRU 090-99-FIM
           COMPUTE TASKS-RUNAWAY = CWCICS-ABSTIME
                                 - TASKS-ABSTIME
           MOVE tasks-RUNSTATUS    TO    set-RUNSTATUS
           MOVE tasks-SCRNSIZE     TO    set-SCRNSIZE
           MOVE TASKS-STARTCODE    TO CWCICS-STARTCODE
           MOVE tasks-STORAGECLEAR TO    set-STORAGECLEAR
           MOVE TASKS-SUSPENDVALUE TO CWCICS-SUSPENDVALUE
           MOVE TASKS-TASK         TO CWCICS-TASK
           MOVE tasks-TASKDATAKEY  TO    set-TASKDATAKEY
           MOVE tasks-TASKDATALOC  TO    set-TASKDATALOC
           MOVE TASKS-TCLASS       TO CWCICS-TCLASS
           MOVE tasks-TRACING      TO    set-TRACING
           MOVE TASKS-TRANCLASS    TO CWCICS-TRANCLASS
           MOVE TASKS-TRANPRIORITY TO CWCICS-TRANPRIORITY
           MOVE TASKS-TRANSACTION  TO CWCICS-TRANSACTION
           MOVE TASKS-TRPROF       TO CWCICS-TRPROF
           MOVE TASKS-TWASIZE      TO CWCICS-TWASIZE
           MOVE tasks-UOWSTATE     TO    set-UOWSTATE
           MOVE TASKS-USERID       TO CWCICS-USERID
           MOVE TASKN              TO TASKS-TASK
           READ TASKS WITH LOCK.

       100-99-FIM. EXIT.

       110-SYNCPOINT.

           PERFORM TEST AFTER UNTIL FS-LOCKEDS > '09'
              MOVE LOW-VALUES TO LOCKEDS-DATASET
              START LOCKEDS KEY NOT LESS LOCKEDS-KEY
              READ LOCKEDS
              IF FS-LOCKEDS < '09'
                 CALL LOCKEDS-DATASET USING X"FFFF"
                 CANCEL LOCKEDS-DATASET
                 DELETE LOCKEDS RECORD
                 IF OPENED-FCT NOT = 0
                    CLOSE FCT
                    MOVE 0 TO OPENED-FCT
                 END-IF
                 IF cvda-ROLLBACK
                 AND FLAG-SYNC = 1
                    MOVE SPACES TO LB-FCT
                    STRING LOCKEDS-SYSID DELIMITED BY SPACE
                           '/cicsFCT'   DELIMITED BY SIZE
                     INTO LB-FCT
                     PERFORM TEST AFTER UNTIL FS-FCT < '10'
                             OPEN I-O FCT
                             IF  FS-FCT > '09'
                                 EXEC COBOLware ISAMerr
                                      STATUS FS-FCT
                                      LABEL  LB-FCT
                                 END-EXEC
                             END-IF
                     END-PERFORM
                     MOVE LOCKEDS-DATASET TO FCT-FILE
                     READ FCT INTO CICS-FCT-RECORD
                     IF FS-FCT = '00'
                        SUBTRACT LOCKEDS-WRITE
                            from FCT-AddCount
                        SUBTRACT LOCKEDS-DELETE
                            from FCT-DeleteCount
                        SUBTRACT LOCKEDS-REWRITE
                            from FCT-UpdateCount
                        REWRITE FCT-RECORD FROM CICS-FCT-RECORD
                     END-IF
                     CLOSE FCT
                 END-IF
              END-IF
           END-PERFORM
           MOVE 1 TO FLAG-SYNC
           IF cvda-ROLLBACK
              ROLLBACK
           ELSE
              COMMIT
           END-IF
           SET tasks-cvda-commit to true
           rewrite tasks-record
           IF   OPENED-ACCUM = 1
                CLOSE ACCUM
                MOVE 0 TO OPENED-ACCUM
           END-IF
           IF  OPENED-FCT = 1
               REWRITE FCT-RECORD
               CLOSE FCT
               MOVE 0 TO OPENED-FCT
           END-IF.

       110-99-FIM. EXIT.

       END PROGRAM CWCICS.
