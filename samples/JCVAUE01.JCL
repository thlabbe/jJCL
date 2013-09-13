//*----------------------------------------------------------------
//JCVAUE01 JOB UTI00TX0,'CONV EBCDIC AUE',CLASS=Z,MSGCLASS=9,
//         RESTART=DEL0101,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=MOVESOL.MORNAY.JCL
//MEMBER   INCLUDE MEMBER=$$N
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES
//*----------------------------------------------------------------
//DEL0101  EXEC PGM=IEFBR14
//SYSPRINT DD SYOUT=*
//STKPCDDC DD DSN=&CIBEBC.TAUEPOP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCDDC DD DSN=&CIBEBC.TAUEUED,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCDDC DD DSN=&CIBEBC.TAUEUEC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCDDC DD DSN=&CIBEBC.TAUEREP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCDDC DD DSN=&CIBEBC.TAUEECA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCDDC DD DSN=&CIBEBC.TAUEMPO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCDDC DD DSN=&CIBEBC.TAUERPD,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCDDC DD DSN=&CIBEBC.TAUEMUD,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCDDC DD DSN=&CIBEBC.TAUELDI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCDDC DD DSN=&CIBEBC.TAUERDC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCDDC DD DSN=&CIBEBC.TAUEMUC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCDDC DD DSN=&CIBEBC.TAUELCI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCDDC DD DSN=&CIBEBC.TAUERDM,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCDDC DD DSN=&CIBEBC.TAUERCM,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCDDC DD DSN=&CIBEBC.TAUEUEM,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCDDC DD DSN=&CIBEBC.TAUEMUM,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPAUE01 DD DSN=&CIBEBC.FPAUE01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STAUEPAC DD DSN=&CIBEBC.TAUEPAC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STUECADH DD DSN=&CIBEBC.TUECADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STUEDADH DD DSN=&CIBEBC.TUEDADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STPOPCPI DD DSN=&CIBEBC.TPOPCPI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TAUEPOP
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAUEPOP
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TAUEPOP
//SORTOUT  DD DSN=&CIBEBC.TAUEPOP,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TAUEUED
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAUEUED
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TAUEUED
//SORTOUT  DD DSN=&CIBEBC.TAUEUED,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TAUEUEC
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAUEUEC
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TAUEUEC
//SORTOUT  DD DSN=&CIBEBC.TAUEUEC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TAUEREP
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAUEREP
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TAUEREP
//SORTOUT  DD DSN=&CIBEBC.TAUEREP,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TAUEECA
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAUEECA
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TAUEECA
//SORTOUT  DD DSN=&CIBEBC.TAUEECA,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TAUEMPO
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAUEMPO
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TAUEMPO
//SORTOUT  DD DSN=&CIBEBC.TAUEMPO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TAUERPD
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAUERPD
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TAUERPD
//SORTOUT  DD DSN=&CIBEBC.TAUERPD,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TAUEMUD
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAUEMUD
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TAUEMUD
//SORTOUT  DD DSN=&CIBEBC.TAUEMUD,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TAUELDI
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAUELDI
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TAUELDI
//SORTOUT  DD DSN=&CIBEBC.TAUELDI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TAUERDC
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAUERDC
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TAUERDC
//SORTOUT  DD DSN=&CIBEBC.TAUERDC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TAUEMUC
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAUEMUC
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TAUEMUC
//SORTOUT  DD DSN=&CIBEBC.TAUEMUC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TAUELCI
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAUELCI
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TAUELCI
//SORTOUT  DD DSN=&CIBEBC.TAUELCI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TAUERDM
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAUERDM
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TAUERDM
//SORTOUT  DD DSN=&CIBEBC.TAUERDM,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TAUERCM
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAUERCM
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TAUERCM
//SORTOUT  DD DSN=&CIBEBC.TAUERCM,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TAUEUEM
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAUEUEM
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TAUEUEM
//SORTOUT  DD DSN=&CIBEBC.TAUEUEM,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TAUEMUM
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAUEMUM
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TAUEMUM
//SORTOUT  DD DSN=&CIBEBC.TAUEMUM,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE BACAUE
//*----------------------------------------------------------------
//SORT0115 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.BACAUE
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.BACAUE
//SORTOUT  DD DSN=&CIBEBC.BACAUE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE FPAUE01
//*----------------------------------------------------------------
//SORT0115 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.FPAUE01
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.FPAUE01
//SORTOUT  DD DSN=&CIBEBC.FPAUE01,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TAUEPAC
//*----------------------------------------------------------------
//SORT0115 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAUEPAC
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TAUEPAC
//SORTOUT  DD DSN=&CIBEBC.TAUEPAC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TUECADH
//*----------------------------------------------------------------
//SORT0115 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TUECADH
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TUECADH
//SORTOUT  DD DSN=&CIBEBC.TUECADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TUEDADH
//*----------------------------------------------------------------
//SORT0115 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TUEDADH
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TUEDADH
//SORTOUT  DD DSN=&CIBEBC.TUEDADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TPOPCPI
//*----------------------------------------------------------------
//SORT0115 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TPOPCPI
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TPOPCPI
//SORTOUT  DD DSN=&CIBEBC.TPOPCPI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*