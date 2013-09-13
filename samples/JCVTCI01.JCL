//*----------------------------------------------------------------
//JCVTCI01 JOB UTI00TX0,'CONV EBCDIC TCI',CLASS=Z,MSGCLASS=9,
//*         RESTART=SORT0101,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=MOVESOL.MORNAY.JCL
//MEMBER   INCLUDE MEMBER=$$N
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS A CONVERTIR
//*----------------------------------------------------------------
//DEL0101  EXEC PGM=IEFBR14
//SYSPRINT DD SYOUT=*
//*SFPIVAD1 DD DSN=&CIBEBC.DROITCI,
//*            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCDDR DD DSN=&CIBEBC.TKPCDDR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCDNF DD DSN=&CIBEBC.TKPCDNF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCSVBR DD DSN=&CIBEBC.TKPCSVR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCSNF DD DSN=&CIBEBC.TKPCSNF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCDDC DD DSN=&CIBEBC.TKPCDDC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCSAL DD DSN=&CIBEBC.TKPCSAL,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCSIG DD DSN=&CIBEBC.TKPCSIG,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCDEB DD DSN=&CIBEBC.TKPCDEB,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCLOC DD DSN=&CIBEBC.TKPCLOC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCENT DD DSN=&CIBEBC.TKPCENT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCPCO DD DSN=&CIBEBC.TKPCPCO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCPDR DD DSN=&CIBEBC.TKPCPDR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCTRA DD DSN=&CIBEBC.TKPCTRA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCPTS DD DSN=&CIBEBC.TKPCPTS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE DROITCI
//*----------------------------------------------------------------
//*SORT0101 EXEC PGM=SORT
//*COPY     DD DISP=SHR,DSN=&CPYCIB.DROITCI
//*SYSOUT   DD SYSOUT=*
//*SORTIN   DD DISP=SHR,DSN=&CIB.DROITCI
//*SORTOUT  DD DSN=&CIBEBC.DROITCI,
//*            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//*            SPACE=(TRK,(5000,500),RLSE)
//*SYSIN    DD *
//*  SORT FIELDS=NONE
//* *TRANSCO=ASCII-EBCDIC
//* /*
//*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TKPCDDR
//*----------------------------------------------------------------
//SORT0102 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TKPCDDR
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TKPCDDR
//SORTOUT  DD DSN=&CIBEBC.TKPCDDR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TKPCDNF
//*----------------------------------------------------------------
//SORT0103 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TKPCDNF
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TKPCDNF
//SORTOUT  DD DSN=&CIBEBC.TKPCDNF,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TKPCSVR
//*----------------------------------------------------------------
//SORT0104 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TKPCSVR
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TKPCSVR
//SORTOUT  DD DSN=&CIBEBC.TKPCSVR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TKPCSNF
//*----------------------------------------------------------------
//SORT0105 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TKPCSNF
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TKPCSNF
//SORTOUT  DD DSN=&CIBEBC.TKPCSNF,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TKPCDDC
//*----------------------------------------------------------------
//SORT0106 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TKPCDDC
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TKPCDDC
//SORTOUT  DD DSN=&CIBEBC.TKPCDDC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TKPCSAL
//*----------------------------------------------------------------
//SORT0107 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TKPCSAL
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TKPCSAL
//SORTOUT  DD DSN=&CIBEBC.TKPCSAL,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TKPCSIG
//*----------------------------------------------------------------
//SORT0108 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TKPCSIG
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TKPCSIG
//SORTOUT  DD DSN=&CIBEBC.TKPCSIG,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TKPCDEB
//*----------------------------------------------------------------
//SORT0109 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TKPCDEB
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TKPCDEB
//SORTOUT  DD DSN=&CIBEBC.TKPCDEB,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TKPCLOC
//*----------------------------------------------------------------
//SORT0110 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TKPCLOC
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TKPCLOC
//SORTOUT  DD DSN=&CIBEBC.TKPCLOC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TKPCENT
//*----------------------------------------------------------------
//SORT0111 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TKPCENT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TKPCENT
//SORTOUT  DD DSN=&CIBEBC.TKPCENT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TKPCPCO
//*----------------------------------------------------------------
//SORT0112 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TKPCPCO
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TKPCPCO
//SORTOUT  DD DSN=&CIBEBC.TKPCPCO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TKPCPDR
//*----------------------------------------------------------------
//SORT0113 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TKPCPDR
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TKPCPDR
//SORTOUT  DD DSN=&CIBEBC.TKPCPDR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TKPCTRA
//*----------------------------------------------------------------
//SORT0113 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TKPCTRA
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TKPCTRA
//SORTOUT  DD DSN=&CIBEBC.TKPCTRA,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TKPCPTS
//*----------------------------------------------------------------
//SORT0114 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TKPCPTS
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TKPCPTS
//SORTOUT  DD DSN=&CIBEBC.TKPCPTS,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*