//*----------------------------------------------------------------
//JCVCAL01 JOB UTI00TX0,'CONV EBCDIC CAL',CLASS=Z,MSGCLASS=9,
//*         RESTART=DEL0101,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=MOVESOL.MORNAY.JCL
//MEMBER   INCLUDE MEMBER=$$N
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES
//*----------------------------------------------------------------
//DEL0101  EXEC PGM=IEFBR14
//SYSPRINT DD SYOUT=*
//HCR      DD DSN=&CIBEBC.HCR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//HAR      DD DSN=&CIBEBC.HAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ARA      DD DSN=&CIBEBC.ARA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//LCO      DD DSN=&CIBEBC.LCO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//COA      DD DSN=&CIBEBC.COA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//CAL      DD DSN=&CIBEBC.CAL,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//CALEND   DD DSN=&CIBEBC.CALEND,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCAL06   DD DSN=&CIBEBC.FCAL06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCAL10   DD DSN=&CIBEBC.FCAL10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE HCR
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.HCR
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.HCR
//SORTOUT  DD DSN=&CIBEBC.HCR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE HAR
//*----------------------------------------------------------------
//SORT0102 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.HAR
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.HAR
//SORTOUT  DD DSN=&CIBEBC.HAR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE ARA
//*----------------------------------------------------------------
//SORT0103 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.ARA
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.ARA
//SORTOUT  DD DSN=&CIBEBC.ARA,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE LCO
//*----------------------------------------------------------------
//SORT0104 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.LCO
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.LCO
//SORTOUT  DD DSN=&CIBEBC.LCO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE COA
//*----------------------------------------------------------------
//SORT0105 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.COA
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.COA
//SORTOUT  DD DSN=&CIBEBC.COA,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE CAL
//*----------------------------------------------------------------
//SORT0106 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.CAL
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.CAL
//SORTOUT  DD DSN=&CIBEBC.CAL,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE CALEND
//*----------------------------------------------------------------
//SORT0107 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.CPY1000
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.CALEND
//SORTOUT  DD DSN=&CIBEBC.CALEND,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE FCAL06
//*----------------------------------------------------------------
//SORT0108 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCAL06
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL14.FCAL06
//SORTOUT  DD DSN=&CIBEBC.FCAL06,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE FCAL10
//*----------------------------------------------------------------
//SORT0109 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCAL10
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL16.FCAL10
//SORTOUT  DD DSN=&CIBEBC.FCAL10,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*