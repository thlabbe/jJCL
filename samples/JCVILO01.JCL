//*----------------------------------------------------------------
//JCVILO01 JOB UTI00TX0,'CONV EBCDIC ILO',CLASS=Z,MSGCLASS=9,
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
//STRLOINT DD DSN=&CIBEBC.TRLOINT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRCOADI DD DSN=&CIBEBC.TRCOADI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SID-INT DD DSN=&CIBEBC.IDINT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SBREFILO DD DSN=&CIBEBC.BREFILO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPILO   DD DSN=&CIBEBC.FPILO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPILO2  DD DSN=&CIBEBC.FPILO2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPILO3  DD DSN=&CIBEBC.FPILO3,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCHADRS  DD DSN=&CIBEBC.CHADRS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TRLOINT
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRLOINT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TRLOINT
//SORTOUT  DD DSN=&CIBEBC.TRLOINT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TRCOADI
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRCOADI
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TRCOADI
//SORTOUT  DD DSN=&CIBEBC.TRCOADI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE ID-INT
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.CIDINT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.IDINT
//SORTOUT  DD DSN=&CIBEBC.IDINT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE BREFRLO
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.BREFILO
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.BREFILO
//SORTOUT  DD DSN=&CIBEBC.BREFILO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE BREFRLO
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.FPILO
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.FPILO
//SORTOUT  DD DSN=&CIBEBC.FPILO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE BREFRLO
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.FPILO2
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.FPILO2
//SORTOUT  DD DSN=&CIBEBC.FPILO2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE BREFRLO
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.FPILO3
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.FPILO3
//SORTOUT  DD DSN=&CIBEBC.FPILO3,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER PIVOT CHADRS
//*----------------------------------------------------------------
//SORT0110 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.CHADR
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.CHADRS
//SORTOUT  DD DSN=&CIBEBC.CHADRS,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
  *TRANSCO=ASCII-EBCDIC
/*