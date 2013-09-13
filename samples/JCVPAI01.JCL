//*----------------------------------------------------------------
//JCVPAI01 JOB UTI00TX0,'CONV EBCDIC PAI',CLASS=Z,MSGCLASS=9,
//*         RESTART=SORT0106,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=MOVESOL.MORNAY.JCL
//MEMBER   INCLUDE MEMBER=$$N
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES
//*----------------------------------------------------------------
//DEL0101  EXEC PGM=IEFBR14
//SYSPRINT DD SYOUT=*
//SPAIMNT  DD DSN=&CIBEBC.PAIMNT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SAFF     DD DSN=&CIBEBC.AFF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*TBGRPAI  DD DSN=&CIBEBC.TBGRPAI,
//*            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*SWDA     DD DSN=&CIBEBC.WDA,
//*            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*SWPAIT   DD DSN=&CIBEBC.WPAIT,
//*            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SPAI     DD DSN=&CIBEBC.PAI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE PAIMNT
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.CPY1000
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.PAIMNT
//SORTOUT  DD DSN=&CIBEBC.PAIMNT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE AFF
//*----------------------------------------------------------------
//SORT0102 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.AFF
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.AFF
//SORTOUT  DD DSN=&CIBEBC.AFF,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TBGRPAI
//*----------------------------------------------------------------
//*SORT0103 EXEC PGM=SORT
//*COPY     DD DISP=SHR,DSN=&CPYCIB.TGRPAI
//*SYSOUT   DD SYSOUT=*
//*SORTIN   DD DISP=SHR,DSN=&CIB.TBGRPAI
//*SORTOUT  DD DSN=&CIBEBC.TBGRPAI,
//*            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//*            SPACE=(TRK,(5000,500),RLSE)
//*SYSIN    DD *
//*  SORT FIELDS=NONE
//*TRANSCO=ASCII-EBCDIC
//*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE WDA
//*----------------------------------------------------------------
//*SORT0104 EXEC PGM=SORT
//*COPY     DD DISP=SHR,DSN=&CPYCIB.WPAIT
//*SYSOUT   DD SYSOUT=*
//*SORTIN   DD DISP=SHR,DSN=&CIB.WDA
//*SORTOUT  DD DSN=&CIBEBC.WDA,
//*            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//*            SPACE=(TRK,(5000,500),RLSE)
//*SYSIN    DD *
//*  SORT FIELDS=NONE
//* *TRANSCO=ASCII-EBCDIC
//*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE WPAIT
//*----------------------------------------------------------------
//*SORT0105 EXEC PGM=SORT
//*COPY     DD DISP=SHR,DSN=&CPYCIB.WPAIT
//*SYSOUT   DD SYSOUT=*
//*SORTIN   DD DISP=SHR,DSN=&CIB.WPAIT
//*SORTOUT  DD DSN=&CIBEBC.WPAIT,
//*            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//*            SPACE=(TRK,(5000,500),RLSE)
//*SYSIN    DD *
//*  SORT FIELDS=NONE
//*TRANSCO=ASCII-EBCDIC
//*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE PAI
//*----------------------------------------------------------------
//SORT0106 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.PAI
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.PAI
//SORTOUT  DD DSN=&CIBEBC.PAI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*