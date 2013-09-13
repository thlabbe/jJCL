//*----------------------------------------------------------------
//JCVIND01 JOB UTI00TX0,'CONV EBCDIC IND',CLASS=Z,MSGCLASS=9,
//*         RESTART=SORT0111,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=MOVESOL.MORNAY.JCL
//MEMBER   INCLUDE MEMBER=$$N
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES
//*----------------------------------------------------------------
//DEL0101  EXEC PGM=IEFBR14
//SYSPRINT DD SYOUT=*
//STRININD DD DSN=&CIBEBC.TRININD,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRINROL DD DSN=&CIBEBC.TRINROL,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRINIDA DD DSN=&CIBEBC.TRINIDA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRINIDR DD DSN=&CIBEBC.TRINIDR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRCOADD DD DSN=&CIBEBC.TRCOADD,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SBREFIND DD DSN=&CIBEBC.BREFRIN,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCHADR1  DD DSN=&CIBEBC.CHADR1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*SCHADR2  DD DSN=&CIBEBC.CHADR2,
//*            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPIND1  DD DSN=&CIBEBC.FPIND1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TRININD
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRININD
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TRININD
//SORTOUT  DD DSN=&CIBEBC.TRININD,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TRINROL
//*----------------------------------------------------------------
//SORT0102 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRINROL
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TRINROL
//SORTOUT  DD DSN=&CIBEBC.TRINROL,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TRINIDA
//*----------------------------------------------------------------
//SORT0103 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRINIDA
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TRINIDA
//SORTOUT  DD DSN=&CIBEBC.TRINIDA,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TRINIDR
//*----------------------------------------------------------------
//SORT0104 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRINIDR
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TRINIDR
//SORTOUT  DD DSN=&CIBEBC.TRINIDR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TRCOADD
//*----------------------------------------------------------------
//SORT0105 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRCOADD
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TRCOADD
//SORTOUT  DD DSN=&CIBEBC.TRCOADD,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER PIVOT BREFIND
//*----------------------------------------------------------------
//SORT0106 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.BREFRIN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.BREFRIN
//SORTOUT  DD DSN=&CIBEBC.BREFRIN,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER PIVOT CHADR1
//*----------------------------------------------------------------
//SORT0107 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.CHADR
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.CHADR1
//SORTOUT  DD DSN=&CIBEBC.CHADR1,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER PIVOT FPIND1
//*----------------------------------------------------------------
//SORT0111 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.FPIND1
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.FPIND1
//SORTOUT  DD DSN=&CIBEBC.FPIND1,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*