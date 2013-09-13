//*----------------------------------------------------------------
//JCVTIE01 JOB UTI00TX0,'CONV EBCDIC TIE',CLASS=Z,MSGCLASS=9,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=MOVESOL.MORNAY.JCL
//MEMBER   INCLUDE MEMBER=$$N
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS binaires cibles
//*----------------------------------------------------------------
//DEL0101  EXEC PGM=IEFBR14
//SYSPRINT DD SYOUT=*
//MTC      DD DSN=&CIBEBC.MTC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//CPT      DD DSN=&CIBEBC.CPT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//TIJ      DD DSN=&CIBEBC.TIJ,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FPTIE01  DD DSN=&CIBEBC.FPTIE01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//TIERSJ   DD DSN=&CIBEBC.TIERSJ,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE MTC
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.MTC
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.MTC
//SORTOUT  DD DSN=&CIBEBC.MTC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE CPT
//*----------------------------------------------------------------
//SORT0102 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.CPT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.CPT
//SORTOUT  DD DSN=&CIBEBC.CPT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TIJ
//*----------------------------------------------------------------
//SORT0103 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TIJ
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TIJ
//SORTOUT  DD DSN=&CIBEBC.TIJ,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE FPTIE01
//*----------------------------------------------------------------
//SORT0104 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.FPTIE01
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.FPTIE01
//SORTOUT  DD DSN=&CIBEBC.FPTIE01,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TIERSJ
//*----------------------------------------------------------------
//SORT0105 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.CPY1000
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TIERSJ
//SORTOUT  DD DSN=&CIBEBC.TIERSJ,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*