//*----------------------------------------------------------------
//JCVIBL01 JOB UTI00TX0,'CONV EBCDIC IBL',CLASS=Z,MSGCLASS=9,
//         RESTART=DEL0101,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=MOVESOL.MORNAY.JCL
//MEMBER   INCLUDE MEMBER=$$N
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS A CONVERTIR
//*----------------------------------------------------------------
//DEL0101  EXEC PGM=IEFBR14
//SYSPRINT DD SYOUT=*
//STIBLSVI DD DSN=&CIBEBC.TIBLSVI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STIBLRJI DD DSN=&CIBEBC.TIBLRJI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STIBLSVP DD DSN=&CIBEBC.TIBLSVP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STIBLRJP DD DSN=&CIBEBC.TIBLRJP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TIBLSVI
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TIBLSVI
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TIBLSVI
//SORTOUT  DD DSN=&CIBEBC.TIBLSVI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TIBLRJI
//*----------------------------------------------------------------
//SORT0102 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TIBLRJI
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TIBLRJI
//SORTOUT  DD DSN=&CIBEBC.TIBLRJI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TIBLSVP
//*----------------------------------------------------------------
//SORT0103 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TIBLSVP
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TIBLSVP
//SORTOUT  DD DSN=&CIBEBC.TIBLSVP,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TIBLRJP
//*----------------------------------------------------------------
//SORT0104 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TIBLRJP
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TIBLRJP
//SORTOUT  DD DSN=&CIBEBC.TIBLRJP,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*