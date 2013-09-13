//*----------------------------------------------------------------
//JCVDNS01 JOB UTI00TX0,'CONV EBCDIC DNS',CLASS=Z,MSGCLASS=9,
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
//STNKM0DS DD DSN=&CIBEBC.TNKM0DS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SENCDNS DD DSN=&CIBEBC.ENCDNS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STDNDAS DD DSN=&CIBEBC.TDNDAS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TNKM0DS
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TNKM0DS
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TNKM0DS
//SORTOUT  DD DSN=&CIBEBC.TNKM0DS,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE ENCDNS
//*----------------------------------------------------------------
//SORT0102 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.ENCDNS
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.ENCDNS
//SORTOUT  DD DSN=&CIBEBC.ENCDNS,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TDNDAS
//*----------------------------------------------------------------
//SORT0103 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TDNDAS
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TDNDAS
//SORTOUT  DD DSN=&CIBEBC.TDNDAS,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*