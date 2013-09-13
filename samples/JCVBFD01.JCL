//*----------------------------------------------------------------
//JCVBFD01 JOB UTI00TX0,'CONV EBCDIC BFD',CLASS=Z,MSGCLASS=9,
//*         RESTART=SORT0115,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=MOVESOL.MORNAY.JCL
//MEMBER   INCLUDE MEMBER=$$N
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES
//*----------------------------------------------------------------
//DEL0101  EXEC PGM=IEFBR14
//SYSPRINT DD SYOUT=*
//STKBFFLX DD DSN=&CIBEBC.TKBFFLX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKBFADR DD DSN=&CIBEBC.TKBFADR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKBFDEB DD DSN=&CIBEBC.TKBFDEB,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKBFEVD DD DSN=&CIBEBC.TKBFEVD,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKBFENT DD DSN=&CIBEBC.TKBFENT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKBFPCO DD DSN=&CIBEBC.TKBFPCO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKBFPDR DD DSN=&CIBEBC.TKBFPDR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKBFPTS DD DSN=&CIBEBC.TKBFPTS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKBFANO DD DSN=&CIBEBC.TKBFANO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKBFSEC DD DSN=&CIBEBC.TKBFSEC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKBFTRA DD DSN=&CIBEBC.TKBFTRA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*SDROIBFD DD DSN=&CIBEBC.DROIBFD,
//*            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TKBFFLX
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TKBFFLX
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TKBFFLX
//SORTOUT  DD DSN=&CIBEBC.TKBFFLX,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TKBFADR
//*----------------------------------------------------------------
//SORT0102 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TKBFADR
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TKBFADR
//SORTOUT  DD DSN=&CIBEBC.TKBFADR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TKBFDEB
//*----------------------------------------------------------------
//SORT0103 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TKBFDEB
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TKBFDEB
//SORTOUT  DD DSN=&CIBEBC.TKBFDEB,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TKBFEVD
//*----------------------------------------------------------------
//SORT0104 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TKBFEVD
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TKBFEVD
//SORTOUT  DD DSN=&CIBEBC.TKBFEVD,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TKBFENT
//*----------------------------------------------------------------
//SORT0105 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TKBFENT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TKBFENT
//SORTOUT  DD DSN=&CIBEBC.TKBFENT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TKBFPCO
//*----------------------------------------------------------------
//SORT0107 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TKBFPCO
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TKBFPCO
//SORTOUT  DD DSN=&CIBEBC.TKBFPCO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TKBFPDR
//*----------------------------------------------------------------
//SORT0108 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TKBFPDR
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TKBFPDR
//SORTOUT  DD DSN=&CIBEBC.TKBFPDR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TKBFPTS
//*----------------------------------------------------------------
//SORT0109 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TKBFPTS
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TKBFPTS
//SORTOUT  DD DSN=&CIBEBC.TKBFPTS,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TKBFANO
//*----------------------------------------------------------------
//SORT0110 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TKBFANO
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TKBFANO
//SORTOUT  DD DSN=&CIBEBC.TKBFANO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TKBFSEC
//*----------------------------------------------------------------
//SORT0111 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TKBFSEC
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TKBFSEC
//SORTOUT  DD DSN=&CIBEBC.TKBFSEC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TKBFTRA
//*----------------------------------------------------------------
//SORT0114 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TKBFTRA
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TKBFTRA
//SORTOUT  DD DSN=&CIBEBC.TKBFTRA,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE DROIBFD
//*----------------------------------------------------------------
//*SORT0115 EXEC PGM=SORT
//*COPY     DD DISP=SHR,DSN=&CPYCIB.DROIBFD
//*SYSOUT   DD SYSOUT=*
//*SORTIN   DD DISP=SHR,DSN=&CIB.DROIBFD
//*SORTOUT  DD DSN=&CIBEBC.DROIBFD,
//*            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//*            SPACE=(TRK,(5000,500),RLSE)
//*SYSIN    DD *
//*  SORT FIELDS=NONE
//* *TRANSCO=ASCII-EBCDIC
/*
