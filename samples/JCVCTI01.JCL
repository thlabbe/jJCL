//*----------------------------------------------------------------
//JCVCTI01 JOB UTI00TX0,'CONV EBCDIC CTI',CLASS=Z,MSGCLASS=9,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=MOVESOL.MORNAY.JCL
//MEMBER   INCLUDE MEMBER=$$N
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS binaires cibles
//*----------------------------------------------------------------
//DEL0101  EXEC PGM=IEFBR14
//SYSPRINT DD SYOUT=*
//COMTIE   DD DSN=&CIBEBC.COMTIE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//CTI      DD DSN=&CIBEBC.CTI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE COMTIE
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.CPY1000
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.COMTIE
//SORTOUT  DD DSN=&CIBEBC.COMTIE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE CTI
//*----------------------------------------------------------------
//SORT0102 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.CTI
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.CTI
//SORTOUT  DD DSN=&CIBEBC.CTI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*