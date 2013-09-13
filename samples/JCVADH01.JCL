//*----------------------------------------------------------------
//JCVADH01 JOB UTI00TX0,'CONV EBCDIC ADH',CLASS=Z,MSGCLASS=9,
//*         RESTART=SORT0114,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=MOVESOL.MORNAY.JCL
//MEMBER   INCLUDE MEMBER=$$N
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES
//*----------------------------------------------------------------
//DEL0101  EXEC PGM=IEFBR14
//SYSPRINT DD SYOUT=*
//STAGAADN DD DSN=&CIBEBC.TAGAADN,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STAGAHMA DD DSN=&CIBEBC.TAGAHMA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STAGABNA DD DSN=&CIBEBC.TAGABNA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STAGAHSA DD DSN=&CIBEBC.TAGAHSA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STAGACNT DD DSN=&CIBEBC.TAGACNT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STAGAIOC DD DSN=&CIBEBC.TAGAIOC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STAGACAT DD DSN=&CIBEBC.TAGACAT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STAGALCC DD DSN=&CIBEBC.TAGALCC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STAGAGAR DD DSN=&CIBEBC.TAGAGAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STAGACGF DD DSN=&CIBEBC.TAGACGF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STAGACGP DD DSN=&CIBEBC.TAGACGP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STAGATRS DD DSN=&CIBEBC.TAGATRS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STAGATVD DD DSN=&CIBEBC.TAGATVD,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SBACADH  DD DSN=&CIBEBC.BACADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SBACBNA  DD DSN=&CIBEBC.BACBNA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TAGAADN
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAGAADN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TAGAADN
//SORTOUT  DD DSN=&CIBEBC.TAGAADN,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TAGAHMA
//*----------------------------------------------------------------
//SORT0102 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAGAHMA
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TAGAHMA
//SORTOUT  DD DSN=&CIBEBC.TAGAHMA,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TAGABNA
//*----------------------------------------------------------------
//SORT0103 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAGABNA
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TAGABNA
//SORTOUT  DD DSN=&CIBEBC.TAGABNA,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TAGAHSA
//*----------------------------------------------------------------
//SORT0104 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAGAHSA
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TAGAHSA
//SORTOUT  DD DSN=&CIBEBC.TAGAHSA,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TAGACNT
//*----------------------------------------------------------------
//SORT0105 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAGACNT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TAGACNT
//SORTOUT  DD DSN=&CIBEBC.TAGACNT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TAGAIOC
//*----------------------------------------------------------------
//SORT0106 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAGAIOC
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TAGAIOC
//SORTOUT  DD DSN=&CIBEBC.TAGAIOC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TAGACAT
//*----------------------------------------------------------------
//SORT0107 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAGACAT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TAGACAT
//SORTOUT  DD DSN=&CIBEBC.TAGACAT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TAGALCC
//*----------------------------------------------------------------
//SORT0108 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAGALCC
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TAGALCC
//SORTOUT  DD DSN=&CIBEBC.TAGALCC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TAGAGAR
//*----------------------------------------------------------------
//SORT0109 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAGAGAR
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TAGAGAR
//SORTOUT  DD DSN=&CIBEBC.TAGAGAR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TAGACGF
//*----------------------------------------------------------------
//SORT0110 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAGACGF
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TAGACGF
//SORTOUT  DD DSN=&CIBEBC.TAGACGF,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TAGACGP
//*----------------------------------------------------------------
//SORT0111 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAGACGP
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TAGACGP
//SORTOUT  DD DSN=&CIBEBC.TAGACGP,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TAGATRS
//*----------------------------------------------------------------
//SORT0112 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAGATRS
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TAGATRS
//SORTOUT  DD DSN=&CIBEBC.TAGATRS,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TAGATVD
//*----------------------------------------------------------------
//SORT0113 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAGATVD
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TAGATVD
//SORTOUT  DD DSN=&CIBEBC.TAGATVD,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE BACADH
//*----------------------------------------------------------------
//SORT0114 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYINT.CPY0800
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.BACADH
//SORTOUT  DD DSN=&CIBEBC.BACADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE BACBNA
//*----------------------------------------------------------------
//SORT0115 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.BACBNA
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.BACBNA
//SORTOUT  DD DSN=&CIBEBC.BACBNA,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES
//*----------------------------------------------------------------
//DEL0216  EXEC PGM=IEFBR14
//SYSPRINT DD SYOUT=*
//FPADH2   DD DSN=&CIBEBC.FPADH2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FPADH1   DD DSN=&CIBEBC.FPADH1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//TADHADH  DD DSN=&CIBEBC.TADHADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//TCATCPI  DD DSN=&CIBEBC.TCATCPI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//TCNTCPI  DD DSN=&CIBEBC.TCNTCPI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//TCNTCTR  DD DSN=&CIBEBC.TCNTCTR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//TCNTGAR  DD DSN=&CIBEBC.TCNTGAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TCATCPI
//*----------------------------------------------------------------
//SORT0217 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TCATCPI
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TCATCPI
//SORTOUT  DD DSN=&CIBEBC.TCATCPI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE FPADH2
//*----------------------------------------------------------------
//SORT0218 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.FPADH2
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.FPADH2
//SORTOUT  DD DSN=&CIBEBC.FPADH2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TADHADH
//*----------------------------------------------------------------
//SORT0219 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TADHADH
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TADHADH
//SORTOUT  DD DSN=&CIBEBC.TADHADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TCNTCPI
//*----------------------------------------------------------------
//SORT0220 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TCNTCPI
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TCNTCPI
//SORTOUT  DD DSN=&CIBEBC.TCNTCPI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TCNTCTR
//*----------------------------------------------------------------
//SORT0221 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TCNTCTR
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TCNTCTR
//SORTOUT  DD DSN=&CIBEBC.TCNTCTR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TCNTGAR
//*----------------------------------------------------------------
//SORT0222 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TCNTGAR
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TCNTGAR
//SORTOUT  DD DSN=&CIBEBC.TCNTGAR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE FPADH1
//*----------------------------------------------------------------
//SORT0223 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.FPADH1
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.FPADH1
//SORTOUT  DD DSN=&CIBEBC.FPADH1,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
