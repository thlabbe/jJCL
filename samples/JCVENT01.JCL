//*----------------------------------------------------------------
//JCVENT01 JOB UTI00TX0,'CONV EBCDIC ENT',CLASS=Z,MSGCLASS=9,
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
//SCHADR3  DD DSN=&CIBEBC.CHADR3,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENADH DD DSN=&CIBEBC.TRENADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENETB DD DSN=&CIBEBC.TRENETB,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENENT DD DSN=&CIBEBC.TRENENT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENRAT DD DSN=&CIBEBC.TRENRAT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENGRP DD DSN=&CIBEBC.TRENGRP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENAPB DD DSN=&CIBEBC.TRENAPB,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENAPE DD DSN=&CIBEBC.TRENAPE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENCJU DD DSN=&CIBEBC.TRENCJU,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENCVE DD DSN=&CIBEBC.TRENCVE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENDFT DD DSN=&CIBEBC.TRENDFT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENENS DD DSN=&CIBEBC.TRENENS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENFJU DD DSN=&CIBEBC.TRENFJU,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENIAB DD DSN=&CIBEBC.TRENIAB,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENIAE DD DSN=&CIBEBC.TRENIAE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENIEB DD DSN=&CIBEBC.TRENIEB,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENLLO DD DSN=&CIBEBC.TRENLLO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENSGE DD DSN=&CIBEBC.TRENSGE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENSIG DD DSN=&CIBEBC.TRENSIG,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENSOC DD DSN=&CIBEBC.TRENSOC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENSOM DD DSN=&CIBEBC.TRENSOM,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENVIE DD DSN=&CIBEBC.TRENVIE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENVIB DD DSN=&CIBEBC.TRENVIB,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRCOADB DD DSN=&CIBEBC.TRCOADB,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRCOHAB DD DSN=&CIBEBC.TRCOHAB,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*STRCOMPB DD DSN=&CIBEBC.TRCOMPB,
//*            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*STRCOPIE DD DSN=&CIBEBC.TRCOPIE,
//*            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*STRCOPRE DD DSN=&CIBEBC.TRCOPRE,
//*            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SBREFENT DD DSN=&CIBEBC.BREFENT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPENT1  DD DSN=&CIBEBC.FPENT1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPENT2  DD DSN=&CIBEBC.FPENT2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPENT3  DD DSN=&CIBEBC.FPENT3,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPENT4  DD DSN=&CIBEBC.FPENT4,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPENT5  DD DSN=&CIBEBC.FPENT5,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRCOMPB    DD DSN=&CIBEBC.TRCOMPB,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRCOPRE    DD DSN=&CIBEBC.ENT50.TRCOPRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRCOPIE    DD DSN=&CIBEBC.TRCOPIE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENDB     DD DSN=&CIBEBC.TRENVI3.DOUBLONS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*SFPENT6  DD DSN=&CIBEBC.FPENT6,
//*            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE BREFENT
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.BREFENT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.BREFENT
//SORTOUT  DD DSN=&CIBEBC.BREFENT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TRENADH
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRENADH
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TRENADH
//SORTOUT  DD DSN=&CIBEBC.TRENADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TRENETB
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRENETB
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TRENETB
//SORTOUT  DD DSN=&CIBEBC.TRENETB,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TRENENT
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRENENT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TRENENT
//SORTOUT  DD DSN=&CIBEBC.TRENENT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TRENRAT
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRENRAT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TRENRAT
//SORTOUT  DD DSN=&CIBEBC.TRENRAT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TRENGRP
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRENGRP
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TRENGRP
//SORTOUT  DD DSN=&CIBEBC.TRENGRP,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TRENAPB
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRENAPB
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TRENAPB
//SORTOUT  DD DSN=&CIBEBC.TRENAPB,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TRENAPE
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRENAPE
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TRENAPE
//SORTOUT  DD DSN=&CIBEBC.TRENAPE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TRENCJU
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRENCJU
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TRENCJU
//SORTOUT  DD DSN=&CIBEBC.TRENCJU,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TRENCVE
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRENCVE
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TRENCVE
//SORTOUT  DD DSN=&CIBEBC.TRENCVE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TRENDFT
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRENDFT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TRENDFT
//SORTOUT  DD DSN=&CIBEBC.TRENDFT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TRENENS
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRENENS
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TRENENS
//SORTOUT  DD DSN=&CIBEBC.TRENENS,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TRENFJU
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRENFJU
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TRENFJU
//SORTOUT  DD DSN=&CIBEBC.TRENFJU,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TRENIAB
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRENIAB
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TRENIAB
//SORTOUT  DD DSN=&CIBEBC.TRENIAB,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TRENIAE
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRENIAE
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TRENIAE
//SORTOUT  DD DSN=&CIBEBC.TRENIAE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TRENIEB
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRENIEB
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TRENIEB
//SORTOUT  DD DSN=&CIBEBC.TRENIEB,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TRENLLO
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRENLLO
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TRENLLO
//SORTOUT  DD DSN=&CIBEBC.TRENLLO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TRENSGE
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRENSGE
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TRENSGE
//SORTOUT  DD DSN=&CIBEBC.TRENSGE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TRENSIG
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRENSIG
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TRENSIG
//SORTOUT  DD DSN=&CIBEBC.TRENSIG,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TRENSOC
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRENSOC
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TRENSOC
//SORTOUT  DD DSN=&CIBEBC.TRENSOC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TRENSOM
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRENSOM
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TRENSOM
//SORTOUT  DD DSN=&CIBEBC.TRENSOM,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TRENVIE
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRENVIE
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TRENVIE
//SORTOUT  DD DSN=&CIBEBC.TRENVIE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TRENVIB
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRENVIB
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TRENVIB
//SORTOUT  DD DSN=&CIBEBC.TRENVIB,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TRCOADB
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRCOADB
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TRCOADB
//SORTOUT  DD DSN=&CIBEBC.TRCOADB,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TRCOHAB
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRCOHAB
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TRCOHAB
//SORTOUT  DD DSN=&CIBEBC.TRCOHAB,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TRCOMPB
//*----------------------------------------------------------------
//*SORT0101 EXEC PGM=SORT
//*COPY     DD DISP=SHR,DSN=&CPYCIB.TRCOMPB
//*SYSOUT   DD SYSOUT=*
//*SORTIN   DD DISP=SHR,DSN=&CIB.TRCOMPB
//*SORTOUT  DD DSN=&CIBEBC.TRCOMPB,
//*            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//*            SPACE=(TRK,(5000,500),RLSE)
//*SYSIN    DD *
//*  SORT FIELDS=NONE
//**TRANSCO=ASCII-EBCDIC
//*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TRCOPIE
//*----------------------------------------------------------------
//*SORT0101 EXEC PGM=SORT
//*COPY     DD DISP=SHR,DSN=&CPYCIB.TRCOPIE
//*SYSOUT   DD SYSOUT=*
//*SORTIN   DD DISP=SHR,DSN=&CIB.TRCOPIE
//*SORTOUT  DD DSN=&CIBEBC.TRCOPIE,
//*            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//*            SPACE=(TRK,(5000,500),RLSE)
//*SYSIN    DD *
//*  SORT FIELDS=NONE
//*TRANSCO=ASCII-EBCDIC
//*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TRCOPRE
//*----------------------------------------------------------------
//*SORT0101 EXEC PGM=SORT
//*COPY     DD DISP=SHR,DSN=&CPYCIB.TRCOPRE
//*SYSOUT   DD SYSOUT=*
//*SORTIN   DD DISP=SHR,DSN=&CIB.TRCOPRE
//*SORTOUT  DD DSN=&CIBEBC.TRCOPRE,
//*            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//*            SPACE=(TRK,(5000,500),RLSE)
//*SYSIN    DD *
//*  SORT FIELDS=NONE
//**TRANSCO=ASCII-EBCDIC
//*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE BREFENT
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.FPENT1
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.FPENT1
//SORTOUT  DD DSN=&CIBEBC.FPENT1,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE BREFENT
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.FPENT2
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.FPENT2
//SORTOUT  DD DSN=&CIBEBC.FPENT2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE BREFENT
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.FPENT3
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.FPENT3
//SORTOUT  DD DSN=&CIBEBC.FPENT3,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE BREFENT
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.FPENT4
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.FPENT4
//SORTOUT  DD DSN=&CIBEBC.FPENT4,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE BREFENT
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.FPENT5
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.FPENT5
//SORTOUT  DD DSN=&CIBEBC.FPENT5,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE BREFENT
//*----------------------------------------------------------------
//*SORT0101 EXEC PGM=SORT
//*COPY     DD DISP=SHR,DSN=&CPYMIG.FPENT6
//*SYSOUT   DD SYSOUT=*
//*SORTIN   DD DISP=SHR,DSN=&CIB.FPENT6
//*SORTOUT  DD DSN=&CIBEBC.FPENT6,
//*            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//*            SPACE=(TRK,(5000,500),RLSE)
//*SYSIN    DD *
//*  SORT FIELDS=NONE
//*TRANSCO=ASCII-EBCDIC
//*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER PIVOT CHADR3
//*----------------------------------------------------------------
//SORT0109 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.CHADR
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.CHADR3
//SORTOUT  DD DSN=&CIBEBC.CHADR3,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
 *TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER PIVOT TRCOMPB
//*----------------------------------------------------------------
//SORT0111 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRCOMPB
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TRCOMPB
//SORTOUT  DD DSN=&CIBEBC.TRCOMPB,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
 *TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER PIVOT TRCOPIE
//*----------------------------------------------------------------
//SORT0112 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRCOPIE
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TRCOPIE
//SORTOUT  DD DSN=&CIBEBC.TRCOPIE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
 *TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER PIVOT TRCOPRE
//*----------------------------------------------------------------
//SORT0113 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRCOPRE
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TRCOPRE
//SORTOUT  DD DSN=&CIBEBC.TRCOPRE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
 *TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER doublons TRENVI3
//*----------------------------------------------------------------
//SORT0114 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRENVIE
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TRENVI3.DOUBLONS
//SORTOUT  DD DSN=&CIBEBC.TRENVI3.DOUBLONS,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
 *TRANSCO=ASCII-EBCDIC
/*