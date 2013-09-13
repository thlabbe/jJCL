//*----------------------------------------------------------------
//JCVCRE01 JOB UTI00TX0,'CONV EBCDIC CRE',CLASS=Z,MSGCLASS=9,
//*         RESTART=SORT0107,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=MOVESOL.MORNAY.JCL
//MEMBER   INCLUDE MEMBER=$$N
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES
//*----------------------------------------------------------------
//DEL0101  EXEC PGM=IEFBR14
//SYSPRINT DD SYOUT=*
//STRECCRE DD DSN=&CIBEBC.TRECCRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STPERCTX DD DSN=&CIBEBC.TPERCTX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ORC      DD DSN=&CIBEBC.ORC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//CRE      DD DSN=&CIBEBC.CRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//PCRE     DD DSN=&CIBEBC.PCRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//CREANC   DD DSN=&CIBEBC.CREANC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//GARANN   DD DSN=&CIBEBC.FEXCL.GARANN,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//GARFER   DD DSN=&CIBEBC.FEXCL.GARFER,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ADHER    DD DSN=&CIBEBC.FEXCL.ADHER,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCRE05C  DD DSN=&CIBEBC.FCRE05C,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCRE05D  DD DSN=&CIBEBC.FCRE05D,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCRE05A  DD DSN=&CIBEBC.FCRE05A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCRE05B  DD DSN=&CIBEBC.FCRE05B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCRE06   DD DSN=&CIBEBC.FCRE06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCRE07   DD DSN=&CIBEBC.FCRE07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCRE08   DD DSN=&CIBEBC.FCRE08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCRE09   DD DSN=&CIBEBC.FCRE09,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCRE10   DD DSN=&CIBEBC.FCRE10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCRE11   DD DSN=&CIBEBC.FCRE11,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCRE14   DD DSN=&CIBEBC.FCRE14,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCRE16   DD DSN=&CIBEBC.FCRE16,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCRE17   DD DSN=&CIBEBC.FCRE17,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCRE18   DD DSN=&CIBEBC.FCRE18,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//S39CRE   DD DSN=&CIBEBC.S39CRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//WK1CRE   DD DSN=&CIBEBC.WK1CRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FEXCD21   DD DSN=&CIBEBC.FEXCL.ECRD21,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FEXNE09   DD DSN=&CIBEBC.FEXCL.MAJPESO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FEXSO09   DD DSN=&CIBEBC.FEXCL.MAJPENE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FEXNE19   DD DSN=&CIBEBC.FEXCL.FRAISNE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FEXSO19   DD DSN=&CIBEBC.FEXCL.FRAISSO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FEXCLPR   DD DSN=&CIBEBC.FEXCL.PROFIT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FEXCTXN   DD DSN=&CIBEBC.FEXCL.TAUXNUL,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FEXCCRA   DD DSN=&CIBEBC.FEXCL.CRENCRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FEXCCRC   DD DSN=&CIBEBC.FEXCL.CRENREP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FEXCCRN   DD DSN=&CIBEBC.FEXCL.CRENREA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FEXCCRR   DD DSN=&CIBEBC.FEXCL.CRENAFF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FEXCLSN   DD DSN=&CIBEBC.FEXCL.SOLDENE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ANCRE09   DD DSN=&CIBEBC.ANCRE09,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//PGARREF   DD DSN=&CIBEBC.FEXCL.PGARREF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE GARANN
//*----------------------------------------------------------------
//SORT0001 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.GARANN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.FEXCL.GARANN
//SORTOUT  DD DSN=&CIBEBC.FEXCL.GARANN,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE GARFER
//*----------------------------------------------------------------
//SORT0002 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.GARANN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.FEXCL.GARFER
//SORTOUT  DD DSN=&CIBEBC.FEXCL.GARFER,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TRECCRE
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRECCRE
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TRECCRE
//SORTOUT  DD DSN=&CIBEBC.TRECCRE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE TPERCTX
//*----------------------------------------------------------------
//SORT0102 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TPERCTX
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TPERCTX
//SORTOUT  DD DSN=&CIBEBC.TPERCTX,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE ORC
//*----------------------------------------------------------------
//SORT0103 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.CPY1000
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.ORC
//SORTOUT  DD DSN=&CIBEBC.ORC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE CRE
//*----------------------------------------------------------------
//SORT0104 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.CRE
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.CRE
//SORTOUT  DD DSN=&CIBEBC.CRE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE PCRE
//*----------------------------------------------------------------
//SORT0105 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.CRE
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.PCRE
//SORTOUT  DD DSN=&CIBEBC.PCRE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE CREANC
//*----------------------------------------------------------------
//SORT0106 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.CPY1000
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.CREANC
//SORTOUT  DD DSN=&CIBEBC.CREANC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE GARFER
//*----------------------------------------------------------------
//SORT0107 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.FEXCLADH
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.FEXCL.ADHER
//SORTOUT  DD DSN=&CIBEBC.FEXCL.ADHER,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE FCRE05C
//*----------------------------------------------------------------
//SORT0108 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE05
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE05.FCRE05C
//SORTOUT  DD DSN=&CIBEBC.FCRE05C,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE FCRE05D
//*----------------------------------------------------------------
//SORT0109 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE05
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE05.FCRE05D
//SORTOUT  DD DSN=&CIBEBC.FCRE05D,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE FCRE05A
//*----------------------------------------------------------------
//SORT0110 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYSRC.B05ECRIU
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE05.FCRE05A
//SORTOUT  DD DSN=&CIBEBC.FCRE05A,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE FCRE05B
//*----------------------------------------------------------------
//SORT0111 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYSRC.B05ECRIU
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE05.FCRE05B
//SORTOUT  DD DSN=&CIBEBC.FCRE05B,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE FCRE06
//*----------------------------------------------------------------
//SORT0112 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE05
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE06.FCRE06
//SORTOUT  DD DSN=&CIBEBC.FCRE06,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE FCRE07
//*----------------------------------------------------------------
//SORT0113 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE05
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE07.FCRE07
//SORTOUT  DD DSN=&CIBEBC.FCRE07,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE FCRE08
//*----------------------------------------------------------------
//SORT0114 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE05
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE08.FCRE08
//SORTOUT  DD DSN=&CIBEBC.FCRE08,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE FCRE09
//*----------------------------------------------------------------
//SORT0115 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE05
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE09.FCRE09
//SORTOUT  DD DSN=&CIBEBC.FCRE09,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE FCRE10
//*----------------------------------------------------------------
//SORT0116 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE10R
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE10.FCRE10R
//SORTOUT  DD DSN=&CIBEBC.FCRE10,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE FCRE11
//*----------------------------------------------------------------
//SORT0117 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE05
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE11.FCRE11
//SORTOUT  DD DSN=&CIBEBC.FCRE11,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE FCRE14
//*----------------------------------------------------------------
//SORT0118 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE05
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE14.FCRE14
//SORTOUT  DD DSN=&CIBEBC.FCRE14,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE FCRE16
//*----------------------------------------------------------------
//SORT0119 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE05
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE16.FCRE16
//SORTOUT  DD DSN=&CIBEBC.FCRE16,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE FCRE17
//*----------------------------------------------------------------
//SORT0120 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE05
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE17.FCRE17
//SORTOUT  DD DSN=&CIBEBC.FCRE17,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE FCRE18
//*----------------------------------------------------------------
//SORT0121 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE05
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE18.FCRE18
//SORTOUT  DD DSN=&CIBEBC.FCRE18,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE S39CRE
//*----------------------------------------------------------------
//SORT0122 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.S39CRE
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE39.S39CRE
//SORTOUT  DD DSN=&CIBEBC.S39CRE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE WK1CRE
//*----------------------------------------------------------------
//SORT0123 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.WK1CRE
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE21.WK1CRE
//SORTOUT  DD DSN=&CIBEBC.WK1CRE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE FEXCLD21
//*----------------------------------------------------------------
//SORT0124 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.FEXCLD21
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.FEXCL.ECRD21
//SORTOUT  DD DSN=&CIBEBC.FEXCL.ECRD21,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE FEXCLSO
//*----------------------------------------------------------------
//SORT0125 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.FEXCLMAP
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.FEXCL.MAJPESO
//SORTOUT  DD DSN=&CIBEBC.FEXCL.MAJPESO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE FEXCLNE
//*----------------------------------------------------------------
//SORT0126 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.FEXCLMAP
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.FEXCL.MAJPENE
//SORTOUT  DD DSN=&CIBEBC.FEXCL.MAJPENE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE FEXCLFRASO
//*----------------------------------------------------------------
//SORT0127 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.FEXCLFRA
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.FEXCL.FRAISSO
//SORTOUT  DD DSN=&CIBEBC.FEXCL.FRAISSO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE FEXCLFRANE
//*----------------------------------------------------------------
//SORT0128 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.FEXCLFRA
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.FEXCL.FRAISNE
//SORTOUT  DD DSN=&CIBEBC.FEXCL.FRAISNE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE FEXCLPR
//*----------------------------------------------------------------
//SORT0129 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.FEXCLPRO
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.FEXCL.PROFIT
//SORTOUT  DD DSN=&CIBEBC.FEXCL.PROFIT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE FEXCTXN
//*----------------------------------------------------------------
//SORT0130 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.FEXCLTXN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.FEXCL.TAUXNUL
//SORTOUT  DD DSN=&CIBEBC.FEXCL.TAUXNUL,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE FEXCCRA
//*----------------------------------------------------------------
//SORT0131 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.FEXCLCRA
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.FEXCL.CRENCRE
//SORTOUT  DD DSN=&CIBEBC.FEXCL.CRENCRE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE FEXCCRC
//*----------------------------------------------------------------
//SORT0132 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.FEXCLCRC
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.FEXCL.CRENREP
//SORTOUT  DD DSN=&CIBEBC.FEXCL.CRENREP,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE FEXCCRN
//*----------------------------------------------------------------
//SORT0133 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.FEXCLCRN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.FEXCL.CRENREA
//SORTOUT  DD DSN=&CIBEBC.FEXCL.CRENREA,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE FEXCCRR
//*----------------------------------------------------------------
//SORT0134 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.FEXCLCRR
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.FEXCL.CRENAFF
//SORTOUT  DD DSN=&CIBEBC.FEXCL.CRENAFF,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE FEXCLSN
//*----------------------------------------------------------------
//SORT0135 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.FEXCLSNE
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.FEXCL.SOLDENE
//SORTOUT  DD DSN=&CIBEBC.FEXCL.SOLDENE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE ANCRE09
//*----------------------------------------------------------------
//SORT0136 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.ANCRE09
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.ANCRE09
//SORTOUT  DD DSN=&CIBEBC.ANCRE09,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE PGARREF
//*----------------------------------------------------------------
//SORT0137 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.PGARREF
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.FEXCL.PGARREF
//SORTOUT  DD DSN=&CIBEBC.FEXCL.PGARREF,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*