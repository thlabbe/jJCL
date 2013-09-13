//*----------------------------------------------------------------
//JCVCTX01 JOB UTI00TX0,'CONV EBCDIC CTX',CLASS=Z,MSGCLASS=9,
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
//SACX     DD DSN=&CIBEBC.ACX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SECX     DD DSN=&CIBEBC.ECX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SEVX     DD DSN=&CIBEBC.EVX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SHEP     DD DSN=&CIBEBC.HEP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SHPS     DD DSN=&CIBEBC.HPS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SLAA     DD DSN=&CIBEBC.LAA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SPAD     DD DSN=&CIBEBC.PAD,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SPAX     DD DSN=&CIBEBC.PAX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SPCG     DD DSN=&CIBEBC.PCG,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SPGE     DD DSN=&CIBEBC.PGE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SPPC     DD DSN=&CIBEBC.PPC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SPTX     DD DSN=&CIBEBC.PTX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE PCG
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.PCG
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.PCG
//SORTOUT  DD DSN=&CIBEBC.PCG,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE HPS
//*----------------------------------------------------------------
//SORT0102 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.HPS
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.HPS
//SORTOUT  DD DSN=&CIBEBC.HPS,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE HEP
//*----------------------------------------------------------------
//SORT0103 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.HEP
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.HEP
//SORTOUT  DD DSN=&CIBEBC.HEP,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE EVX
//*----------------------------------------------------------------
//SORT0104 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.EVX
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.EVX
//SORTOUT  DD DSN=&CIBEBC.EVX,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE PAX
//*----------------------------------------------------------------
//SORT0105 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.PAX
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.PAX
//SORTOUT  DD DSN=&CIBEBC.PAX,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE PTX
//*----------------------------------------------------------------
//SORT0106 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.PTX
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.PTX
//SORTOUT  DD DSN=&CIBEBC.PTX,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER CIBLE PGE
//*----------------------------------------------------------------
//SORT0107 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.PGE
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.PGE
//SORTOUT  DD DSN=&CIBEBC.PGE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
*TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER PIVOT ACX
//*----------------------------------------------------------------
//SORT0108 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.ACX
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.ACX
//SORTOUT  DD DSN=&CIBEBC.ACX,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
  *TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER PIVOT LAA
//*----------------------------------------------------------------
//SORT0109 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.LAA
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.LAA
//SORTOUT  DD DSN=&CIBEBC.LAA,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
  *TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER PIVOT PPC
//*----------------------------------------------------------------
//SORT0110 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.PPC
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.PPC
//SORTOUT  DD DSN=&CIBEBC.PPC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
  *TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER PIVOT ECX
//*----------------------------------------------------------------
//SORT0111 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.ECX
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.ECX
//SORTOUT  DD DSN=&CIBEBC.ECX,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
  *TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER PIVOT PAD
//*----------------------------------------------------------------
//SORT0112 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.PAD
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.PAD
//SORTOUT  DD DSN=&CIBEBC.PAD,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
  *TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES
//*----------------------------------------------------------------
//DEL0201  EXEC PGM=IEFBR14
//SYSPRINT DD SYOUT=*
//SCACX    DD DSN=&CIBEBC.CACX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCLAA    DD DSN=&CIBEBC.CLAA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCECX    DD DSN=&CIBEBC.CECX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCEVX    DD DSN=&CIBEBC.CEVX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCHEP    DD DSN=&CIBEBC.CHEP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCHPS    DD DSN=&CIBEBC.CHPS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCPAD    DD DSN=&CIBEBC.CPAD,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCPAX    DD DSN=&CIBEBC.CPAX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCPCG    DD DSN=&CIBEBC.CPCG,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCPGE    DD DSN=&CIBEBC.CPGE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCPPC    DD DSN=&CIBEBC.CPPC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCPTPIV  DD DSN=&CIBEBC.CPTPIV,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCPTX    DD DSN=&CIBEBC.CPTX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SPROCTX  DD DSN=&CIBEBC.PROCTX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SPTX     DD DSN=&CIBEBC.PERSCTX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER PIVOT PROCTX
//*----------------------------------------------------------------
//SORT0201 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYMIG.CPY1000
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.PROCTX
//SORTOUT  DD DSN=&CIBEBC.PROCTX,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
  *TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER PIVOT CPCG
//*----------------------------------------------------------------
//SORT0202 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.PCG
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.CPCG
//SORTOUT  DD DSN=&CIBEBC.CPCG,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
  *TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER PIVOT CHPS
//*----------------------------------------------------------------
//SORT0203 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.HPS
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.CHPS
//SORTOUT  DD DSN=&CIBEBC.CHPS,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
  *TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER PIVOT CHEP
//*----------------------------------------------------------------
//SORT0204 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.HEP
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.CHEP
//SORTOUT  DD DSN=&CIBEBC.CHEP,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
  *TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER PIVOT CEVX
//*----------------------------------------------------------------
//SORT0205 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.EVX
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.CEVX
//SORTOUT  DD DSN=&CIBEBC.CEVX,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
  *TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER PIVOT CPAX
//*----------------------------------------------------------------
//SORT0206 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.PAX
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.CPAX
//SORTOUT  DD DSN=&CIBEBC.CPAX,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
  *TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER PIVOT CPTX
//*----------------------------------------------------------------
//SORT0207 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.PTX
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.CPTX
//SORTOUT  DD DSN=&CIBEBC.CPTX,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
  *TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER PIVOT CPGE
//*----------------------------------------------------------------
//SORT0208 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.PGE
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.CPGE
//SORTOUT  DD DSN=&CIBEBC.CPGE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
  *TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER PIVOT CACX
//*----------------------------------------------------------------
//SORT0209 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.ACX
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.CACX
//SORTOUT  DD DSN=&CIBEBC.CACX,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
  *TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER PIVOT CLAA
//*----------------------------------------------------------------
//SORT0210 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.LAA
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.CLAA
//SORTOUT  DD DSN=&CIBEBC.CLAA,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
  *TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER PIVOT CPPC
//*----------------------------------------------------------------
//SORT0211 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.PPC
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.CPPC
//SORTOUT  DD DSN=&CIBEBC.CPPC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
  *TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER PIVOT CECX
//*----------------------------------------------------------------
//SORT0212 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.ECX
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.CECX
//SORTOUT  DD DSN=&CIBEBC.CECX,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
  *TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER PIVOT CPAD
//*----------------------------------------------------------------
//SORT0213 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.PAD
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.CPAD
//SORTOUT  DD DSN=&CIBEBC.CPAD,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
  *TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER PIVOT CPTPIV
//*----------------------------------------------------------------
//SORT0214 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.CPTPIV
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.CPTPIV
//SORTOUT  DD DSN=&CIBEBC.CPTPIV,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
  *TRANSCO=ASCII-EBCDIC
/*
//*----------------------------------------------------------------
//*CONVERSION DU FICHIER PIVOT PERSCTX
//*----------------------------------------------------------------
//SORT0215 EXEC PGM=SORT
//COPY     DD DISP=SHR,DSN=&CPYCIB.PERSCTX
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.PERSCTX
//SORTOUT  DD DSN=&CIBEBC.PERSCTX,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
  *TRANSCO=ASCII-EBCDIC
/*