//J9PRILO  JOB UTI00TX0,'PR BREF A04-ILO',CLASS=Z,MSGCLASS=X,
//*         RESTART=DEL0203,
//*         RESTART=SORT0202,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=PLURM.MOVE.PR.JCL.PROD
//MEMBER   INCLUDE MEMBER=$$PRE
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRILO01
//*----------------------------------------------------------------
//DEL0101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SA01TGES DD DSN=&MIG.ILO01.A01TGES,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA03ADH  DD DSN=&MIG.ILO01.A03ADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER A01TGES
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01TGES SORT A01TGEST-C-GRPT-GEST ASC ¬29:1¦
//* SA01TGES SORT A01TGEST-C-ADH ASC ¬31:9¦
//* SA01TGES SORT A01TGEST-C-ORDRE ASC ¬41:4¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01TIERG
//SORTIN   DD DISP=SHR,DSN=&SRC.A01TIERG
//SORTOUT  DD DSN=&MIG.ILO01.A01TGES,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(29,1,CH,A,
               31,9,CH,A,
               41,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A03ADH
//*----------------------------------------------------------------
//SORT0102 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA03ADH SORT A03-ADHERENT-C-GRPT-GEST ASC ¬1:1¦
//* SA03ADH SORT A03-ADHERENT-C-ADH ASC ¬3:9¦
//* SA03ADH SORT A03-ADHERENT-C-ORDRE ASC ¬13:4¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03ADHER
//SORTIN   DD DISP=SHR,DSN=&MIG.A03ADHER
//SORTOUT  DD DSN=&MIG.ILO01.A03ADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,1,CH,A,
               3,9,CH,A,
               13,4,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SAFILO1  DD DSN=&MIG.ILO01.AFILO1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRILO01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRILO01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRILO01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01TGES DD DSN=&MIG.ILO01.A01TGES.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA03ADH  DD DSN=&MIG.ILO01.A03ADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZAFILO1  DD DSN=&MIG.ILO01.AFILO1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRILO01
//*----------------------------------------------------------------
//PPRILO01 EXEC PGM=PPRILO01,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRILO01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRILO01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRILO01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SA01TGES DD DISP=SHR,DSN=&MIG.ILO01.A01TGES
//SA03ADH  DD DISP=SHR,DSN=&MIG.ILO01.A03ADH
//*--------------<FICHIERS CIBLES>---------------------------------
//SAFILO1  DD DSN=&MIG.ILO01.AFILO1,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01TGES DD DSN=&MIG.ILO01.A01TGES.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA03ADH  DD DSN=&MIG.ILO01.A03ADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZAFILO1  DD DSN=&MIG.ILO01.AFILO1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRILO02
//*----------------------------------------------------------------
//DEL0201       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFILO1   DD DSN=&MIG.ILO02.FILO1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA01HADR DD DSN=&MIG.ILO02.A01HADR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA01CONT DD DSN=&MIG.ILO02.A01CONT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA01ENTR DD DSN=&MIG.ILO02.A01ENTR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPENT2  DD DSN=&MIG.ILO02.FPENT2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPENT3  DD DSN=&MIG.ILO02.FPENT3,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FILO1
//*----------------------------------------------------------------
//SORT0201 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFILO1 SORT A01TGEST-C-ID-PERS ASC ¬16:12¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01TIERG
//SORTIN   DD DISP=SHR,DSN=&MIG.ILO01.AFILO1
//SORTOUT  DD DSN=&MIG.ILO02.FILO1,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A01HISTA
//*----------------------------------------------------------------
//SORT0202 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01HADR SORT A01-HIST-ADRESSE-C-ID-PERS ASC [17:12—
//* SA01HADR SORT A01-HIST-ADRESSE-C-TYPE-ADR ASC [29:1—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01HISTA
//SORTIN   DD DISP=SHR,DSN=&SRC.A01HISTA
//SORTOUT  DD DSN=&MIG.ILO02.A01HADR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(17,12,CH,A,
               29,1,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A01CONT
//*----------------------------------------------------------------
//SORT0203 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01CONT SORT A01-CONTACT-C-ID-PERS ASC ¬16:12¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01CONTA
//SORTIN   DD DISP=SHR,DSN=&SRC.A01CONTA
//SORTOUT  DD DSN=&MIG.ILO02.A01CONT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A01ENTR
//*----------------------------------------------------------------
//SORT0204 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01ENTR SORT A01-ENTR-ETAB-C-ID-PERS ASC ¬2:12¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01ENTR
//SORTIN   DD DISP=SHR,DSN=&SRC.A01ENTR
//SORTOUT  DD DSN=&MIG.ILO02.A01ENTR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FPENT2
//*----------------------------------------------------------------
//SORT0205 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFPENT2 SORT FPENT2-C-ID-PERS ASC ¬21:12¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.FPENT2
//SORTIN   DD DISP=SHR,DSN=&CIB.FPENT2
//SORTOUT  DD DSN=&MIG.ILO02.FPENT2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(21,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FPENT3
//*----------------------------------------------------------------
//SORT0206 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFPENT3 SORT FPENT3-C-ID-PERS ASC ¬21:12¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.FPENT3
//SORTIN   DD DISP=SHR,DSN=&CIB.FPENT3
//SORTOUT  DD DSN=&MIG.ILO02.FPENT3,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(21,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0202       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STRLOINT DD DSN=&CIB.TRLOINT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRCOADI DD DSN=&CIB.TRCOADI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SIDINT   DD DSN=&CIB.IDINT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPILO   DD DSN=&CIB.FPILO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPILO2  DD DSN=&CIB.FPILO2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPILO3  DD DSN=&CIB.FPILO3,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCHADRS  DD DSN=&CIB.CHADRS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0203       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRILO02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRILO02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRILO02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCHADRE  DD DSN=&MIG.ILO02.CHADRE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPAYS   DD DSN=&MIG.ILO02.FPAYS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFILO1   DD DSN=&MIG.ILO02.FILO1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01HADR DD DSN=&MIG.ILO02.A01HADR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01CONT DD DSN=&MIG.ILO02.A01CONT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01ENTR DD DSN=&MIG.ILO02.A01ENTR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPENT2  DD DSN=&MIG.ILO02.FPENT2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPENT3  DD DSN=&MIG.ILO02.FPENT3.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRLOINT DD DSN=&MIG.ILO02.TRLOINT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRCOADI DD DSN=&MIG.ILO02.TRCOADI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZIDINT   DD DSN=&MIG.ILO02.IDINT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPILO   DD DSN=&MIG.ILO02.FPILO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPILO2  DD DSN=&MIG.ILO02.FPILO2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPILO3  DD DSN=&MIG.ILO02.FPILO3.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCHADRS  DD DSN=&MIG.ILO02.CHADRS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRILO02
//*----------------------------------------------------------------
//PPRILO02 EXEC PGM=PPRILO02,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRILO02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRILO02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRILO02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//CHADRE   DD DISP=SHR,DSN=&CIB.CHADR3
//FPAYS    DD DISP=SHR,DSN=&TAB.CPAY
//TCOZCA   DD DISP=SHR,DSN=&TAB.TRCOZCA
//SFILO1   DD DISP=SHR,DSN=&MIG.ILO02.FILO1
//SA01HADR DD DISP=SHR,DSN=&MIG.RTI01.A01HADR
//SA01CONT DD DISP=SHR,DSN=&MIG.ILO02.A01CONT
//SA01ENTR DD DISP=SHR,DSN=&MIG.ILO02.A01ENTR
//SFPENT2  DD DISP=SHR,DSN=&MIG.ILO02.FPENT2
//SFPENT3  DD DISP=SHR,DSN=&MIG.ILO02.FPENT3
//*--------------<FICHIERS CIBLES>---------------------------------
//STRLOINT DD DSN=&CIB.TRLOINT,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STRCOADI DD DSN=&CIB.TRCOADI,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SIDINT   DD DSN=&CIB.IDINT,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFPILO   DD DSN=&CIB.FPILO,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFPILO2  DD DSN=&CIB.FPILO2,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFPILO3  DD DSN=&CIB.FPILO3,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SCHADRS  DD DSN=&CIB.CHADRS,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZCHADRE  DD DSN=&MIG.ILO02.CHADRE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPAYS   DD DSN=&MIG.ILO02.FPAYS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFILO1   DD DSN=&MIG.ILO02.FILO1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01HADR DD DSN=&MIG.ILO02.A01HADR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01CONT DD DSN=&MIG.ILO02.A01CONT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01ENTR DD DSN=&MIG.ILO02.A01ENTR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPENT2  DD DSN=&MIG.ILO02.FPENT2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPENT3  DD DSN=&MIG.ILO02.FPENT3.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRLOINT DD DSN=&MIG.ILO02.TRLOINT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRCOADI DD DSN=&MIG.ILO02.TRCOADI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZIDINT   DD DSN=&MIG.ILO02.IDINT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPILO   DD DSN=&MIG.ILO02.FPILO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPILO2  DD DSN=&MIG.ILO02.FPILO2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPILO3  DD DSN=&MIG.ILO02.FPILO3.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCHADRS  DD DSN=&MIG.ILO02.CHADRS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRILO50
//*----------------------------------------------------------------
//DEL5001       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL5002       EXEC PGM=IEFBR14,COND=(0,NE)
//SBREFILO DD DSN=&CIB.BREFILO,
//*SBREFILO DD DSN=&MIG.ILO50.BREFILO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL5003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRILO50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRILO50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRILO50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRLOINT DD DSN=&MIG.ILO50.TRLOINT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRCOADI DD DSN=&MIG.ILO50.TRCOADI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZBREFILO DD DSN=&MIG.ILO50.BREFILO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRILO50
//*----------------------------------------------------------------
//PPRILO50 EXEC PGM=PPRILO50,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRILO50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRILO50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRILO50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//TRLOINT  DD DISP=SHR,DSN=&CIB.TRLOINT
//TRCOADI  DD DISP=SHR,DSN=&CIB.TRCOADI
//*--------------<FICHIERS CIBLES>---------------------------------
//SBREFILO DD DSN=&CIB.BREFILO,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRLOINT DD DSN=&MIG.ILO50.TRLOINT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRCOADI DD DSN=&MIG.ILO50.TRCOADI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZBREFILO DD DSN=&MIG.ILO50.BREFILO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*
//*=========================================
//* Conversion en ebcdic des fichiers cibles
//*=========================================
//*
//*JOB99  EXEC JCL=JCVILO01
//*
//*
//* FIN DU JCL DE MIGRATION