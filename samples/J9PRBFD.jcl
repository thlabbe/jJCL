//J9PRBFD  JOB UTI00TX0,'PR DROITS C06-BFD',CLASS=Z,MSGCLASS=X,
//*         RESTART=DEL0701,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=PLURM.MOVE.PR.JCL.PROD
//MEMBER   INCLUDE MEMBER=$$PRE
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRBFD01
//*----------------------------------------------------------------
//DEL0101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SJ21BFD  DD DSN=&MIG.BFD01.J21BFD,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ21DDR  DD DSN=&MIG.BFD01.J21DDR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER J21BFD
//*----------------------------------------------------------------
//SORT0103 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ21BFD SORT J21-BFD-ETAT-ADR-C-ID-PERS ASC [16:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J21BFD
//SORTIN   DD DISP=SHR,DSN=&SRC.J21BFD
//SORTOUT  DD DSN=&MIG.BFD01.J21BFD,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J21DDR
//*----------------------------------------------------------------
//SORT0104 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ21DDR SORT J21-DDR-DNF-C-ID-PERS ASC [16:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J21DDR
//SORTIN   DD DISP=SHR,DSN=&SRC.J21DDR
//SORTOUT  DD DSN=&MIG.BFD01.J21DDR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFBFDDDR DD DSN=&MIG.BFD01.FBFDDDR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRBFD01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRBFD01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRBFD01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPADR1  DD DSN=&MIG.BFD01.FPADR1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPIND1  DD DSN=&MIG.BFD01.FPIND1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ21BFD  DD DSN=&MIG.BFD01.J21BFD.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ21DDR  DD DSN=&MIG.BFD01.J21DDR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFBFDDDR DD DSN=&MIG.BFD01.FBFDDDR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRBFD01
//*----------------------------------------------------------------
//PPRBFD01 EXEC PGM=PPRBFD01,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRBFD01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRBFD01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRBFD01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//*SFPADR1  DD DISP=SHR,DSN=&MIG.BFD01.FPADR1
//SFPADR1  DD DISP=SHR,DSN=&MIG.TCI01.FPADR01
//*SFPIND1  DD DISP=SHR,DSN=&MIG.BFD01.FPIND1
//SFPIND1  DD DISP=SHR,DSN=&MIG.ADR01.FPIND1
//SJ21BFD  DD DISP=SHR,DSN=&MIG.BFD01.J21BFD
//SJ21DDR  DD DISP=SHR,DSN=&MIG.BFD01.J21DDR
//*--------------<FICHIERS CIBLES>---------------------------------
//SFBFDDDR DD DSN=&MIG.BFD01.FBFDDDR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPADR1  DD DSN=&MIG.BFD01.FPADR1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPIND1  DD DSN=&MIG.BFD01.FPIND1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZJ21BFD  DD DSN=&MIG.BFD01.J21BFD.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZJ21DDR  DD DSN=&MIG.BFD01.J21DDR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFBFDDDR DD DSN=&MIG.BFD01.FBFDDDR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRBFD02
//*----------------------------------------------------------------
//DEL0201       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFBFDDDR DD DSN=&MIG.BFD02.FBFDDDR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ21DOS  DD DSN=&MIG.BFD02.J21DOS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FBFDDDR
//*----------------------------------------------------------------
//SORT0201 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFBFDDDR SORT FBFDDDR-C-ID-DDR-DNF ASC [25:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FBFDDDR
//SORTIN   DD DISP=SHR,DSN=&MIG.BFD01.FBFDDDR
//SORTOUT  DD DSN=&MIG.BFD02.FBFDDDR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(25,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J21DOS
//*----------------------------------------------------------------
//SORT0202 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ21DOS SORT J21-DOSSIER-PRC-C-ID-DDR-DNF ASC [16:12ó
//* SJ21DOS SORT J21-DOSSIER-PRC-C-ID-DOS-PRC DESC [2:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J21DOSSI
//SORTIN   DD DISP=SHR,DSN=&SRC.J21DOSSI
//SORTOUT  DD DSN=&MIG.BFD02.J21DOS,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,12,CH,A,
               2,12,CH,D)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0202       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFBFDDOS DD DSN=&MIG.BFD02.FBFDDOS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0203       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRBFD02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRBFD02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRBFD02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFBFDDDR DD DSN=&MIG.BFD02.FBFDDDR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ21DOS  DD DSN=&MIG.BFD02.J21DOS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFBFDDOS DD DSN=&MIG.BFD02.FBFDDOS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRBFD02
//*----------------------------------------------------------------
//PPRBFD02 EXEC PGM=PPRBFD02,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRBFD02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRBFD02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRBFD02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFBFDDDR DD DISP=SHR,DSN=&MIG.BFD02.FBFDDDR
//SJ21DOS  DD DISP=SHR,DSN=&MIG.BFD02.J21DOS
//*--------------<FICHIERS CIBLES>---------------------------------
//SFBFDDOS DD DSN=&MIG.BFD02.FBFDDOS,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFBFDDDR DD DSN=&MIG.BFD02.FBFDDDR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZJ21DOS  DD DSN=&MIG.BFD02.J21DOS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFBFDDOS DD DSN=&MIG.BFD02.FBFDDOS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRBFD03
//*----------------------------------------------------------------
//DEL0301       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFBFDDDR DD DSN=&MIG.BFD03.FBFDDDR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ21BFDE DD DSN=&MIG.BFD03.J21BFD,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ21BFDA DD DSN=&MIG.BFD03.J21BFDAD,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ21ANO  DD DSN=&MIG.BFD03.J21ANO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FBFDDDR
//*----------------------------------------------------------------
//SORT0301 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFBFDDDR SORT FBFDDDR-C-ID-ETAT-ADR ASC ç13:12Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FBFDDDR
//SORTIN   DD DISP=SHR,DSN=&MIG.BFD01.FBFDDDR
//SORTOUT  DD DSN=&MIG.BFD03.FBFDDDR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(13,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J21BFDE
//*----------------------------------------------------------------
//SORT0302 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ21BFDE SORT J21-BFD-ETAT-ADR-C-ID-ETAT-ADR ASC ç2:12Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J21BFD
//SORTIN   DD DISP=SHR,DSN=&SRC.J21BFD
//SORTOUT  DD DSN=&MIG.BFD03.J21BFD,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J21BFDA
//*----------------------------------------------------------------
//SORT0303 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ21BFDA SORT J21-BFD-ADRESSE-C-ID-ETAT-ADR ASC [16:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J21BFDAD
//SORTIN   DD DISP=SHR,DSN=&SRC.J21BFDAD
//SORTOUT  DD DSN=&MIG.BFD03.J21BFDAD,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J21ANO
//*----------------------------------------------------------------
//SORT0304 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ21ANO SORT J21-ANO-PRC-C-ID-ETAT-ADR ASC ç371:12Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J21ANO
//SORTIN   DD DISP=SHR,DSN=&SRC.J21ANO
//SORTOUT  DD DSN=&MIG.BFD03.J21ANO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(371,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0302       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STKBFADR DD DSN=&CIB.TKBFADR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFEVDADR DD DSN=&MIG.BFD03.FEVDADR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFSECAD1 DD DSN=&MIG.BFD03.FSECAD1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFANOAD1 DD DSN=&MIG.BFD03.FANOAD1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFFLXADI DD DSN=&MIG.BFD03.FFLXADI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCHRADR DD DSN=&MIG.BFD03.FCHRADR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0303       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRBFD03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRBFD03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRBFD03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFINST   DD DSN=&MIG.BFD03.FINST.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ21RFAN DD DSN=&MIG.BFD03.J21RFAN.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTIBFVAN DD DSN=&MIG.BFD03.TIBFVAN.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFBFDDDR DD DSN=&MIG.BFD03.FBFDDDR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ21BFDE DD DSN=&MIG.BFD03.J21BFDE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ21BFDA DD DSN=&MIG.BFD03.J21BFDA.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ21ANO  DD DSN=&MIG.BFD03.J21ANO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTKBFADR DD DSN=&MIG.BFD03.TKBFADR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFEVDADR DD DSN=&MIG.BFD03.FEVDADR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFSECAD1 DD DSN=&MIG.BFD03.FSECAD1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFANOAD1 DD DSN=&MIG.BFD03.FANOAD1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFFLXADI DD DSN=&MIG.BFD03.FFLXADI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCHRADR DD DSN=&MIG.BFD03.FCHRADR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRBFD03
//*----------------------------------------------------------------
//PPRBFD03 EXEC PGM=PPRBFD03,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRBFD03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRBFD03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRBFD03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//FINST    DD DISP=SHR,DSN=&SRC.L01INSTI
//J21RFAN  DD DISP=SHR,DSN=&SRC.J21RFANO
//TIBFVAN  DD DISP=SHR,DSN=&TAB.TIBFVAN
//SFBFDDDR DD DISP=SHR,DSN=&MIG.BFD03.FBFDDDR
//SJ21BFDE DD DISP=SHR,DSN=&MIG.BFD03.J21BFD
//SJ21BFDA DD DISP=SHR,DSN=&MIG.BFD03.J21BFDAD
//SJ21ANO  DD DISP=SHR,DSN=&MIG.BFD03.J21ANO
//*--------------<FICHIERS CIBLES>---------------------------------
//STKBFADR DD DSN=&CIB.TKBFADR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFEVDADR DD DSN=&MIG.BFD03.FEVDADR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFSECAD1 DD DSN=&MIG.BFD03.FSECAD1,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFANOAD1 DD DSN=&MIG.BFD03.FANOAD1,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFFLXADI DD DSN=&MIG.BFD03.FFLXADI,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFCHRADR DD DSN=&MIG.BFD03.FCHRADR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFINST   DD DSN=&MIG.BFD03.FINST.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZJ21RFAN DD DSN=&MIG.BFD03.J21RFAN.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTIBFVAN DD DSN=&MIG.BFD03.TIBFVAN.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFBFDDDR DD DSN=&MIG.BFD03.FBFDDDR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZJ21BFDE DD DSN=&MIG.BFD03.J21BFDE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZJ21BFDA DD DSN=&MIG.BFD03.J21BFDA.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZJ21ANO  DD DSN=&MIG.BFD03.J21ANO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTKBFADR DD DSN=&MIG.BFD03.TKBFADR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFEVDADR DD DSN=&MIG.BFD03.FEVDADR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFSECAD1 DD DSN=&MIG.BFD03.FSECAD1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFANOAD1 DD DSN=&MIG.BFD03.FANOAD1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFFLXADI DD DSN=&MIG.BFD03.FFLXADI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCHRADR DD DSN=&MIG.BFD03.FCHRADR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRBFD04
//*----------------------------------------------------------------
//DEL0401       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFFLXADI DD DSN=&MIG.BFD04.FFLXADI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FFLXADI
//*----------------------------------------------------------------
//SORT0401 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFFLXADI SORT FFLXADRI-C-ID-PERS ASC ç1:12Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FFLXADI
//SORTIN   DD DISP=SHR,DSN=&MIG.BFD03.FFLXADI
//SORTOUT  DD DSN=&MIG.BFD04.FFLXADI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0402       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFFLXADR DD DSN=&MIG.BFD04.FFLXADR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0403       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRBFD04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRBFD04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRBFD04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFFLXADI DD DSN=&MIG.BFD04.FFLXADI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFFLXADR DD DSN=&MIG.BFD04.FFLXADR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRBFD04
//*----------------------------------------------------------------
//PPRBFD04 EXEC PGM=PPRBFD04,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRBFD04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRBFD04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRBFD04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFFLXADI DD DISP=SHR,DSN=&MIG.BFD04.FFLXADI
//*--------------<FICHIERS CIBLES>---------------------------------
//SFFLXADR DD DSN=&MIG.BFD04.FFLXADR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFFLXADI DD DSN=&MIG.BFD04.FFLXADI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFFLXADR DD DSN=&MIG.BFD04.FFLXADR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRBFD05
//*----------------------------------------------------------------
//DEL0501       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFBFDDOS DD DSN=&MIG.BFD05.FBFDDOS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ21DDR  DD DSN=&MIG.BFD05.J21DDR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ21DOS  DD DSN=&MIG.BFD05.J21DOSSI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FBFDDOS
//*----------------------------------------------------------------
//SORT0501 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFBFDDOS SORT FBFDDOS-C-ID-DDR-DNF ASC ç13:12Ù
//* SFBFDDOS SORT FBFDDOS-C-ID-DOS-PRC ASC ç25:12Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FBFDDOS
//SORTIN   DD DISP=SHR,DSN=&MIG.BFD02.FBFDDOS
//SORTOUT  DD DSN=&MIG.BFD05.FBFDDOS,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(13,12,CH,A,
               25,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J21DDR
//*----------------------------------------------------------------
//SORT0502 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ21DDR SORT J21-DDR-DNF-C-ID-DDR-DNF ASC ç2:12Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J21DDR
//SORTIN   DD DISP=SHR,DSN=&SRC.J21DDR
//SORTOUT  DD DSN=&MIG.BFD05.J21DDR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J21DOS
//*----------------------------------------------------------------
//SORT0503 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ21DOS SORT J21-DOSSIER-PRC-C-ID-DDR-DNF ASC [16:12ó
//* SJ21DOS SORT J21-DOSSIER-PRC-C-ID-DOS-PRC ASC [2:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J21DOSSI
//SORTIN   DD DISP=SHR,DSN=&SRC.J21DOSSI
//SORTOUT  DD DSN=&MIG.BFD05.J21DOSSI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,12,CH,A,
               2,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0502       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFEBMBF  DD DSN=&MIG.BFD05.FEBMBF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFEVDMBF DD DSN=&MIG.BFD05.FEVDMBF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFSECMB1 DD DSN=&MIG.BFD05.FSECMB1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFFLXMBI DD DSN=&MIG.BFD05.FFLXMBI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFBFD01  DD DSN=&MIG.BFD05.FBFD01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0503       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRBFD05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRBFD05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRBFD05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCHRADR DD DSN=&MIG.BFD05.FCHRADR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFINST   DD DSN=&MIG.BFD05.FINST.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFBFDDOS DD DSN=&MIG.BFD05.FBFDDOS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ21DDR  DD DSN=&MIG.BFD05.J21DDR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ21DOS  DD DSN=&MIG.BFD05.J21DOS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFEBMBF  DD DSN=&MIG.BFD05.FEBMBF.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFEVDMBF DD DSN=&MIG.BFD05.FEVDMBF.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFSECMB1 DD DSN=&MIG.BFD05.FSECMB1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFFLXMBI DD DSN=&MIG.BFD05.FFLXMBI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFBFD01  DD DSN=&MIG.BFD05.FBFD01.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRBFD05
//*----------------------------------------------------------------
//PPRBFD05 EXEC PGM=PPRBFD05,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRBFD05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRBFD05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRBFD05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//FCHRADR  DD DISP=SHR,DSN=&MIG.BFD03.FCHRADR
//FINST    DD DISP=SHR,DSN=&SRC.L01INSTI
//SFBFDDOS DD DISP=SHR,DSN=&MIG.BFD05.FBFDDOS
//SJ21DDR  DD DISP=SHR,DSN=&MIG.BFD05.J21DDR
//SJ21DOS  DD DISP=SHR,DSN=&MIG.BFD05.J21DOSSI
//*--------------<FICHIERS CIBLES>---------------------------------
//SFEBMBF  DD DSN=&MIG.BFD05.FEBMBF,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFEVDMBF DD DSN=&MIG.BFD05.FEVDMBF,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFSECMB1 DD DSN=&MIG.BFD05.FSECMB1,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFFLXMBI DD DSN=&MIG.BFD05.FFLXMBI,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFBFD01  DD DSN=&MIG.BFD05.FBFD01,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCHRADR DD DSN=&MIG.BFD05.FCHRADR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFINST   DD DSN=&MIG.BFD05.FINST.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFBFDDOS DD DSN=&MIG.BFD05.FBFDDOS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZJ21DDR  DD DSN=&MIG.BFD05.J21DDR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZJ21DOS  DD DSN=&MIG.BFD05.J21DOS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFEBMBF  DD DSN=&MIG.BFD05.FEBMBF.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFEVDMBF DD DSN=&MIG.BFD05.FEVDMBF.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFSECMB1 DD DSN=&MIG.BFD05.FSECMB1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFFLXMBI DD DSN=&MIG.BFD05.FFLXMBI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFBFD01  DD DSN=&MIG.BFD05.FBFD01.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRBFD06
//*----------------------------------------------------------------
//DEL0601       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFFLXMBI DD DSN=&MIG.BFD06.FFLXMBI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FFLXMBI
//*----------------------------------------------------------------
//SORT0601 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFFLXMBI SORT FFLXMBFI-C-ID-PERS ASC ç1:12Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FFLXMBI
//SORTIN   DD DISP=SHR,DSN=&MIG.BFD05.FFLXMBI
//SORTOUT  DD DSN=&MIG.BFD06.FFLXMBI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0602       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFFLXMBF DD DSN=&MIG.BFD06.FFLXMBF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0603       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRBFD06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRBFD06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRBFD06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFFLXMBI DD DSN=&MIG.BFD06.FFLXMBI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFFLXMBF DD DSN=&MIG.BFD06.FFLXMBF.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRBFD06
//*----------------------------------------------------------------
//PPRBFD06 EXEC PGM=PPRBFD06,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRBFD06,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRBFD06,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRBFD06,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFFLXMBI DD DISP=SHR,DSN=&MIG.BFD06.FFLXMBI
//*--------------<FICHIERS CIBLES>---------------------------------
//SFFLXMBF DD DSN=&MIG.BFD06.FFLXMBF,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFFLXMBI DD DSN=&MIG.BFD06.FFLXMBI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFFLXMBF DD DSN=&MIG.BFD06.FFLXMBF.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRBFD07
//*----------------------------------------------------------------
//DEL0701       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFBFD01  DD DSN=&MIG.BFD07.FBFD01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ21ENT  DD DSN=&MIG.BFD07.J21ENT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ21PCO  DD DSN=&MIG.BFD07.J21PCO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ21PDR  DD DSN=&MIG.BFD07.J21PDR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ21TRA  DD DSN=&MIG.BFD07.J21TRA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ21PTS  DD DSN=&MIG.BFD07.J21PTS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ21ANO  DD DSN=&MIG.BFD07.J21ANO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFANOAD1 DD DSN=&MIG.BFD07.FANOAD1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FBFD01
//*----------------------------------------------------------------
//SORT0701 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFBFD01 SORT FBFD01-C-ID-DOS-PRC ASC ç1:12Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FBFD01
//SORTIN   DD DISP=SHR,DSN=&MIG.BFD05.FBFD01
//SORTOUT  DD DSN=&MIG.BFD07.FBFD01,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J21ENT
//*----------------------------------------------------------------
//SORT0702 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFANOAD1 SORT FANOAD1-C-ID-DOS-PRC ASC ç189:12Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J21ENT
//SORTIN   DD DISP=SHR,DSN=&SRC.J21ENT
//SORTOUT  DD DSN=&MIG.BFD07.J21ENT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
  *MEMSIZE=1000000000
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J21PCO
//*----------------------------------------------------------------
//SORT0703 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ21PCO SORT J21-PCO-C-ID-DOS-PRC ASC [2:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J21PCO
//SORTIN   DD DISP=SHR,DSN=&SRC.J21PCO
//SORTOUT  DD DSN=&MIG.BFD07.J21PCO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J21PDR
//*----------------------------------------------------------------
//SORT0704 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ21PDR SORT J21-PDR-C-ID-DOS-PRC ASC [2:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J21PDR
//SORTIN   DD DISP=SHR,DSN=&SRC.J21PDR
//SORTOUT  DD DSN=&MIG.BFD07.J21PDR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J21TRA
//*----------------------------------------------------------------
//SORT0705 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ21TRA SORT J21-TRA-C-ID-DOS-PRC ASC [2:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J21TRA
//SORTIN   DD DISP=SHR,DSN=&SRC.J21TRA
//SORTOUT  DD DSN=&MIG.BFD07.J21TRA,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J21PTS
//*----------------------------------------------------------------
//SORT0706 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ21PTS SORT J21-PTS-C-ID-DOS-PRC ASC [2:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J21PTS
//SORTIN   DD DISP=SHR,DSN=&SRC.J21PTS
//SORTOUT  DD DSN=&MIG.BFD07.J21PTS,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,UNIT=(SYSDA,20),
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J21ANO
//*----------------------------------------------------------------
//SORT0707 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ21ANO SORT J21-ANO-PRC-C-ID-DOS-PRC ASC [37:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J21ANO
//SORTIN   DD DISP=SHR,DSN=&SRC.J21ANO
//SORTOUT  DD DSN=&MIG.BFD07.J21ANO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(37,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FANOAD1
//*----------------------------------------------------------------
//SORT0708 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFANOAD1 SORT FANOAD1-C-ID-DOS-PRC ASC [189:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FANOAD1
//SORTIN   DD DISP=SHR,DSN=&MIG.BFD03.FANOAD1
//SORTOUT  DD DSN=&MIG.BFD07.FANOAD1,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(189,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0702       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STKBFENT DD DSN=&CIB.TKBFENT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKBFPCO DD DSN=&CIB.TKBFPCO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKBFPDR DD DSN=&CIB.TKBFPDR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKBFTRA DD DSN=&CIB.TKBFTRA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKBFPTS DD DSN=&CIB.TKBFPTS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFANOMBF DD DSN=&MIG.BFD07.FANOMBF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFBFD02  DD DSN=&MIG.BFD07.FBFD02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFANOADR DD DSN=&MIG.BFD07.FANOADR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0703       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRBFD07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRBFD07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRBFD07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ21RFAN DD DSN=&MIG.BFD07.J21RFAN.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTIBFVAN DD DSN=&MIG.BFD07.TIBFVAN.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFBFD01  DD DSN=&MIG.BFD07.FBFD01.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ21ENT  DD DSN=&MIG.BFD07.J21ENT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ21PCO  DD DSN=&MIG.BFD07.J21PCO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ21PDR  DD DSN=&MIG.BFD07.J21PDR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ21TRA  DD DSN=&MIG.BFD07.J21TRA.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ21PTS  DD DSN=&MIG.BFD07.J21PTS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ21ANO  DD DSN=&MIG.BFD07.J21ANO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFANOAD1 DD DSN=&MIG.BFD07.FANOAD1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTKBFENT DD DSN=&MIG.BFD07.TKBFENT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTKBFPCO DD DSN=&MIG.BFD07.TKBFPCO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTKBFPDR DD DSN=&MIG.BFD07.TKBFPDR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTKBFTRA DD DSN=&MIG.BFD07.TKBFTRA.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTKBFPTS DD DSN=&MIG.BFD07.TKBFPTS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFANOMBF DD DSN=&MIG.BFD07.FANOMBF.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFBFD02  DD DSN=&MIG.BFD07.FBFD02.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFANOADR DD DSN=&MIG.BFD07.FANOADR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRBFD07
//*----------------------------------------------------------------
//PPRBFD07 EXEC PGM=PPRBFD07,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRBFD07,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRBFD07,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRBFD07,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//J21RFAN  DD DISP=SHR,DSN=&SRC.J21RFANO
//TIBFVAN  DD DISP=SHR,DSN=&TAB.TIBFVAN
//SFBFD01  DD DISP=SHR,DSN=&MIG.BFD07.FBFD01
//SJ21ENT  DD DISP=SHR,DSN=&MIG.BFD07.J21ENT
//SJ21PCO  DD DISP=SHR,DSN=&MIG.BFD07.J21PCO
//SJ21PDR  DD DISP=SHR,DSN=&MIG.BFD07.J21PDR
//SJ21TRA  DD DISP=SHR,DSN=&MIG.BFD07.J21TRA
//SJ21PTS  DD DISP=SHR,DSN=&MIG.BFD07.J21PTS
//SJ21ANO  DD DISP=SHR,DSN=&MIG.BFD07.J21ANO
//SFANOAD1 DD DISP=SHR,DSN=&MIG.BFD07.FANOAD1
//*--------------<FICHIERS CIBLES>---------------------------------
//STKBFENT DD DSN=&CIB.TKBFENT,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STKBFPCO DD DSN=&CIB.TKBFPCO,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STKBFPDR DD DSN=&CIB.TKBFPDR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STKBFTRA DD DSN=&CIB.TKBFTRA,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STKBFPTS DD DSN=&CIB.TKBFPTS,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFANOMBF DD DSN=&MIG.BFD07.FANOMBF,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFBFD02  DD DSN=&MIG.BFD07.FBFD02,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFANOADR DD DSN=&MIG.BFD07.FANOADR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZJ21RFAN DD DSN=&MIG.BFD07.J21RFAN.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTIBFVAN DD DSN=&MIG.BFD07.TIBFVAN.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFBFD01  DD DSN=&MIG.BFD07.FBFD01.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZJ21ENT  DD DSN=&MIG.BFD07.J21ENT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZJ21PCO  DD DSN=&MIG.BFD07.J21PCO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZJ21PDR  DD DSN=&MIG.BFD07.J21PDR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZJ21TRA  DD DSN=&MIG.BFD07.J21TRA.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZJ21PTS  DD DSN=&MIG.BFD07.J21PTS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZJ21ANO  DD DSN=&MIG.BFD07.J21ANO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFANOAD1 DD DSN=&MIG.BFD07.FANOAD1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTKBFENT DD DSN=&MIG.BFD07.TKBFENT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTKBFPCO DD DSN=&MIG.BFD07.TKBFPCO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTKBFPDR DD DSN=&MIG.BFD07.TKBFPDR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTKBFTRA DD DSN=&MIG.BFD07.TKBFTRA.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTKBFPTS DD DSN=&MIG.BFD07.TKBFPTS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFANOMBF DD DSN=&MIG.BFD07.FANOMBF.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFBFD02  DD DSN=&MIG.BFD07.FBFD02.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFANOADR DD DSN=&MIG.BFD07.FANOADR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRBFD08
//*----------------------------------------------------------------
//DEL0801       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFBFD02  DD DSN=&MIG.BFD08.FBFD02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFEBMBF  DD DSN=&MIG.BFD08.FEBMBF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FBFD02
//*----------------------------------------------------------------
//SORT0801 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFBFD02 SORT FBFD02-NO-CLE-FLX ASC ç1:9Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FBFD02
//SORTIN   DD DISP=SHR,DSN=&MIG.BFD07.FBFD02
//SORTOUT  DD DSN=&MIG.BFD08.FBFD02,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,9,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FEBMBF
//*----------------------------------------------------------------
//SORT0802 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFEBMBF SORT FEBMBF-NO-CLE-FLX ASC ç1:9Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FEBMBF
//SORTIN   DD DISP=SHR,DSN=&MIG.BFD05.FEBMBF
//SORTOUT  DD DSN=&MIG.BFD08.FEBMBF,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,9,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0802       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STKBFDEB DD DSN=&CIB.TKBFDEB,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0803       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRBFD08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRBFD08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRBFD08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFBFD02  DD DSN=&MIG.BFD08.FBFD02.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFEBMBF  DD DSN=&MIG.BFD08.FEBMBF.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTKBFDEB DD DSN=&MIG.BFD08.TKBFDEB.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRBFD08
//*----------------------------------------------------------------
//PPRBFD08 EXEC PGM=PPRBFD08,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRBFD08,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRBFD08,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRBFD08,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFBFD02  DD DISP=SHR,DSN=&MIG.BFD08.FBFD02
//SFEBMBF  DD DISP=SHR,DSN=&MIG.BFD08.FEBMBF
//*--------------<FICHIERS CIBLES>---------------------------------
//STKBFDEB DD DSN=&CIB.TKBFDEB,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFBFD02  DD DSN=&MIG.BFD08.FBFD02.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFEBMBF  DD DSN=&MIG.BFD08.FEBMBF.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTKBFDEB DD DSN=&MIG.BFD08.TKBFDEB.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS CONCATENES
//*----------------------------------------------------------------
//DEL0901       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STKBFEVD    DD DSN=&CIB.TKBFEVD,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKBFFLX    DD DSN=&CIB.TKBFFLX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFBFDSEC    DD DSN=&MIG.BFD09.FBFDSEC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKBFANO    DD DSN=&CIB.TKBFANO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*CONCATENATION DES 2 FICHIERS FEVDADR ET FEVDMBF
//*----------------------------------------------------------------
//SORT0901 EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TKBFEVD
//SORTIN01 DD DISP=SHR,DSN=&MIG.BFD03.FEVDADR
//SORTIN02 DD DISP=SHR,DSN=&MIG.BFD05.FEVDMBF
//SORTOUT  DD DSN=&CIB.TKBFEVD,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=COPY
/*
//*----------------------------------------------------------------
//*CONCATENATION DES 2 FICHIERS FFLXADR ET FFLXMBF
//*----------------------------------------------------------------
//SORT0902 EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TKBFFLX
//SORTIN01 DD DISP=SHR,DSN=&MIG.BFD04.FFLXADR
//SORTIN02 DD DISP=SHR,DSN=&MIG.BFD06.FFLXMBF
//SORTOUT  DD DSN=&CIB.TKBFFLX,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=COPY
/*
//*
//*----------------------------------------------------------------
//*CONCATENATION DES 2 FICHIERS FSECAD1 ET FSECMB1
//*----------------------------------------------------------------
//SORT0903 EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FSECAD1
//SORTIN01 DD DISP=SHR,DSN=&MIG.BFD03.FSECAD1
//SORTIN02 DD DISP=SHR,DSN=&MIG.BFD05.FSECMB1
//SORTOUT  DD DSN=&MIG.BFD09.FBFDSEC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=COPY
/*
//*----------------------------------------------------------------
//*CONCATENATION DES 2 FICHIERS FANOADR ET FANOMBF
//*----------------------------------------------------------------
//SORT0904 EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FANOADR
//SORTIN01 DD DISP=SHR,DSN=&MIG.BFD07.FANOADR
//SORTIN02 DD DISP=SHR,DSN=&MIG.BFD07.FANOMBF
//SORTOUT  DD DSN=&CIB.TKBFANO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=COPY
/*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRBFD10
//*----------------------------------------------------------------
//DEL1001       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFBFDSEC DD DSN=&MIG.BFD10.FBFDSEC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FBFDSEC
//*----------------------------------------------------------------
//SORT1001 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFBFDSEC SORT FBFDSEC-NO-CLE-CPTE-PTCP ASC ç10:20Ù
//* SFBFDSEC SORT FBFDSEC-CO-TYPE-PROC ASC ç153:3Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FBFDSEC
//SORTIN   DD DISP=SHR,DSN=&MIG.BFD09.FBFDSEC
//SORTOUT  DD DSN=&MIG.BFD10.FBFDSEC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(10,20,CH,A,
               153,3,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STKBFSEC DD DSN=&CIB.TKBFSEC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRBFD10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRBFD10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRBFD10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFBFDSEC DD DSN=&MIG.BFD10.FBFDSEC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTKBFSEC DD DSN=&MIG.BFD10.TKBFSEC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRBFD10
//*----------------------------------------------------------------
//PPRBFD10 EXEC PGM=PPRBFD10,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRBFD10,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRBFD10,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRBFD10,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFBFDSEC DD DISP=SHR,DSN=&MIG.BFD10.FBFDSEC
//*--------------<FICHIERS CIBLES>---------------------------------
//STKBFSEC DD DSN=&CIB.TKBFSEC,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFBFDSEC DD DSN=&MIG.BFD10.FBFDSEC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTKBFSEC DD DSN=&MIG.BFD10.TKBFSEC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*PPRBFD50 NON EXECUTE CAR LE KM N UTILISE PAS LE PIVOT ENCAPSULE
