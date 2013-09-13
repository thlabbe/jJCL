//J9PRCR1  JOB UTI00TX0,'PR GRECCO G02-CRE',CLASS=Z,MSGCLASS=X,         JOB00836
//*         RESTART=DEL2102,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=PLURM.MOVE.VPREF.JCL.PROD
//MEMBER   INCLUDE MEMBER=$$PRRT
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCRE01
//*----------------------------------------------------------------
//DEL0101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STADHADH DD DSN=&MIG.CRE01.TADHADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB05SYNT DD DSN=&MIG.CRE01.B05SYNT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ04DAS  DD DSN=&MIG.CRE01.J04DAS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB05PERI DD DSN=&MIG.CRE01.B05PERI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB05OPER DD DSN=&MIG.CRE01.B05OPER,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SURCOMTI DD DSN=&MIG.CRE01.URCOMTI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SECRIT1  DD DSN=&MIG.CRE01.B05ECRI1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TADHADH
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STADHADH SORT ADHSADH-C-GRPT-GEST ASC [94:1—
//* STADHADH SORT ADHSADH-C-ADH ASC [95:9—
//* STADHADH SORT ADHSADH-C-ORDRE ASC [104:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TADHADH
//SORTIN   DD DISP=SHR,DSN=&CIB.TADHADH
//SORTOUT  DD DSN=&MIG.CRE01.TADHADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(94,1,CH,A,
               95,9,CH,A,
               104,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B05SYNT
//*----------------------------------------------------------------
//SORT0102 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05SYNT SORT B05-SYNTHESE-CPTE-C-GRPT-GEST ASC [1:1—
//* SB05SYNT SORT B05-SYNTHESE-CPTE-C-ADH ASC [3:9—
//* SB05SYNT SORT B05-SYNTHESE-CPTE-C-ORDRE ASC [13:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.B05SYNTH
//SORTIN   DD DISP=SHR,DSN=&SRC.B05SYNTH
//SORTOUT  DD DSN=&MIG.CRE01.B05SYNT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,1,CH,A,
               3,9,CH,A,
               13,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J04DAS
//*----------------------------------------------------------------
//SORT0103 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ04DAS SORT J04-DAS-C-GRPT-GEST ASC [20:1—
//* SJ04DAS SORT J04-DAS-C-ADH ASC [22:9—
//* SJ04DAS SORT J04-DAS-C-ORDRE ASC [32:4—
//* SJ04DAS SORT J04-DAS-Q-EXE ASC [483:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J04DAS
//SORTIN   DD DISP=SHR,DSN=&SRC.J04DAS
//SORTOUT  DD DSN=&MIG.CRE01.J04DAS,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(20,1,CH,A,
               22,9,CH,A,
               32,4,CH,A,
               483,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B05PERI
//*----------------------------------------------------------------
//SORT0104 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05PERI SORT B05-PER-DEB-C-GRPT-GEST ASC [50:1—
//* SB05PERI SORT B05-PER-DEB-C-ADH ASC [52:9—
//* SB05PERI SORT B05-PER-DEB-C-ORDRE ASC [62:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.B05PERIO
//SORTIN   DD DISP=SHR,DSN=&SRC.B05PERIO
//SORTOUT  DD DSN=&MIG.CRE01.B05PERI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(50,1,CH,A,
               52,9,CH,A,
               62,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B05OPER
//*----------------------------------------------------------------
//SORT0105 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05OPER SORT B05-OPER-COMPTE-C-GRPT-GEST ASC [41:1—
//* SB05OPER SORT B05-OPER-COMPTE-C-ADH ASC [43:9—
//* SB05OPER SORT B05-OPER-COMPTE-C-ORDRE ASC [53:4—
//* SB05OPER SORT B05-OPER-COMPTE-C-EXE-AFFECT ASC [87:4—
//* SB05OPER SORT B05-OPER-COMPTE-C-TYPE-PER ASC [94:1—
//* SB05OPER SORT B05-OPER-COMPTE-C-PER-AFFECT ASC [92:2—
//* SB05OPER SORT B05-OPER-COMPTE-C-ORDRE-CTX ASC [96:5—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.B05OPER
//SORTIN   DD DISP=SHR,DSN=&SRC.B05OPER
//SORTOUT  DD DSN=&MIG.CRE01.B05OPER,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(41,1,CH,A,
               43,9,CH,A,
               53,4,CH,A,
               87,4,CH,A,
               94,1,CH,A,
               92,2,CH,A,
               96,5,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER URCOMTI
//*----------------------------------------------------------------
//SORT0106 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SURCOMTI SORT URCOMTIE-C-GRPT-GEST ASC [49:1—
//* SURCOMTI SORT URCOMTIE-C-ADH ASC [50:9—
//* SURCOMTI SORT URCOMTIE-C-ORDRE ASC [59:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.URCOMTIE
//SORTIN   DD DISP=SHR,DSN=&TAB.URCOMTI
//SORTOUT  DD DSN=&MIG.CRE01.URCOMTI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,1,CH,A,
               50,9,CH,A,
               59,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI1 DU FICHIER B05ECRI
//*----------------------------------------------------------------
//SORT0107 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05ECRI SORT B05-ECRITURE-COMPTE-W2-PT-GEST ASC [49:1—
//* SB05ECRI SORT B05-ECRITURE-COMPTE-C-ADH ASC [51:9—
//* SB05ECRI SORT B05-ECRITURE-COMPTE-C-ORDRE ASC [61:4—
//* SB05ECRI SORT B05-ECRITURE-COMPTE-D-CREATION ASC [129:8—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.B05ECRIT
//SORTIN   DD DISP=SHR,DSN=&SRC.B05ECRIT
//SORTOUT  DD DSN=&MIG.CRE01.B05ECRI1,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,UNIT=(SYSDA,20),
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,1,CH,A,
               51,9,CH,A,
               61,4,CH,A,
               129,8,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE01D DD DSN=&MIG.CRE01.FCRE01D,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCRE01C DD DSN=&MIG.CRE01.FCRE01C,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPER01  DD DSN=&MIG.CRE01.FPER01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFICPROF DD DSN=&MIG.CRE01.FICPROF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFEXCLAD DD DSN=&CIB.FEXCL.ADHER,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCRE01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCRE01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCRE01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCRE01
//*----------------------------------------------------------------
//PPRCRE01 EXEC PGM=PPRCRE01,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCRE01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCRE01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCRE01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//ADHSEL   DD DUMMY
//*ADH=H0181204540000
//STADHADH DD DISP=SHR,DSN=&MIG.CRE01.TADHADH
//SB05ECRI DD DISP=SHR,DSN=&MIG.CRE01.B05ECRI1
//SB05SYNT DD DISP=SHR,DSN=&MIG.CRE01.B05SYNT
//SJ04DAS  DD DISP=SHR,DSN=&MIG.CRE01.J04DAS
//SB05PERI DD DISP=SHR,DSN=&MIG.CRE01.B05PERI
//SB05OPER DD DISP=SHR,DSN=&MIG.CRE01.B05OPER
//SURCOMTI DD DISP=SHR,DSN=&MIG.CRE01.URCOMTI
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE01D DD DSN=&MIG.CRE01.FCRE01D,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(500,500),RLSE),UNIT=(SYSDA,20)
//SFCRE01C DD DSN=&MIG.CRE01.FCRE01C,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(500,500),RLSE),UNIT=(SYSDA,20)
//SFPER01  DD DSN=&MIG.CRE01.FPER01,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFEXCLAD DD DSN=&CIB.FEXCL.ADHER,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFICPROF DD DSN=&MIG.CRE01.FICPROF,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCRE60
//*----------------------------------------------------------------
//DEL6001       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE01  DD DSN=&MIG.CRE60.FCRE01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB05OPEV DD DSN=&MIG.CRE60.B05OPEV,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE01
//*----------------------------------------------------------------
//SORT6001 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE01 SORT FCRE01-ECRITURE-COMPTE-ENR(24:12) ASC [77:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE01
//SORTIN01 DD DISP=SHR,DSN=&MIG.CRE01.FCRE01D
//SORTIN02 DD DISP=SHR,DSN=&MIG.CRE01.FCRE01C
//SORTOUT  DD DSN=&MIG.CRE60.FCRE01,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE),
//SYSIN    DD *
  SORT FIELDS=(77,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B05OPEV
//*----------------------------------------------------------------
//SORT6002 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05OPEV SORT B05-OPER-VENTIL-C-ID-OPER ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.B05OPERV
//SORTIN   DD DISP=SHR,DSN=&SRC.B05OPERV
//SORTOUT  DD DSN=&MIG.CRE60.B05OPEV,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL6002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE01B DD DSN=&MIG.CRE60.FCRE01B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL6003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCRE60,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCRE60,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCRE60,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCRE60
//*----------------------------------------------------------------
//PPRCRE60 EXEC PGM=PPRCRE60,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCRE60,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCRE60,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCRE60,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRE01  DD DISP=SHR,DSN=&MIG.CRE60.FCRE01
//SB05OPEV DD DISP=SHR,DSN=&MIG.CRE60.B05OPEV
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE01B DD DSN=&MIG.CRE60.FCRE01B,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(5000,500),RLSE),UNIT=(SYSDA,20)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCRE61
//*----------------------------------------------------------------
//DEL6101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE01B DD DSN=&MIG.CRE61.FCRE01B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB05OPER DD DSN=&MIG.CRE61.B05OPER,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB05OPE2 DD DSN=&MIG.CRE61.B05OPE2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE01B
//*----------------------------------------------------------------
//SORT6101 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE01B SORT B-FCRE01-C-OPER ASC [290:12—
//* FIN   CRITERE XGEN

//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE01
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE60.FCRE01B
//SORTOUT  DD DSN=&MIG.CRE61.FCRE01B,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(290,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B05OPER
//*----------------------------------------------------------------
//SORT6102 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05OPER SORT B05-OPER-COMPTE-C-OPER ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.B05OPER
//SORTIN   DD DISP=SHR,DSN=&SRC.B05OPER
//SORTOUT  DD DSN=&MIG.CRE61.B05OPER,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B05OPE2
//*----------------------------------------------------------------
//SORT6103 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05OPE2 SORT BB05-OPER-COMPTE-C-OPER-ISSU ASC [16:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.B05OPER
//SORTIN   DD DISP=SHR,DSN=&SRC.B05OPER
//SORTOUT  DD DSN=&MIG.CRE61.B05OPE2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL6102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE01  DD DSN=&MIG.CRE61.FCRE01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCRE01  DD DSN=&MIG.CRE61.FCRE01C,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL6103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCRE61,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCRE61,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCRE61,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCRE61
//*----------------------------------------------------------------
//PPRCRE61 EXEC PGM=PPRCRE61,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCRE61,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCRE61,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCRE61,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRE01B DD DISP=SHR,DSN=&MIG.CRE61.FCRE01B
//SB05OPER DD DISP=SHR,DSN=&MIG.CRE61.B05OPER
//SB05OPE2 DD DISP=SHR,DSN=&MIG.CRE61.B05OPE2
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE01  DD DSN=&MIG.CRE61.FCRE01,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(5000,5000),RLSE),UNIT=(SYSDA,20)
//SFCRE01C DD DSN=&MIG.CRE61.FCRE01C,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(5000,5000),RLSE),UNIT=(SYSDA,20)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCRE59
//*----------------------------------------------------------------
//DEL5901       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SB05PER1 DD DSN=&MIG.CRE59.B05PER1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB05PER2 DD DSN=&MIG.CRE59.B05PER2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB07RECO DD DSN=&MIG.CRE59.B07RECO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER B05PER1
//*----------------------------------------------------------------
//SORT5901 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05PERI SORT F1-B05-PER-DEB-C-ID-REC-INI ASC [284:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.B05PERIO
//SORTIN   DD DISP=SHR,DSN=&SRC.B05PERIO
//SORTOUT  DD DSN=&MIG.CRE59.B05PER1,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(284,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B05PER2
//*----------------------------------------------------------------
//SORT5902 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05PERI SORT F2-B05-PER-DEB-C-ID-RECOUVR ASC [17:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.B05PERIO
//SORTIN   DD DISP=SHR,DSN=&SRC.B05PERIO
//SORTOUT  DD DSN=&MIG.CRE59.B05PER2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(17,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B07RECO
//*----------------------------------------------------------------
//SORT5903 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB07RECO SORT B07-RECOUVR-C-ID-RECOUVR ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.B07RECOU
//SORTIN   DD DISP=SHR,DSN=&SRC.B07RECOU
//SORTOUT  DD DSN=&MIG.CRE59.B07RECO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL5902       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFICRECO DD DSN=&MIG.CRE59.FICRECO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL5903       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCRE59,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCRE59,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCRE59,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCRE59
//*----------------------------------------------------------------
//PPRCRE59 EXEC PGM=PPRCRE59,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCRE59,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCRE59,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCRE59,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SB05PER1 DD DISP=SHR,DSN=&MIG.CRE59.B05PER1
//SB05PER2 DD DISP=SHR,DSN=&MIG.CRE59.B05PER2
//SB07RECO DD DISP=SHR,DSN=&MIG.CRE59.B07RECO
//*--------------<FICHIERS CIBLES>---------------------------------
//SFICRECO DD DSN=&MIG.CRE59.FICRECO,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCRE02
//*----------------------------------------------------------------
//DEL0201       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SB07RECO DD DSN=&MIG.CRE02.B07RECO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ04DAS1 DD DSN=&MIG.CRE02.J04DAS1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ04DAS2 DD DSN=&MIG.CRE02.J04DAS2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER B07RECO
//*----------------------------------------------------------------
//SORT0201 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB07RECO SORT FICRECO-C-ID-REC-ACCES ASC [13:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.CPY0458
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE59.FICRECO
//SORTOUT  DD DSN=&MIG.CRE02.B07RECO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(6000,500),RLSE),UNIT=(SYSDA,20)
//SYSIN    DD *
  SORT FIELDS=(13,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J04DAS1
//*----------------------------------------------------------------
//SORT0203 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ04DAS1 SORT J1-J04-DAS-C-ID-REC-SDA ASC [531:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J04DAS
//SORTIN   DD DISP=SHR,DSN=&SRC.J04DAS
//SORTOUT  DD DSN=&MIG.CRE02.J04DAS1,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(531,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J04DAS2
//*----------------------------------------------------------------
//SORT0204 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ04DAS2 SORT J2-J04-DAS-C-ID-REC-SDA50 ASC [547:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J04DAS
//SORTIN   DD DISP=SHR,DSN=&SRC.J04DAS
//SORTOUT  DD DSN=&MIG.CRE02.J04DAS2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(547,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0202       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE02  DD DSN=&MIG.CRE02.FCRE02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0203       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCRE02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCRE02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCRE02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCRE02
//*----------------------------------------------------------------
//PPRCRE02 EXEC PGM=PPRCRE02,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCRE02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCRE02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCRE02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SB07RECO DD DISP=SHR,DSN=&MIG.CRE02.B07RECO
//SJ04DAS1 DD DISP=SHR,DSN=&MIG.CRE02.J04DAS1
//SJ04DAS2 DD DISP=SHR,DSN=&MIG.CRE02.J04DAS2
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE02  DD DSN=&MIG.CRE02.FCRE02,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCRE03
//*----------------------------------------------------------------
//DEL0301       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE01  DD DSN=&MIG.CRE03.FCRE01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCRE02  DD DSN=&MIG.CRE03.FCRE02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCRE02B DD DSN=&MIG.CRE03.FCRE02B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPER01  DD DSN=&MIG.CRE03.FPER01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE01
//*----------------------------------------------------------------
//SORT0301 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE01 SORT FCRE01-C-ID-PER-DEBIT ASC [38:12—
//* FIN   CRITERE XGEN

//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE01
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE61.FCRE01
//SORTOUT  DD DSN=&MIG.CRE03.FCRE01,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE),UNIT=(SYSDA,20)
//SYSIN    DD *
  SORT FIELDS=(38,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE02
//*----------------------------------------------------------------
//SORT0302 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE02 SORT FCRE02-PER-DEB-C-ID-PER-DEBIT ASC [1:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE02
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE02.FCRE02
//SORTOUT  DD DSN=&MIG.CRE03.FCRE02,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE02B
//*----------------------------------------------------------------
//SORT0302 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE02B SORT FCRE02-PER-DEB-C-ID-PER-DEBIT ASC [1:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE02
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE02.FCRE02
//SORTOUT  DD DSN=&MIG.CRE03.FCRE02B,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FPER01
//*----------------------------------------------------------------
//SORT0303 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFPER01 SORT FPER01-C-ID-PER-DEBIT ASC [1:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FPER01
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE01.FPER01
//SORTOUT  DD DSN=&MIG.CRE03.FPER01,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0302       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE03  DD DSN=&MIG.CRE03.FCRE03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCRE03A DD DSN=&MIG.CRE03.FCRE03A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0303       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCRE03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCRE03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCRE03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCRE03
//*----------------------------------------------------------------
//PPRCRE03 EXEC PGM=PPRCRE03,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCRE03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCRE03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCRE03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//TPERIOD  DD DISP=SHR,DSN=&TAB.TPERIOD
//SFCRE01  DD DISP=SHR,DSN=&MIG.CRE03.FCRE01
//SFCRE02  DD DISP=SHR,DSN=&MIG.CRE03.FCRE02
//SFCRE02B DD DISP=SHR,DSN=&MIG.CRE03.FCRE02B
//SFPER01  DD DISP=SHR,DSN=&MIG.CRE03.FPER01
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE03  DD DSN=&MIG.CRE03.FCRE03,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(5000,500),RLSE),UNIT=(SYSDA,20)
//SFCRE03A DD DSN=&MIG.CRE03.FCRE03A,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(5000,500),RLSE),UNIT=(SYSDA,20)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCRE58
//*----------------------------------------------------------------
//DEL5801       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE03W DD DSN=&MIG.CRE58.FCRE03W,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB07RECO DD DSN=&MIG.CRE58.B07RECO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE03W
//*----------------------------------------------------------------
//SORT5801 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE03W SORT FCRE03-C-GRPT-GEST ASC [24:1—
//* SFCRE03W SORT FCRE03-C-ADH ASC [25:9—
//* SFCRE03W SORT FCRE03-C-ORDRE ASC [34:4—
//* SFCRE03W SORT FCRE03-OPERC-EXE-AFFECT ASC [430:4—
//* SFCRE03W SORT FCRE03-OPERC-PER-AFFECT ASC [434:2—
//* SFCRE03W SORT FCRE03-OPERC-TYPE-PER ASC [436:1—
//* SFCRE03W SORT FCRE03-OPERC-ORDRE-CTX ASC [437:5—
//* FIN   CRITERE XGEN

//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE03
//SORTIN01 DD DISP=SHR,DSN=&MIG.CRE03.FCRE03
//SORTIN02 DD DISP=SHR,DSN=&MIG.CRE03.FCRE03A
//SORTOUT  DD DSN=&MIG.CRE58.FCRE03W,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE),UNIT=(SYSDA,20)
//SYSIN    DD *
  SORT FIELDS=(24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A,
               430,4,CH,A,
               434,2,CH,A,
               436,1,CH,A,
               437,5,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B07RECO
//*----------------------------------------------------------------
//SORT5802 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB07RECO SORT B07-RECOUVR-C-GRPT-GEST ASC [15:1—
//* SB07RECO SORT B07-RECOUVR-C-ADH ASC [17:9—
//* SB07RECO SORT B07-RECOUVR-C-ORDRE ASC [27:4—
//* SB07RECO SORT B07-RECOUVR-C-EXE-AFFECT ASC [35:4—
//* SB07RECO SORT B07-RECOUVR-C-PER-AFFECT ASC [40:2—
//* SB07RECO SORT B07-RECOUVR-C-TYPE-PER ASC [42:1—
//* SB07RECO SORT B07-RECOUVR-C-ORDRE-CTX ASC [44:5—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.B07RECOU
//SORTIN   DD DISP=SHR,DSN=&SRC.B07RECOU
//SORTOUT  DD DSN=&MIG.CRE58.B07RECO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(15,1,CH,A,
               17,9,CH,A,
               27,4,CH,A,
               35,4,CH,A,
               40,2,CH,A,
               42,1,CH,A,
               44,5,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL5802       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE58  DD DSN=&MIG.CRE58.FCRE58,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL5803       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCRE58,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCRE58,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCRE58,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCRE58
//*----------------------------------------------------------------
//PPRCRE58 EXEC PGM=PPRCRE58,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCRE58,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCRE58,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCRE58,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRE03W DD DISP=SHR,DSN=&MIG.CRE58.FCRE03W
//SB07RECO DD DISP=SHR,DSN=&MIG.CRE58.B07RECO
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE58  DD DSN=&MIG.CRE58.FCRE58,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(5000,5000),RLSE),UNIT=(SYSDA,20)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCRE04
//*----------------------------------------------------------------
//DEL0401       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE58  DD DSN=&MIG.CRE04.FCRE58,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCRE01C DD DSN=&MIG.CRE04.FCRE01C,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE08
//*----------------------------------------------------------------
//SORT0401 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE08 SORT FCRE03-C-GRPT-GEST ASC [24:1—
//* SFCRE08 SORT FCRE03-C-ADH ASC [25:9—
//* SFCRE08 SORT FCRE03-C-ORDRE ASC [34:4—
//* SFCRE08 SORT FCRE03-ECRITURE-COMPTE-ENR(66:4) ASC [119:4—
//* SFCRE08 SORT FCRE03-ECRITURE-COMPTE-ENR(71:2) ASC [124:2—
//* SFCRE08 SORT FCRE03-ECRITURE-COMPTE-ENR(73:1) ASC [126:1—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE03
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE58.FCRE58
//SORTOUT  DD DSN=&MIG.CRE04.FCRE58,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A,
               119,4,CH,A,
               124,2,CH,A,
               126,1,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE01C
//*----------------------------------------------------------------
//SORT0401 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE01C SORT FCRE01C-C-GRPT-GEST ASC [1:1—
//* SFCRE01C SORT FCRE01C-C-ADH ASC [2:9—
//* SFCRE01C SORT FCRE01C-C-ORDRE ASC [11:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE01C
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE61.FCRE01C
//SORTOUT  DD DSN=&MIG.CRE04.FCRE01C,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,1,CH,A,
               2,9,CH,A,
               11,4,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0402       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE04  DD DSN=&MIG.CRE04.FCRE04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0403       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCRE04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCRE04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCRE04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCRE04
//*----------------------------------------------------------------
//PPRCRE04 EXEC PGM=PPRCRE04,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCRE04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCRE04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCRE04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//TPERIOD  DD DISP=SHR,DSN=&TAB.TPERIOD
//SFCRE58  DD DISP=SHR,DSN=&MIG.CRE04.FCRE58
//SB05OPER DD DISP=SHR,DSN=&MIG.CRE01.B05OPER
//STADHADH DD DISP=SHR,DSN=&MIG.CRE01.TADHADH
//SFCRE01C DD DISP=SHR,DSN=&MIG.CRE04.FCRE01C
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE04  DD DSN=&MIG.CRE04.FCRE04,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(5000,5000),RLSE),UNIT=(SYSDA,20)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCRE36
//*----------------------------------------------------------------
//DEL3601       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STCNTGAR DD DSN=&MIG.CRE36.TCNTGAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STPOPCPI DD DSN=&MIG.CRE36.TPOPCPI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TCNTGAR
//*----------------------------------------------------------------
//SORT3601 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STCNTGAR SORT CNTGAR-ID-TECH-UR-ADHR ASC [21:20—
//* STCNTGAR SORT CNTGAR-NO-ORD-ADHS ASC [41:3—
//* STCNTGAR SORT CNTGAR-NO-ORD-CPRO ASC [95:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TCNTGAR
//SORTIN   DD DISP=SHR,DSN=&CIB.TCNTGAR
//SORTOUT  DD DSN=&MIG.CRE36.TCNTGAR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(21,20,CH,A,
               41,3,CH,A,
               95,3,CH,A,
               206,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TPOPCPI
//*----------------------------------------------------------------
//SORT3602 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STPOPCPI SORT TAB-POP-CPI-ID-TECH-UR-ADHR ASC [21:20—
//* STPOPCPI SORT TAB-POP-CPI-NO-ORD-ADHS ASC [41:3—
//* STPOPCPI SORT TAB-POP-CPI-NO-ORD-CPRO ASC [64:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TPOPCPI
//SORTIN   DD DISP=SHR,DSN=&CIB.TPOPCPI
//SORTOUT  DD DSN=&MIG.CRE36.TPOPCPI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(21,20,CH,A,
               41,3,CH,A,
               64,3,CH,A,
               297,3,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL3602       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SWKGAR   DD DSN=&MIG.CRE36.WKGAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL3603       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCRE36,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCRE36,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCRE36,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCRE36
//*----------------------------------------------------------------
//PPRCRE36 EXEC PGM=PPRCRE36,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCRE36,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCRE36,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCRE36,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//STCNTGAR DD DISP=SHR,DSN=&MIG.CRE36.TCNTGAR
//STPOPCPI DD DISP=SHR,DSN=&MIG.CRE36.TPOPCPI
//*--------------<FICHIERS CIBLES>---------------------------------
//SWKGAR   DD DSN=&MIG.CRE36.WKGAR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCRE05
//*----------------------------------------------------------------
//DEL0501       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//CONTRAT  DD DSN=&MIG.CRE05.CONTRAT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCRE58  DD DSN=&MIG.CRE05.FCRE04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STCNTGAR DD DSN=&MIG.CRE05.TCNTGAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SWKGAR   DD DSN=&MIG.CRE05.WKGAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER CONTRAT
//*----------------------------------------------------------------
//SORT0501 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* CONTRAT SORT A03-CONTRAT-C-ID-CONTRAT ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03CONTR
//SORTIN   DD DISP=SHR,DSN=&MIG.A03CONTR
//SORTOUT  DD DSN=&MIG.CRE05.CONTRAT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE04
//*----------------------------------------------------------------
//SORT0502 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE04 SORT FCRE04-C-GRPT-GEST ASC [24:1—
//* SFCRE04 SORT FCRE04-C-ADH ASC [25:9—
//* SFCRE04 SORT FCRE04-C-ORDRE ASC [34:4—
//* SFCRE04 SORT FCRE04-ECRITURE-COMPTE-ENR(171:12) ASC [224:12—
//* SFCRE04 SORT FCRE04-ECRITURE-COMPTE-ENR(183:5) ASC [236:5—
//* SFCRE04 SORT FCRE04-ECRITURE-COMPTE-ENR(188:1) ASC [241:1—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE04
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE04.FCRE04
//SORTOUT  DD DSN=&MIG.CRE05.FCRE04,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE),UNIT=(SYSDA,20)
//SYSIN    DD *
  SORT FIELDS=(24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A,
               224,12,CH,A,
               236,5,CH,A,
               241,1,CH,A,
               224,7,CH,A,
               274,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TCNTGAR
//*----------------------------------------------------------------
//SORT0503 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STCNTGAR SORT CNTGAR-C-GRPT-GEST ASC [137:1—
//* STCNTGAR SORT CNTGAR-C-ADH ASC [138:9—
//* STCNTGAR SORT CNTGAR-C-ORDRE ASC [147:4—
//* STCNTGAR SORT CNTGAR-C-ID-CONTRAT ASC [175:12—
//* STCNTGAR SORT CNTGAR-C-GAR ASC [223:5—
//* STCNTGAR SORT CNTGAR-C-CADRE ASC [228:1—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TCNTGAR
//SORTIN   DD DISP=SHR,DSN=&CIB.TCNTGAR
//SORTOUT  DD DSN=&MIG.CRE05.TCNTGAR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(137,1,CH,A,
               138,9,CH,A,
               147,4,CH,A,
               175,12,CH,A,
               223,5,CH,A,
               228,1,CH,A,
               117,10,CH,D)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER WKGAR
//*----------------------------------------------------------------
//SORT0504 EXEC PGM=SORT,COND=(0,NE)
//* SWKGAR SORT WKGAR-C-GRPT-GEST ASC [91:1—
//* SWKGAR SORT WKGAR-C-ADH ASC [92:9—
//* SWKGAR SORT WKGAR-C-ORDRE ASC [101:4—
//* SWKGAR SORT WKGAR-C-ID-CONTRAT ASC [27:12—
//* SWKGAR SORT WKGAR-C-GAR ASC [56:5—
//* SWKGAR SORT WKGAR-C-CADRE ASC [61:1—
//* SWKGAR SORT WKGAR-C-CPI ASC [39:17—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.WKGAR
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE36.WKGAR
//SORTOUT  DD DSN=&MIG.CRE05.WKGAR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(91,1,CH,A,
               92,9,CH,A,
               101,4,CH,A,
               27,12,CH,A,
               56,5,CH,A,
               61,1,CH,A,
               39,17,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0502       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE05D DD DSN=&MIG.CRE05.FCRE05D,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCRE05C DD DSN=&MIG.CRE05.FCRE05C,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCRE05A DD DSN=&MIG.CRE05.FCRE05A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCRE05B DD DSN=&MIG.CRE05.FCRE05B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SGARANN  DD DSN=&CIB.FEXCL.GARANN,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SGARFER  DD DSN=&CIB.FEXCL.GARFER,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//PGARREF  DD DSN=&CIB.FEXCL.PGARREF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0503       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCRE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCRE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCRE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCRE05
//*----------------------------------------------------------------
//PPRCRE05 EXEC PGM=PPRCRE05,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCRE05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCRE05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCRE05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//CONTRAT  DD DISP=SHR,DSN=&MIG.CRE05.CONTRAT
//SFCRE04  DD DISP=SHR,DSN=&MIG.CRE05.FCRE04
//STCNTGAR DD DISP=SHR,DSN=&MIG.CRE05.TCNTGAR
//SWKGAR   DD DISP=SHR,DSN=&MIG.CRE05.WKGAR
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE05D DD DSN=&MIG.CRE05.FCRE05D,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(5000,5000),RLSE),UNIT=(SYSDA,20)
//SFCRE05C DD DSN=&MIG.CRE05.FCRE05C,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(5000,5000),RLSE),UNIT=(SYSDA,20)
//SFCRE05A DD DSN=&MIG.CRE05.FCRE05A,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(5000,5000),RLSE),UNIT=(SYSDA,20)
//SFCRE05B DD DSN=&MIG.CRE05.FCRE05B,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(5000,5000),RLSE),UNIT=(SYSDA,20)
//SGARANN  DD DSN=&CIB.FEXCL.GARANN,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SGARFER  DD DSN=&CIB.FEXCL.GARFER,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SPGARREF DD DSN=&CIB.FEXCL.PGARREF,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCRE26
//*----------------------------------------------------------------
//DEL2601       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE05  DD DSN=&MIG.CRE26.FCRE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SBFCRE05 DD DSN=&MIG.CRE26.BFCRE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE05
//*----------------------------------------------------------------
//SORT2601 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE05 SORT FCRE05-C-ID-INST-M ASC [263:7—
//* SFCRE05 SORT FCRE05-C-GRPT-GEST ASC [24:1—
//* SFCRE05 SORT FCRE05-C-ADH ASC [25:9—
//* SFCRE05 SORT FCRE05-C-ORDRE ASC [34:4—
//* SFCRE05 SORT FCRE05-C-EXE-AFFECT ASC [50:4—
//* SFCRE05 SORT FCRE05-C-PER-AFFECT-M ASC [343:2—
//* SFCRE05 SORT FCRE05-C-TYPE-PER-M ASC [345:1—
//* SFCRE05 SORT FCRE05-C-ID-GARA-REF ASC [346:4—
//* SFCRE05 SORT FCRE05-D-DEB-PER-M ASC [324:8—
//* SFCRE05 SORT FCRE05-D-FIN-PER-M ASC [332:8—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE05
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE05.FCRE05D
//SORTOUT  DD DSN=&MIG.CRE26.FCRE05,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE),UNIT=(SYSDA,20)
//SYSIN    DD *
  SORT FIELDS=(263,7,CH,A,
               24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A,
               50,4,CH,A,
               343,2,CH,A,
               345,1,CH,A,
               346,4,CH,A,
               324,8,CH,A,
               332,8,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER BFCRE05
//*----------------------------------------------------------------
//SORT2602 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SBFCRE05 SORT BFCRE05-C-ID-INST-M ASC [263:7—
//* SBFCRE05 SORT BFCRE05-C-GRPT-GEST ASC [24:1—
//* SBFCRE05 SORT BFCRE05-C-ADH ASC [25:9—
//* SBFCRE05 SORT BFCRE05-C-ORDRE ASC [34:4—
//* SBFCRE05 SORT BFCRE05-C-EXE-AFFECT ASC [50:4—
//* SBFCRE05 SORT BFCRE05-C-PER-AFFECT-M ASC [343:2—
//* SBFCRE05 SORT BFCRE05-C-TYPE-PER-M ASC [345:1—
//* SBFCRE05 SORT BFCRE05-C-ID-GARA-REF ASC [346:4—
//* SBFCRE05 SORT BFCRE05-D-DEB-PER-M ASC [324:8—
//* SBFCRE05 SORT BFCRE05-D-FIN-PER-M ASC [332:8—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE05
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE05.FCRE05C
//SORTOUT  DD DSN=&MIG.CRE26.BFCRE05,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE),UNIT=(SYSDA,20)
//SYSIN    DD *
  SORT FIELDS=(263,7,CH,A,
               24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A,
               50,4,CH,A,
               343,2,CH,A,
               345,1,CH,A,
               346,4,CH,A,
               324,8,CH,A,
               332,8,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2602       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE26  DD DSN=&MIG.CRE26.FCRE26,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SBFCRE26 DD DSN=&MIG.CRE26.BFCRE26,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2603       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCRE26,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCRE26,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCRE26,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCRE26
//*----------------------------------------------------------------
//PPRCRE26 EXEC PGM=PPRCRE26,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCRE26,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCRE26,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCRE26,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRE05  DD DISP=SHR,DSN=&MIG.CRE26.FCRE05
//SBFCRE05 DD DISP=SHR,DSN=&MIG.CRE26.BFCRE05
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE26  DD DSN=&MIG.CRE26.FCRE26,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(5000,5000),RLSE),UNIT=(SYSDA,20)
//SBFCRE26 DD DSN=&MIG.CRE26.BFCRE26,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(5000,5000),RLSE),UNIT=(SYSDA,20)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCRE06
//*----------------------------------------------------------------
//DEL0601       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE05  DD DSN=&MIG.CRE06.FCRE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SBFCRE05 DD DSN=&MIG.CRE06.BFCRE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE05
//*----------------------------------------------------------------
//SORT0601 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE05 SORT FCRE05-C-ID-INST-M ASC [263:7—
//* SFCRE05 SORT FCRE05-C-GRPT-GEST ASC [24:1—
//* SFCRE05 SORT FCRE05-C-ADH ASC [25:9—
//* SFCRE05 SORT FCRE05-C-ORDRE ASC [34:4—
//* SFCRE05 SORT FCRE05-C-EXE-AFFECT ASC [50:4—
//* SFCRE05 SORT FCRE05-C-PER-AFFECT-M ASC [343:2—
//* SFCRE05 SORT FCRE05-C-TYPE-PER-M ASC [345:1—
//* SFCRE05 SORT FCRE05-C-ID-GARA-REF ASC [346:4—
//* SFCRE05 SORT FCRE05-D-DEB-PER-M ASC [324:8—
//* SFCRE05 SORT FCRE05-D-FIN-PER-M ASC [332:8—
//* SFCRE05 SORT FCRE05-IDPOP [456:9—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE05
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE26.FCRE26
//SORTOUT  DD DSN=&MIG.CRE06.FCRE05,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(263,7,CH,A,
               24,14,CH,A,
               50,4,CH,A,
               343,2,CH,A,
               345,1,CH,A,
               346,4,CH,A,
               324,8,CH,A,
               332,8,CH,A,
               456,9,CH,A,
               38,12,CH,D)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER BFCRE05
//*----------------------------------------------------------------
//SORT0602 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SBFCRE05 SORT BFCRE05-C-ID-INST-M ASC [263:7—
//* SBFCRE05 SORT BFCRE05-C-GRPT-GEST ASC [24:1—
//* SBFCRE05 SORT BFCRE05-C-ADH ASC [25:9—
//* SBFCRE05 SORT BFCRE05-C-ORDRE ASC [34:4—
//* SBFCRE05 SORT BFCRE05-C-EXE-AFFECT ASC [50:4—
//* SBFCRE05 SORT BFCRE05-C-PER-AFFECT-M ASC [343:2—
//* SBFCRE05 SORT BFCRE05-C-TYPE-PER-M ASC [345:1—
//* SBFCRE05 SORT BFCRE05-C-ID-GARA-REF ASC [346:4—
//* SBFCRE05 SORT BFCRE05-D-DEB-PER-M ASC [324:8—
//* SBFCRE05 SORT BFCRE05-D-FIN-PER-M ASC [332:8—
//* SBFCRE05 SORT BFCRE05-IDPOP ASC [456:9—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE05
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE26.BFCRE26
//SORTOUT  DD DSN=&MIG.CRE06.BFCRE05,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(263,7,CH,A,
               24,14,CH,A,
               50,4,CH,A,
               343,2,CH,A,
               345,1,CH,A,
               346,4,CH,A,
               324,8,CH,A,
               332,8,CH,A,
               456,9,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0602       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE06  DD DSN=&MIG.CRE06.FCRE06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0603       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCRE06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCRE06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCRE06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCRE06
//*----------------------------------------------------------------
//PPRCRE06 EXEC PGM=PPRCRE06,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCRE06,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCRE06,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCRE06,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRE05  DD DISP=SHR,DSN=&MIG.CRE06.FCRE05
//SBFCRE05 DD DISP=SHR,DSN=&MIG.CRE06.BFCRE05
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE06  DD DSN=&MIG.CRE06.FCRE06,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCRE57
//*----------------------------------------------------------------
//DEL5701       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE05  DD DSN=&MIG.CRE57.FCRE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SBFCRE05 DD DSN=&MIG.CRE57.BFCRE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE05
//*----------------------------------------------------------------
//SORT5701 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE05 SORT FCRE05-C-ID-INST-M ASC [263:7—
//* SFCRE05 SORT FCRE05-C-GRPT-GEST ASC [24:1—
//* SFCRE05 SORT FCRE05-C-ADH ASC [25:9—
//* SFCRE05 SORT FCRE05-C-ORDRE ASC [34:4—
//* SFCRE05 SORT FCRE05-C-ID-GARA-REF ASC [346:4—
//* SFCRE05 SORT FCRE05-D-DEB-PER-M ASC [324:8—
//* SFCRE05 SORT FCRE05-D-FIN-PER-M ASC [332:8—
//* SFCRE05 SORT FCRE05-ECRITURE-COMPTE-ENR(75:5) ASC [128:5—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE05
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE05.FCRE05D
//SORTOUT  DD DSN=&MIG.CRE57.FCRE05,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(263,7,CH,A,
               24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A,
               346,4,CH,A,
               324,8,CH,A,
               332,8,CH,A,
               128,5,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER BFCRE05
//*----------------------------------------------------------------
//SORT5702 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SBFCRE05 SORT BFCRE05-C-ID-INST-M ASC [263:7—
//* SBFCRE05 SORT BFCRE05-C-GRPT-GEST ASC [24:1—
//* SBFCRE05 SORT BFCRE05-C-ADH ASC [25:9—
//* SBFCRE05 SORT BFCRE05-C-ORDRE ASC [34:4—
//* SBFCRE05 SORT BFCRE05-C-ID-GARA-REF ASC [346:4—
//* SBFCRE05 SORT BFCRE05-D-DEB-PER-M ASC [324:8—
//* SBFCRE05 SORT BFCRE05-D-FIN-PER-M ASC [332:8—
//* SBFCRE05 SORT BFCRE05-ECRITURE-COMPTE-ENR(75:5) ASC [128:5—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE05
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE05.FCRE05C
//SORTOUT  DD DSN=&MIG.CRE57.BFCRE05,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(263,7,CH,A,
               24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A,
               346,4,CH,A,
               324,8,CH,A,
               332,8,CH,A,
               128,5,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL5702       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE57  DD DSN=&MIG.CRE57.FCRE57,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SBFCRE57 DD DSN=&MIG.CRE57.BFCRE57,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SANCRE09 DD DSN=&CIB.ANCRE09,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL5703       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCRE57,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCRE57,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCRE57,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCRE57
//*----------------------------------------------------------------
//PPRCRE57 EXEC PGM=PPRCRE57,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCRE57,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCRE57,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCRE57,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRE05  DD DISP=SHR,DSN=&MIG.CRE57.FCRE05
//SBFCRE05 DD DISP=SHR,DSN=&MIG.CRE57.BFCRE05
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE57  DD DSN=&MIG.CRE57.FCRE57,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SBFCRE57 DD DSN=&MIG.CRE57.BFCRE57,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SANCRE09 DD DSN=&CIB.ANCRE09,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCRE07
//*----------------------------------------------------------------
//DEL0701       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE05  DD DSN=&MIG.CRE07.FCRE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SBFCRE05 DD DSN=&MIG.CRE07.BFCRE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE05
//*----------------------------------------------------------------
//SORT0701 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE05 SORT FCRE05-C-ID-INST-M ASC [263:7—
//* SFCRE05 SORT FCRE05-C-GRPT-GEST ASC [24:1—
//* SFCRE05 SORT FCRE05-C-ADH ASC [25:9—
//* SFCRE05 SORT FCRE05-C-ORDRE ASC [34:4—
//* SFCRE05 SORT FCRE05-C-ID-GARA-REF ASC [346:4—
//* SFCRE05 SORT FCRE05-D-DEB-PER-M ASC [324:8—
//* SFCRE05 SORT FCRE05-D-FIN-PER-M ASC [332:8—
//* SFCRE05 SORT FCRE05-IDPOP ASC [456:9—
//* SFCRE05 SORT F57-C-ACTION ASC [474:1—
//* SFCRE05 SORT FCRE05-ECRITURE-COMPTE-ENR(75:5) ASC [128:5—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.CPY0474
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE57.FCRE57
//SORTOUT  DD DSN=&MIG.CRE07.FCRE05,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(263,7,CH,A,
               24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A,
               346,4,CH,A,
               324,8,CH,A,
               332,8,CH,A,
               456,9,CH,A,
               474,1,CH,A,
               128,5,CH,A,
               38,12,CH,D)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER BFCRE05
//*----------------------------------------------------------------
//SORT0702 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SBFCRE05 SORT BFCRE05-C-ID-INST-M ASC [263:7—
//* SBFCRE05 SORT BFCRE05-C-GRPT-GEST ASC [24:1—
//* SBFCRE05 SORT BFCRE05-C-ADH ASC [25:9—
//* SBFCRE05 SORT BFCRE05-C-ORDRE ASC [34:4—
//* SBFCRE05 SORT BFCRE05-C-ID-GARA-REF ASC [346:4—
//* SBFCRE05 SORT BFCRE05-D-DEB-PER-M ASC [324:8—
//* SBFCRE05 SORT BFCRE05-D-FIN-PER-M ASC [332:8—
//* SBFCRE05 SORT BFCRE05-IDPOP ASC [456:9—
//* SBFCRE05 SORT BF57-C-ACTION ASC [474:1—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.CPY0474
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE57.BFCRE57
//SORTOUT  DD DSN=&MIG.CRE07.BFCRE05,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(263,7,CH,A,
               24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A,
               346,4,CH,A,
               324,8,CH,A,
               332,8,CH,A,
               456,9,CH,A,
               474,1,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0702       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE07  DD DSN=&MIG.CRE07.FCRE07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFEXCD21 DD DSN=&CIB.FEXCL.ECRD21,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0703       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCRE07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCRE07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCRE07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCRE07
//*----------------------------------------------------------------
//PPRCRE07 EXEC PGM=PPRCRE07,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCRE07,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCRE07,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCRE07,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRE05  DD DISP=SHR,DSN=&MIG.CRE07.FCRE05
//SBFCRE05 DD DISP=SHR,DSN=&MIG.CRE07.BFCRE05
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE07  DD DSN=&MIG.CRE07.FCRE07,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFEXCD21 DD DSN=&CIB.FEXCL.ECRD21,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCRE08
//*----------------------------------------------------------------
//DEL0801       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE05  DD DSN=&MIG.CRE08.FCRE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SBFCRE05 DD DSN=&MIG.CRE08.BFCRE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE05
//*----------------------------------------------------------------
//SORT0801 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE05 SORT FCRE05-C-GRPT-GEST ASC [24:1—
//* SFCRE05 SORT FCRE05-C-ADH ASC [25:9—
//* SFCRE05 SORT FCRE05-C-ORDRE ASC [34:4—
//* SFCRE05 SORT FCRE05-ECRITURE-COMPTE-ENR(66:4) ASC [119:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE05
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE05.FCRE05D
//SORTOUT  DD DSN=&MIG.CRE08.FCRE05,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE),UNIT=(SYSDA,20)
//SYSIN    DD *
  SORT FIELDS=(24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A,
               119,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER BFCRE05
//*----------------------------------------------------------------
//SORT0802 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SBFCRE05 SORT B-FCRE05-C-GRPT-GEST ASC [24:1—
//* SBFCRE05 SORT B-FCRE05-C-ADH ASC [25:9—
//* SBFCRE05 SORT B-FCRE05-C-ORDRE ASC [34:4—
//* SBFCRE05 SORT B-FCRE05-ECRITURE-COMPTE-ENR(66:4) ASC [119:4—
//* SBFCRE05 SORT B-FCRE05-ECRITURE-COMPTE-ENR(87:21) DESC [140:21]
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE05
//SORTIN01 DD DISP=SHR,DSN=&MIG.CRE06.FCRE06
//SORTIN02 DD DISP=SHR,DSN=&MIG.CRE07.FCRE07
//SORTOUT  DD DSN=&MIG.CRE08.BFCRE05,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE),UNIT=(SYSDA,20)
//SYSIN    DD *
  SORT FIELDS=(24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A,
               119,4,CH,A,
               140,21,CH,D)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0802       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE08  DD DSN=&MIG.CRE08.FCRE08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0803       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCRE08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCRE08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCRE08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCRE08
//*----------------------------------------------------------------
//PPRCRE08 EXEC PGM=PPRCRE08,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCRE08,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCRE08,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCRE08,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRE05  DD DISP=SHR,DSN=&MIG.CRE08.FCRE05
//SBFCRE05 DD DISP=SHR,DSN=&MIG.CRE08.BFCRE05
//*SB05OPER DD DISP=SHR,DSN=&MIG.CRE01.B05OPER
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE08  DD DSN=&MIG.CRE08.FCRE08,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(5000,5000),RLSE),UNIT=(SYSDA,20)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCRE09
//*----------------------------------------------------------------
//DEL0901       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE08  DD DSN=&MIG.CRE09.FCRE08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB05PERI DD DSN=&MIG.CRE09.B05PERI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SECRIT2  DD DSN=&MIG.CRE09.B05ECRI2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE08
//*----------------------------------------------------------------
//SORT0901 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE08 SORT F8-FCRE05-C-ID-PER-DEBIT ASC [38:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.CPY0514
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE08.FCRE08
//SORTOUT  DD DSN=&MIG.CRE09.FCRE08,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(38,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B05PERI
//*----------------------------------------------------------------
//SORT0902 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05PERI SORT B05-PER-DEB-C-ID-PER-DEBIT ASC [30:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.B05PERIO
//SORTIN   DD DISP=SHR,DSN=&SRC.B05PERIO
//SORTOUT  DD DSN=&MIG.CRE09.B05PERI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(30,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI2 DU FICHIER B05ECRI
//*----------------------------------------------------------------
//SORT0903 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05ECRI SORT B05-ECRITURE-COMPTE-W1-R-DEBIT ASC [37:12—
//* SB05ECRI SORT B05-ECRITURE-COMPTE-W7-E-DEBIT ASC [82:3—
//* SB05ECRI SORT B05-ECRITURE-COMPTE-W14-ONTRAT ASC [171:12—
//* SB05ECRI SORT B05-ECRITURE-COMPTE-C-GAR ASC [183:5—
//* SB05ECRI SORT B05-ECRITURE-COMPTE-C-CADRE ASC [188:1—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.B05ECRIT
//SORTIN   DD DISP=SHR,DSN=&SRC.B05ECRIT
//SORTOUT  DD DSN=&MIG.CRE09.B05ECRI2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(37,12,CH,A,
               82,3,CH,A,
               171,12,CH,A,
               183,5,CH,A,
               188,1,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0902       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE09  DD DSN=&MIG.CRE09.FCRE09,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFEXCLSO DD DSN=&CIB.FEXCL.MAJPESO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFEXCLNE DD DSN=&CIB.FEXCL.MAJPENE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0903       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCRE09,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCRE09,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCRE09,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCRE09
//*----------------------------------------------------------------
//PPRCRE09 EXEC PGM=PPRCRE09,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCRE09,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCRE09,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCRE09,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRE08  DD DISP=SHR,DSN=&MIG.CRE09.FCRE08
//SB05ECRI DD DISP=SHR,DSN=&MIG.CRE09.B05ECRI2
//SB05PERI DD DISP=SHR,DSN=&MIG.CRE09.B05PERI
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE09  DD DSN=&MIG.CRE09.FCRE09,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFEXCLSO DD DSN=&CIB.FEXCL.MAJPESO,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFEXCLNE DD DSN=&CIB.FEXCL.MAJPENE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCRE19
//*----------------------------------------------------------------
//DEL1901       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE05  DD DSN=&MIG.CRE19.FCRE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SBFCRE05 DD DSN=&MIG.CRE19.BFCRE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCFCRE05 DD DSN=&MIG.CRE19.CFCRE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE05
//*----------------------------------------------------------------
//SORT1901 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE05 SORT FCRE05-C-GRP-GEST ASC [24:1—
//* SFCRE05 SORT FCRE05-C-ADH ASC [25:9—
//* SFCRE05 SORT FCRE05-C-ORDRE ASC [34:4—
//* SFCRE05 SORT FCRE05-C-EXE-AFFECT ASC [50:4—
//* SFCRE05 SORT FCRE05-C-PER-AFFECT-M ASC [343:2—
//* SFCRE05 SORT FCRE05-C-TYPE-PER-M ASC [345:1—
//* SFCRE05 SORT FCRE05-ECRITURE-COMPTE-ENR(82:3) ASC [135:3—
//* SFCRE05 SORT FCRE05-ECRITURE-COMPTE-ENR(129:8) ASC [182:8—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE05
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE05.FCRE05D
//SORTOUT  DD DSN=&MIG.CRE19.FCRE05,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               50,4,CH,A,
               343,2,CH,A,
               345,1,CH,A,
               135,3,CH,A,
               182,8,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER BFCRE05
//*----------------------------------------------------------------
//SORT1902 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SBFCRE05 SORT BFCRE05-C-GRP-GEST ASC [24:1—
//* SBFCRE05 SORT BFCRE05-C-ADH ASC [25:9—
//* SBFCRE05 SORT BFCRE05-C-ORDRE ASC [34:4—
//* SBFCRE05 SORT BFCRE05-C-EXE-AFFECT ASC [50:4—
//* SBFCRE05 SORT BFCRE05-C-PER-AFFECT-M ASC [343:2—
//* SBFCRE05 SORT BFCRE05-C-TYPE-PER-M ASC [345:1—
//* SBFCRE05 SORT BFCRE05-ECRITURE-COMPTE-ENR(82:3) ASC [135:3—
//* SCFCRE05 SORT B-FCRE05-ECRITURE-COMPTE-ENR(87:21) ASC [140:21—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE05
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE05.FCRE05C
//SORTOUT  DD DSN=&MIG.CRE19.BFCRE05,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               50,4,CH,A,
               343,2,CH,A,
               345,1,CH,A,
               135,3,CH,A,
               140,21,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER CFCRE05
//*----------------------------------------------------------------
//SORT1903 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SCFCRE05 SORT B-FCRE05-C-GRPT-GEST ASC [24:1—
//* SCFCRE05 SORT B-FCRE05-C-ADH ASC [25:9—
//* SCFCRE05 SORT B-FCRE05-C-ORDRE ASC [34:4—
//* SCFCRE05 SORT B-FCRE05-C-EXE-AFFECT ASC [50:4—
//* SCFCRE05 SORT B-FCRE05-C-PER-AFFECT-M ASC [343:2—
//* SCFCRE05 SORT B-FCRE05-C-TYPE-PER-M ASC [345:1—
//* SCFCRE05 SORT B-FCRE05-ECRITURE-COMPTE-ENR(87:21) DESC [140:21—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE05
//SORTIN01 DD DISP=SHR,DSN=&MIG.CRE06.FCRE06
//SORTIN02 DD DISP=SHR,DSN=&MIG.CRE07.FCRE07
//SORTOUT  DD DSN=&MIG.CRE19.CFCRE05,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               50,4,CH,A,
               343,2,CH,A,
               345,1,CH,A,
               140,21,CH,D)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1902       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE19  DD DSN=&MIG.CRE19.FCRE19,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFEXCLSO DD DSN=&CIB.FEXCL.FRAISSO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFEXCLNE DD DSN=&CIB.FEXCL.FRAISNE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1903       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCRE19,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCRE19,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCRE19,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCRE19
//*----------------------------------------------------------------
//PPRCRE19 EXEC PGM=PPRCRE19,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCRE19,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCRE19,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCRE19,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRE05  DD DISP=SHR,DSN=&MIG.CRE19.FCRE05
//SBFCRE05 DD DISP=SHR,DSN=&MIG.CRE19.BFCRE05
//SCFCRE05 DD DISP=SHR,DSN=&MIG.CRE19.CFCRE05
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE19  DD DSN=&MIG.CRE19.FCRE19,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFEXCLSO DD DSN=&CIB.FEXCL.FRAISSO,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFEXCLNE DD DSN=&CIB.FEXCL.FRAISNE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCRE10
//*----------------------------------------------------------------
//DEL1001       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE19  DD DSN=&MIG.CRE10.FCRE19,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCRE05C DD DSN=&MIG.CRE10.FCRE05C,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE19
//*----------------------------------------------------------------
//SORT1001 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE19 SORT F19-FCRE05-C-GRPT-GEST ASC [24:1—
//* SFCRE19 SORT F19-FCRE05-C-ADH ASC [25:9—
//* SFCRE19 SORT F19-FCRE05-C-ORDRE ASC [34:4—
//* SFCRE19 SORT F19-FCRE05-C-EXE-AFFECT ASC [50:4—
//* SFCRE19 SORT F19-FCRE05-C-PER-AFFECT-M ASC [343:2—
//* SFCRE19 SORT F19-FCRE05-C-TYPE-PER-M ASC [345:1—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE05
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE19.FCRE19
//SORTOUT  DD DSN=&MIG.CRE10.FCRE19,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A,
               50,4,CH,A,
               343,2,CH,A,
               345,1,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE05C
//*----------------------------------------------------------------
//SORT1002 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE05C SORT CFCRE05-C-GRPT-GEST ASC [24:1—
//* SFCRE05C SORT CFCRE05-C-ADH ASC [25:9—
//* SFCRE05C SORT CFCRE05-C-ORDRE ASC [34:4—
//* SFCRE05C SORT CFCRE05-C-EXE-AFFECT ASC [50:4—
//* SFCRE05C SORT CFCRE05-C-PER-AFFECT-M ASC [343:2—
//* SFCRE05C SORT CFCRE05-C-TYPE-PER-M ASC [345:1—
//* SFCRE05C SORT CFCRE05-ECRITURE-COMPTE-ENR(87:21) DESC [140:21—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE05
//SORTIN01 DD DISP=SHR,DSN=&MIG.CRE06.FCRE06
//SORTIN02 DD DISP=SHR,DSN=&MIG.CRE07.FCRE07
//SORTOUT  DD DSN=&MIG.CRE10.FCRE05C,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A,
               50,4,CH,A,
               343,2,CH,A,
               345,1,CH,A,
               140,21,CH,D)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE10R DD DSN=&MIG.CRE10.FCRE10R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCRE10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCRE10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCRE10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCRE10
//*----------------------------------------------------------------
//PPRCRE10 EXEC PGM=PPRCRE10,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCRE10,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCRE10,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCRE10,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRE19  DD DISP=SHR,DSN=&MIG.CRE10.FCRE19
//SFCRE05C DD DISP=SHR,DSN=&MIG.CRE10.FCRE05C
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE10R DD DSN=&MIG.CRE10.FCRE10R,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCRE11
//*----------------------------------------------------------------
//DEL1101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE19  DD DSN=&MIG.CRE11.FCRE19,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SBFCRE05 DD DSN=&MIG.CRE11.BFCRE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCRE10R DD DSN=&MIG.CRE11.FCRE10R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE19
//*----------------------------------------------------------------
//SORT1101 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE19 SORT F19-FCRE05-C-ID-PER-DEBIT ASC [38:12—
//* SFCRE19 SORT F19-FCRE05-ECRITURE-COMPTE-ENR(82:3) ASC [135:3—
//* SFCRE19 SORT F19-FCRE05-ECRITURE-COMPTE-ENR(129:8) ASC [182:8—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE05
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE19.FCRE19
//SORTOUT  DD DSN=&MIG.CRE11.FCRE19,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(38,12,CH,A,
               135,3,CH,A,
               182,8,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER BFCRE05
//*----------------------------------------------------------------
//SORT1102 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE19B SORT F9B-FCRE05-C-ID-PER-DEBIT ASC [38:12—
//* SFCRE19B SORT F9B-FCRE05-ECRITURE-COMPTE-ENR(129:8) ASC [182:8—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE05
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE05.FCRE05C
//SORTOUT  DD DSN=&MIG.CRE11.BFCRE05,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(38,12,CH,A,
               182,8,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE10R
//*----------------------------------------------------------------
//SORT1103 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE10R SORT FCRE10R-C-ID-PER-DEBIT ASC [1:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.CPY0037
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE10.FCRE10R
//SORTOUT  DD DSN=&MIG.CRE11.FCRE10R,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE11  DD DSN=&MIG.CRE11.FCRE11,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCRE11,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCRE11,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCRE11,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCRE11
//*----------------------------------------------------------------
//PPRCRE11 EXEC PGM=PPRCRE11,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCRE11,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCRE11,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCRE11,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRE19  DD DISP=SHR,DSN=&MIG.CRE11.FCRE19
//SFCRE19B DD DISP=SHR,DSN=&MIG.CRE11.FCRE19
//SBFCRE05 DD DISP=SHR,DSN=&MIG.CRE11.BFCRE05
//SFCRE10R DD DISP=SHR,DSN=&MIG.CRE11.FCRE10R
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE11  DD DSN=&MIG.CRE11.FCRE11,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCRE12
//*----------------------------------------------------------------
//DEL1201       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SB05INFO DD DSN=&MIG.CRE12.B05INFO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB05OPEV DD DSN=&MIG.CRE12.B05OPEV,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER B05INFO
//*----------------------------------------------------------------
//SORT1201 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05INFO SORT B05-INFOS-DEBIT-C-OPER ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.B05INFOD
//SORTIN   DD DISP=SHR,DSN=&SRC.B05INFOD
//SORTOUT  DD DSN=&MIG.CRE12.B05INFO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B05OPEV
//*----------------------------------------------------------------
//SORT1202 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05OPEV SORT B05-OPER-VENTIL-C-OPER ASC [16:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.B05OPERV
//SORTIN   DD DISP=SHR,DSN=&SRC.B05OPERV
//SORTOUT  DD DSN=&MIG.CRE12.B05OPEV,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1202       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFOPE12  DD DSN=&MIG.CRE12.FOPE12,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1203       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCRE12,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCRE12,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCRE12,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCRE12
//*----------------------------------------------------------------
//PPRCRE12 EXEC PGM=PPRCRE12,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCRE12,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCRE12,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCRE12,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SB05OPER DD DISP=SHR,DSN=&MIG.CRE61.B05OPER
//SB05INFO DD DISP=SHR,DSN=&MIG.CRE12.B05INFO
//SB05OPEV DD DISP=SHR,DSN=&MIG.CRE12.B05OPEV
//*--------------<FICHIERS CIBLES>---------------------------------
//SFOPE12  DD DSN=&MIG.CRE12.FOPE12,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCRE13
//*----------------------------------------------------------------
//DEL1301       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFOPE12  DD DSN=&MIG.CRE13.FOPE12,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FOPE12
//*----------------------------------------------------------------
//SORT1301 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFOPE12 SORT FOPE12-C-ID-RECOUVR ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FOPE12
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE12.FOPE12
//SORTOUT  DD DSN=&MIG.CRE13.FOPE12,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1302       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFOPE13  DD DSN=&MIG.CRE13.FOPE13,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1303       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCRE13,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCRE13,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCRE13,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCRE13
//*----------------------------------------------------------------
//PPRCRE13 EXEC PGM=PPRCRE13,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCRE13,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCRE13,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCRE13,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFOPE12  DD DISP=SHR,DSN=&MIG.CRE13.FOPE12
//SB07RECO DD DISP=SHR,DSN=&MIG.CRE59.B07RECO
//SB05PERI DD DISP=SHR,DSN=&MIG.CRE59.B05PER2
//*--------------<FICHIERS CIBLES>---------------------------------
//SFOPE13  DD DSN=&MIG.CRE13.FOPE13,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCRE14
//*----------------------------------------------------------------
//DEL1401       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFOPE13  DD DSN=&MIG.CRE14.FOPE13,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STCNTGAR DD DSN=&MIG.CRE14.TCNTGAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FOPE13
//*----------------------------------------------------------------
//SORT1401 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFOPE13 SORT FOPE13-OPER-COMPTE-ENR(41:1) ASC [82:1—
//* SFOPE13 SORT FOPE13-OPER-COMPTE-ENR(43:9) ASC [84:9—
//* SFOPE13 SORT FOPE13-OPER-COMPTE-ENR(53:4) ASC [94:4—
//* SFOPE13 SORT FOPE13-OPER-COMPTE-ENR(87:4) ASC [128:4—
//* SFOPE13 SORT FOPE13-OPER-COMPTE-ENR(94:1) ASC [135:1—
//* SFOPE13 SORT FOPE13-OPER-COMPTE-ENR(92:2) ASC [133:2—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FOPE13
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE13.FOPE13
//SORTOUT  DD DSN=&MIG.CRE14.FOPE13,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(82,1,CH,A,
               84,9,CH,A,
               94,4,CH,A,
               128,4,CH,A,
               135,1,CH,A,
               133,2,CH,A,
               14,12,CH,D)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TCNTGAR
//*----------------------------------------------------------------
//SORT1402 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STCNTGAR SORT CNTGAR-C-GRPT-GEST ASC [137:1—
//* STCNTGAR SORT CNTGAR-C-ADH ASC [138:9—
//* STCNTGAR SORT CNTGAR-C-ORDRE ASC [147:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TCNTGAR
//SORTIN   DD DISP=SHR,DSN=&CIB.TCNTGAR
//SORTOUT  DD DSN=&MIG.CRE14.TCNTGAR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(137,1,CH,A,
               138,9,CH,A,
               147,4,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1402       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE14  DD DSN=&MIG.CRE14.FCRE14,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1403       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCRE14,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCRE14,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCRE14,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCRE14
//*----------------------------------------------------------------
//PPRCRE14 EXEC PGM=PPRCRE14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCRE14,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCRE14,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCRE14,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//TPERIOD  DD DISP=SHR,DSN=&TAB.TPERIOD
//SFOPE13  DD DISP=SHR,DSN=&MIG.CRE14.FOPE13
//STADHADH DD DISP=SHR,DSN=&MIG.CRE01.TADHADH
//SB05OPER DD DISP=SHR,DSN=&MIG.CRE01.B05OPER
//STCNTGAR DD DISP=SHR,DSN=&MIG.CRE14.TCNTGAR
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE14  DD DSN=&MIG.CRE14.FCRE14,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM .
//*----------------------------------------------------------------
//DEL1501       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE05  DD DSN=&MIG.CRE15.FCRE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB05SYNT DD DSN=&MIG.CRE15.B05SYNT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STCNTGAR DD DSN=&MIG.CRE15.TCNTGAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE05
//*----------------------------------------------------------------
//SORT1501 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE05 SORT FCRE05-C-GRPT-GEST ASC [24:1—
//* SFCRE05 SORT FCRE05-C-ADH ASC [25:9—
//* SFCRE05 SORT FCRE05-C-ORDRE ASC [34:4—
//* SFCRE05 SORT FCRE05-ECRITURE-COMPTE-ENR(66:4) ASC [119:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE05
//SORTIN01 DD DISP=SHR,DSN=&MIG.CRE05.FCRE05D
//SORTIN02 DD DISP=SHR,DSN=&MIG.CRE05.FCRE05C
//SORTOUT  DD DSN=&MIG.CRE15.FCRE05,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A,
               119,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B05SYNT
//*----------------------------------------------------------------
//SORT1502 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05SYNT SORT B05-SYNTHESE-CPTE-C-GRPT-GEST ASC [1:1—
//* SB05SYNT SORT B05-SYNTHESE-CPTE-C-ADH ASC [3:9—
//* SB05SYNT SORT B05-SYNTHESE-CPTE-C-ORDRE ASC [13:4—
//* SB05SYNT SORT B05-SYNTHESE-CPTE-C-EXE ASC [18:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.B05SYNTH
//SORTIN   DD DISP=SHR,DSN=&SRC.B05SYNTH
//SORTOUT  DD DSN=&MIG.CRE15.B05SYNT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,1,CH,A,
               3,9,CH,A,
               13,4,CH,A,
               18,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TCNTGAR
//*----------------------------------------------------------------
//SORT1503 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STCNTGAR SORT CNTGAR-C-GRPT-GEST ASC [137:1—
//* STCNTGAR SORT CNTGAR-C-ADH ASC [138:9—
//* STCNTGAR SORT CNTGAR-C-ORDRE ASC [147:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TCNTGAR
//SORTIN   DD DISP=SHR,DSN=&CIB.TCNTGAR
//SORTOUT  DD DSN=&MIG.CRE15.TCNTGAR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(137,1,CH,A,
               138,9,CH,A,
               147,4,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1502       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE15  DD DSN=&MIG.CRE15.FCRE15,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1503       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCRE15,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCRE15,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCRE15,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCRE15
//*----------------------------------------------------------------
//PPRCRE15 EXEC PGM=PPRCRE15,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCRE15,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCRE15,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCRE15,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRE05  DD DISP=SHR,DSN=&MIG.CRE15.FCRE05
//SB05SYNT DD DISP=SHR,DSN=&MIG.CRE15.B05SYNT
//STCNTGAR DD DISP=SHR,DSN=&MIG.CRE15.TCNTGAR
//STADHADH DD DISP=SHR,DSN=&MIG.CRE01.TADHADH
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE15  DD DSN=&MIG.CRE15.FCRE15,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCRE25
//*----------------------------------------------------------------
//DEL2501       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE15  DD DSN=&MIG.CRE25.FCRE15,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE15
//*----------------------------------------------------------------
//SORT2501 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE15 SORT F15-FCRE05-C-ID-PER-DEBIT ASC [38:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE05
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE15.FCRE15
//SORTOUT  DD DSN=&MIG.CRE25.FCRE15,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(38,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2502       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE25  DD DSN=&MIG.CRE25.FCRE25,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2503       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCRE25,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCRE25,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCRE25,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCRE25
//*----------------------------------------------------------------
//PPRCRE25 EXEC PGM=PPRCRE25,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCRE25,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCRE25,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCRE25,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRE15  DD DISP=SHR,DSN=&MIG.CRE25.FCRE15
//SB05ECRI DD DISP=SHR,DSN=&MIG.CRE09.B05ECRI2
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE25  DD DSN=&MIG.CRE25.FCRE25,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCRE16
//*----------------------------------------------------------------
//DEL1601       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE15  DD DSN=&MIG.CRE16.FCRE15,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE15
//*----------------------------------------------------------------
//SORT1601 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE15 SORT F15-FCRE05-C-ID-INST-M ASC [263:7—
//* SFCRE15 SORT F15-FCRE05-C-GRPT-GEST ASC [24:1—
//* SFCRE15 SORT F15-FCRE05-C-ADH ASC [25:9—
//* SFCRE15 SORT F15-FCRE05-C-ORDRE ASC [34:4—
//* SFCRE15 SORT F15-FCRE05-C-ID-GARA-REF ASC [346:4—
//* SFCRE05 SORT F15-FCRE05-IDPOP ASC [456:9—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE25
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE25.FCRE25
//SORTOUT  DD DSN=&MIG.CRE16.FCRE15,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(263,7,CH,A,
               1,20,CH,A,
               21,3,CH,A,
               346,4,CH,A,
               456,9,CH,A,
               38,12,CH,D)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1602       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE16  DD DSN=&MIG.CRE16.FCRE16,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1603       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCRE16,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCRE16,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCRE16,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCRE16
//*----------------------------------------------------------------
//PPRCRE16 EXEC PGM=PPRCRE16,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCRE16,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCRE16,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCRE16,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRE15  DD DISP=SHR,DSN=&MIG.CRE16.FCRE15
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE16  DD DSN=&MIG.CRE16.FCRE16,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCRE17
//*----------------------------------------------------------------
//DEL1701       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE15  DD DSN=&MIG.CRE17.FCRE15,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE15
//*----------------------------------------------------------------
//SORT1701 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE15 SORT F15-FCRE05-C-ID-INST-M ASC [263:7—
//* SFCRE15 SORT F15-FCRE05-C-GRPT-GEST ASC [24:1—
//* SFCRE15 SORT F15-FCRE05-C-ADH ASC [25:9—
//* SFCRE15 SORT F15-FCRE05-C-ORDRE ASC [34:4—
//* SFCRE15 SORT F15-FCRE05-ECRITURE-COMPTE-ENR(81:1) ASC [134:1—
//* SFCRE15 SORT F15-FCRE05-ECRITURE-COMPTE-ENR(207:1) ASC [260:1—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE25
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE25.FCRE25
//SORTOUT  DD DSN=&MIG.CRE17.FCRE15,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(263,7,CH,A,
               1,20,CH,A,
               21,3,CH,A,
               134,1,CH,A,
               260,1,CH,A,
               38,12,CH,D)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1702       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE17  DD DSN=&MIG.CRE17.FCRE17,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1703       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCRE17,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCRE17,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCRE17,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCRE17
//*----------------------------------------------------------------
//PPRCRE17 EXEC PGM=PPRCRE17,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCRE17,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCRE17,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCRE17,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRE15  DD DISP=SHR,DSN=&MIG.CRE17.FCRE15
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE17  DD DSN=&MIG.CRE17.FCRE17,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCRE18
//*----------------------------------------------------------------
//DEL1801       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE15  DD DSN=&MIG.CRE18.FCRE15,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCRE15  DD DSN=&MIG.CRE18.DCRE15,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE15
//*----------------------------------------------------------------
//SORT1801 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE15 SORT F15-FCRE05-C-ID-INST-M ASC [263:7—
//* SFCRE15 SORT F15-FCRE05-C-GRPT-GEST ASC [24:1—
//* SFCRE15 SORT F15-FCRE05-C-ADH ASC [25:9—
//* SFCRE15 SORT F15-FCRE05-C-ORDRE ASC [34:4—
//* SFCRE15 SORT F15-FCRE05-ECRITURE-COMPTE-ENR(82:3) ASC [135:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE25
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE25.FCRE25
//SORTOUT  DD DSN=&MIG.CRE18.FCRE15,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               135,3,CH,A,
               263,7,CH,A)
/*
//SORT1802 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE15 SORT F15-FCRE05-C-ID-INST-M ASC [263:7—
//* SFCRE15 SORT F15-FCRE05-C-GRPT-GEST ASC [24:1—
//* SFCRE15 SORT F15-FCRE05-C-ADH ASC [25:9—
//* SFCRE15 SORT F15-FCRE05-C-ORDRE ASC [34:4—
//* SFCRE15 SORT F15-FCRE05-ECRITURE-COMPTE-ENR(82:3) ASC [135:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE25
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE25.FCRE25
//SORTOUT  DD DSN=&MIG.CRE18.DCRE15,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               135,3,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1802       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE18  DD DSN=&MIG.CRE18.FCRE18,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1803       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCRE18,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCRE18,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCRE18,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCRE18
//*----------------------------------------------------------------
//PPRCRE18 EXEC PGM=PPRCRE18,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCRE18,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCRE18,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCRE18,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRE15  DD DISP=SHR,DSN=&MIG.CRE18.FCRE15
//SFCRE15C DD DISP=SHR,DSN=&MIG.CRE18.DCRE15
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRE18  DD DSN=&MIG.CRE18.FCRE18,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCRE20
//*----------------------------------------------------------------
//DEL2001       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFICPROF DD DSN=&MIG.CRE20.FICPROF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FICPROF
//*----------------------------------------------------------------
//SORT2001 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFICPROF SORT FICPROF-C-ID-PER-DEBIT ASC [35:12—
//* SFICPROF SORT FICPROF-ECRITURE-COMPTE-ENR(16:7) ASC [62:7—
//* SFICPROF SORT FICPROF-ECRITURE-COMPTE-ENR(66:4) ASC [112:4—
//* SFICPROF SORT FICPROF-ECRITURE-COMPTE-ENR(71:2) ASC [117:2—
//* SFICPROF SORT FICPROF-ECRITURE-COMPTE-ENR(73:1) ASC [119:1—
//* SFICPROF SORT FICPROF-ECRITURE-COMPTE-ENR(75:5) ASC [121:5—
//* SFICPROF SORT FICPROF-ECRITURE-COMPTE-ENR(81:1) ASC [127:1—
//* SFICPROF SORT FICPROF-ECRITURE-COMPTE-ENR(82:3) ASC [128:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.CPY0255
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE01.FICPROF
//SORTOUT  DD DSN=&MIG.CRE20.FICPROF,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(35,12,CH,A,
               62,7,CH,A,
               112,4,CH,A,
               117,2,CH,A,
               119,1,CH,A,
               121,5,CH,A,
               127,1,CH,A,
               128,3,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFEXCLPR DD DSN=&CIB.FEXCL.PROFIT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCRE20,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCRE20,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCRE20,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCRE20
//*----------------------------------------------------------------
//PPRCRE20 EXEC PGM=PPRCRE20,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCRE20,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCRE20,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCRE20,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFICPROF DD DISP=SHR,DSN=&MIG.CRE20.FICPROF
//SB05PERI DD DISP=SHR,DSN=&MIG.CRE09.B05PERI
//*--------------<FICHIERS CIBLES>---------------------------------
//SFEXCLPR DD DSN=&CIB.FEXCL.PROFIT,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCRE21
//*----------------------------------------------------------------
//DEL2101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FCRE09   DD DSN=&MIG.CRE21.FCRE09,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE09
//*----------------------------------------------------------------
//SORT2101 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* FCRE09 SORT F09-FCRE05-C-GRPT-GEST ASC [24:1—
//* FCRE09 SORT F09-FCRE05-C-ADH ASC [25:9—
//* FCRE09 SORT F09-FCRE05-C-ORDRE ASC [34:4—
//* FCRE09 SORT F09-FCRE05-C-EXE-AFFECT ASC [50:4—
//* FCRE09 SORT F09-FCRE05-C-ID-INST-M ASC [263:7—
//* FCRE09 SORT F09-FCRE05-ECRITURE-COMPTE-ENR(171:12) ASC [224:12—
//* FCRE09 SORT F09-FCRE05-ECRITURE-COMPTE-ENR(183:5) ASC [236:5—
//* FCRE09 SORT F09-FCRE05-ECRITURE-COMPTE-ENR(188:1) ASC [241:1—
//* FCRE09 SORT F09-FCRE05-ECRITURE-COMPTE-ENR(81:1) ASC [134:1—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRE05
//SORTIN   DD DISP=SHR,DSN=&MIG.CRE09.FCRE09
//SORTOUT  DD DSN=&MIG.CRE21.FCRE09,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A,
               50,4,CH,A,
               263,7,CH,A,
               224,12,CH,A,
               236,5,CH,A,
               241,1,CH,A,
               134,1,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SCRE21  DD DSN=&MIG.CRE21.SCRE21,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCRE21,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCRE21,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCRE21,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCRE21
//*----------------------------------------------------------------
//PPRCRE21 EXEC PGM=PPRCRE21,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCRE21,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCRE21,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCRE21,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//FCRE06   DD DISP=SHR,DSN=&MIG.CRE06.FCRE06
//FCRE07   DD DISP=SHR,DSN=&MIG.CRE07.FCRE07
//*FCRE09   DD DISP=SHR,DSN=&MIG.CRE09.FCRE09
//FCRE09   DD DISP=SHR,DSN=&MIG.CRE21.FCRE09
//FCRE11   DD DISP=SHR,DSN=&MIG.CRE11.FCRE11
//FCRE14   DD DISP=SHR,DSN=&MIG.CRE14.FCRE14
//FCRE16   DD DISP=SHR,DSN=&MIG.CRE16.FCRE16
//FCRE17   DD DISP=SHR,DSN=&MIG.CRE17.FCRE17
//FCRE18   DD DISP=SHR,DSN=&MIG.CRE18.FCRE18
//*--------------<FICHIERS CIBLES>---------------------------------
//SCRE21  DD DSN=&MIG.CRE21.CRE21,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
/*