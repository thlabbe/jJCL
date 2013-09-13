//J9PRPAI  JOB UTI00TX0,'PR GRECCO G04-PAI',CLASS=Z,MSGCLASS=X,
//*         RESTART=DEL3401,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=PLURM.MOVE.PRREF.JCL.PROD
//MEMBER   INCLUDE MEMBER=$$PRR
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRPAI30
//*----------------------------------------------------------------
//DEL3001       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SB05INFP DD DSN=&MIG.PAI30.B05INFP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA01RIBF DD DSN=&MIG.PAI30.A01RIBF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER B05INFP
//*----------------------------------------------------------------
//SORT3001 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05INFP SORT B05-INFOS-PAI-C-ID-RIB-UTILI ASC [31:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.B05INFOP
//SORTIN   DD DISP=SHR,DSN=&SRC.B05INFOP
//SORTOUT  DD DSN=&MIG.PAI30.B05INFP,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(31,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A01RIBF
//*----------------------------------------------------------------
//SORT3002 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01RIBF SORT A01-RIB-FRANCE-C-ID-RIB ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01RIBFR
//SORTIN   DD DISP=SHR,DSN=&SRC.A01RIBFR
//SORTOUT  DD DSN=&MIG.PAI30.A01RIBF,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL3002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFPAI30  DD DSN=&MIG.PAI30.FPAI30,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL3003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRPAI30,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRPAI30,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRPAI30,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZB05INFP DD DSN=&MIG.PAI30.B05INFP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01RIBF DD DSN=&MIG.PAI30.A01RIBF.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPAI30  DD DSN=&MIG.PAI30.FPAI30.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRPAI30
//*----------------------------------------------------------------
//PPRPAI30 EXEC PGM=PPRPAI30,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRPAI30,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRPAI30,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRPAI30,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SB05INFP DD DISP=SHR,DSN=&MIG.PAI30.B05INFP
//SA01RIBF DD DISP=SHR,DSN=&MIG.PAI30.A01RIBF
//*--------------<FICHIERS CIBLES>---------------------------------
//SFPAI30  DD DSN=&MIG.PAI30.FPAI30,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZB05INFP DD DSN=&MIG.PAI30.B05INFP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01RIBF DD DSN=&MIG.PAI30.A01RIBF.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPAI30  DD DSN=&MIG.PAI30.FPAI30.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRPAI31
//*----------------------------------------------------------------
//DEL3101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//B05REFO  DD DSN=&MIG.PAI31.B05REFO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB05OPER DD DSN=&MIG.PAI31.B05OPER,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFEXCADH DD DSN=&MIG.PAI31.FEXCADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STADHADH DD DSN=&MIG.PAI31.TADHADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SURCOMTI DD DSN=&MIG.PAI31.URCOMTI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER B05REFO
//*----------------------------------------------------------------
//SORT3103 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.B05REFOR
//SORTIN   DD DISP=SHR,DSN=&SRC.B05REFOR
//SORTOUT  DD DSN=&MIG.PAI31.B05REFO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,2,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B05OPER
//*----------------------------------------------------------------
//SORT3104 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05OPER SORT B05-OPER-COMPTE-C-GRPT-GEST ASC [41:1—
//* SB05OPER SORT B05-OPER-COMPTE-C-ADH ASC [43:9—
//* SB05OPER SORT B05-OPER-COMPTE-C-ORDRE ASC [53:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//*BIEN LAISSER &SRC.B05OPER POUR RECUPERER LES ADH MANDATAIRES
//COPY     DD DISP=SHR,DSN=&CPYSRC.B05OPER
//SORTIN   DD DISP=SHR,DSN=&SRC.B05OPER
//SORTOUT  DD DSN=&MIG.PAI31.B05OPER,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(41,1,CH,A,
               43,9,CH,A,
               53,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FEXCADH   Inutilisé
//*----------------------------------------------------------------
//*SORT3105 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFEXCADH SORT EXCLA-C-GRPT-GEST ASC [1:1—
//* SFEXCADH SORT EXCLA-C-ADH ASC [2:9—
//* SFEXCADH SORT EXCLA-C-ORDRE ASC [11:4—
//* FIN   CRITERE XGEN
//*SYSOUT   DD SYSOUT=*
//*COPY     DD DISP=SHR,DSN=&CPYCIB.ADHER
//*SORTIN   DD DISP=SHR,DSN=&CIB.FEXCL.ADHER
//*SORTOUT  DD DSN=&MIG.PAI31.FEXCADH,
//*            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//*            SPACE=(TRK,(5000,500),RLSE)
//*SYSIN    DD *
//*  SORT FIELDS=(1,1,CH,A,
//*               2,9,CH,A,
//*               11,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TADHADH
//*----------------------------------------------------------------
//SORT3106 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STADHADH SORT ADHSADH-C-GRPT-GEST ASC [94:1—
//* STADHADH SORT ADHSADH-C-ADH ASC [95:9—
//* STADHADH SORT ADHSADH-C-ORDRE ASC [104:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TADHADH
//SORTIN   DD DISP=SHR,DSN=&CIB.TADHADH
//SORTOUT  DD DSN=&MIG.PAI31.TADHADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(94,1,CH,A,
               95,9,CH,A,
               104,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER URCOMTI
//*----------------------------------------------------------------
//SORT3107 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SURCOMTI SORT URCOMTIE-C-GRPT-GEST ASC [49:1—
//* SURCOMTI SORT URCOMTIE-C-ADH ASC [50:9—
//* SURCOMTI SORT URCOMTIE-C-ORDRE ASC [59:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.URCOMTIE
//SORTIN   DD DISP=SHR,DSN=&TAB.URCOMTI
//SORTOUT  DD DSN=&MIG.PAI31.URCOMTI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,1,CH,A,
               50,9,CH,A,
               59,4,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL3102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFPAI01  DD DSN=&MIG.PAI31.FPAI01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPAI02  DD DSN=&MIG.PAI31.FPAI02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SAFFFCU  DD DSN=&MIG.PAI31.AFFFCU,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL3103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRPAI31,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRPAI31,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRPAI31,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZB05REFO DD DSN=&MIG.PAI31.B05REFO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZB05OPER DD DSN=&MIG.PAI31.B05OPER.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFEXCADH DD DSN=&MIG.PAI31.FEXCADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTADHADH DD DSN=&MIG.PAI31.TADHADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZURCOMTI DD DSN=&MIG.PAI31.URCOMTI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPAI01  DD DSN=&MIG.PAI31.FPAI01.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPAI02  DD DSN=&MIG.PAI31.FPAI02.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZAFFFCU  DD DSN=&MIG.PAI31.AFFFCU.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRPAI31
//*----------------------------------------------------------------
//PPRPAI31 EXEC PGM=PPRPAI31,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRPAI31,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRPAI31,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRPAI31,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//*TGESPAI  DD DISP=SHR,DSN=&TAB.TGESPAI
//*TC1RRIB  DD DISP=SHR,DSN=&TAB.TC1RRIB
//B05REFO  DD DISP=SHR,DSN=&MIG.PAI31.B05REFO
//SB05OPER DD DISP=SHR,DSN=&MIG.PAI31.B05OPER
//* Inutilisé SFEXCADH DD DISP=SHR,DSN=&MIG.PAI31.FEXCADH
//SFEXCADH DD DUMMY
//STADHADH DD DISP=SHR,DSN=&MIG.PAI31.TADHADH
//SURCOMTI DD DISP=SHR,DSN=&MIG.PAI31.URCOMTI
//*--------------<FICHIERS CIBLES>---------------------------------
//SFPAI01  DD DSN=&MIG.PAI31.FPAI01,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFPAI02  DD DSN=&MIG.PAI31.FPAI02,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SAFFFCU  DD DSN=&MIG.PAI31.AFFFCU,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZB05REFO DD DSN=&MIG.PAI31.B05REFO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZB05OPER DD DSN=&MIG.PAI31.B05OPER.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFEXCADH DD DSN=&MIG.PAI31.FEXCADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTADHADH DD DSN=&MIG.PAI31.TADHADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZURCOMTI DD DSN=&MIG.PAI31.URCOMTI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPAI01  DD DSN=&MIG.PAI31.FPAI01.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPAI02  DD DSN=&MIG.PAI31.FPAI02.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZAFFFCU  DD DSN=&MIG.PAI31.AFFFCU.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRPAI32
//*----------------------------------------------------------------
//DEL3201       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SEPAI    DD DSN=&MIG.PAI32.EPAI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB05INFP DD DSN=&MIG.PAI32.B05INFP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB05INFR DD DSN=&MIG.PAI32.B05INFR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPAI30  DD DSN=&MIG.PAI32.FPAI30,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER EPAI
//*----------------------------------------------------------------
//SORT3201 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SEPAI SORT E-PAI-IDPAIRET(1:10) ASC [56:10—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.PAI
//SORTIN   DD DISP=SHR,DSN=&MIG.PAI31.FPAI01
//SORTOUT  DD DSN=&MIG.PAI32.EPAI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(56,10,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B05INFP
//*----------------------------------------------------------------
//SORT3202 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05INFP SORT B05-INFOS-PAI-C-OPER(3:10) ASC [4:10—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.B05INFOP
//SORTIN   DD DISP=SHR,DSN=&SRC.B05INFOP
//SORTOUT  DD DSN=&MIG.PAI32.B05INFP,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(4,10,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B05INFR
//*----------------------------------------------------------------
//SORT3203 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05INFR SORT B05-INFOS-REMB-C-OPER(3:10) ASC [4:10—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.B05INFOR
//SORTIN   DD DISP=SHR,DSN=&SRC.B05INFOR
//SORTOUT  DD DSN=&MIG.PAI32.B05INFR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(4,10,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FPAI30
//*----------------------------------------------------------------
//SORT3204 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFPAI30 SORT FPAI30-C-OPER(3:10) ASC [3:10—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FPAI30
//SORTIN   DD DISP=SHR,DSN=&MIG.PAI30.FPAI30
//SORTOUT  DD DSN=&MIG.PAI32.FPAI30,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(3,10,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL3202       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SPAI     DD DSN=&MIG.PAI32.PAI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL3203       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRPAI32,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRPAI32,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRPAI32,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZEPAI    DD DSN=&MIG.PAI32.EPAI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZB05INFP DD DSN=&MIG.PAI32.B05INFP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZB05INFR DD DSN=&MIG.PAI32.B05INFR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPAI30  DD DSN=&MIG.PAI32.FPAI30.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZPAI     DD DSN=&MIG.PAI32.PAI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRPAI32
//*----------------------------------------------------------------
//PPRPAI32 EXEC PGM=PPRPAI32,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRPAI32,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRPAI32,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRPAI32,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SEPAI    DD DISP=SHR,DSN=&MIG.PAI32.EPAI
//SB05INFP DD DISP=SHR,DSN=&MIG.PAI32.B05INFP
//SB05INFR DD DISP=SHR,DSN=&MIG.PAI32.B05INFR
//SFPAI30  DD DISP=SHR,DSN=&MIG.PAI32.FPAI30
//*--------------<FICHIERS CIBLES>---------------------------------
//SPAI     DD DSN=&MIG.PAI32.PAI,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZEPAI    DD DSN=&MIG.PAI32.EPAI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZB05INFP DD DSN=&MIG.PAI32.B05INFP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZB05INFR DD DSN=&MIG.PAI32.B05INFR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPAI30  DD DSN=&MIG.PAI32.FPAI30.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZPAI     DD DSN=&MIG.PAI32.PAI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRPAI33
//*----------------------------------------------------------------
//DEL3301       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SEPAI    DD DSN=&MIG.PAI33.EPAI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB05INFR DD DSN=&MIG.PAI33.B05INFR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPAI30  DD DSN=&MIG.PAI33.FPAI30,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER EPAI
//*----------------------------------------------------------------
//SORT3301 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SEPAI SORT E-PAI-IDPAIRET(1:10) ASC [56:10—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.PAI
//SORTIN   DD DISP=SHR,DSN=&MIG.PAI31.FPAI02
//SORTOUT  DD DSN=&MIG.PAI33.EPAI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(56,10,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B05INFR
//*----------------------------------------------------------------
//SORT3302 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05INFR SORT B05-INFOS-REMB-C-OPER(3:10) ASC [4:10—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.B05INFOR
//SORTIN   DD DISP=SHR,DSN=&SRC.B05INFOR
//SORTOUT  DD DSN=&MIG.PAI33.B05INFR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(4,10,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FPAI30
//*----------------------------------------------------------------
//SORT3303 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFPAI30 SORT FPAI30-C-OPER(3:10) ASC [3:10—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FPAI30
//SORTIN   DD DISP=SHR,DSN=&MIG.PAI30.FPAI30
//SORTOUT  DD DSN=&MIG.PAI33.FPAI30,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(3,10,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL3302       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SPAI     DD DSN=&MIG.PAI33.PAI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL3303       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRPAI33,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRPAI33,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRPAI33,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZEPAI    DD DSN=&MIG.PAI33.EPAI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZB05INFR DD DSN=&MIG.PAI33.B05INFR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPAI30  DD DSN=&MIG.PAI33.FPAI30.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZPAI     DD DSN=&MIG.PAI33.PAI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRPAI33
//*----------------------------------------------------------------
//PPRPAI33 EXEC PGM=PPRPAI33,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRPAI33,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRPAI33,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRPAI33,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SEPAI    DD DISP=SHR,DSN=&MIG.PAI33.EPAI
//SB05INFR DD DISP=SHR,DSN=&MIG.PAI33.B05INFR
//SFPAI30  DD DISP=SHR,DSN=&MIG.PAI33.FPAI30
//*--------------<FICHIERS CIBLES>---------------------------------
//SPAI     DD DSN=&MIG.PAI33.PAI,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZEPAI    DD DSN=&MIG.PAI33.EPAI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZB05INFR DD DSN=&MIG.PAI33.B05INFR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPAI30  DD DSN=&MIG.PAI33.FPAI30.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZPAI     DD DSN=&MIG.PAI33.PAI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRPAI34
//*----------------------------------------------------------------
//DEL3401       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRE    DD DSN=&MIG.PAI34.FCRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRE
//*----------------------------------------------------------------
//SORT3401 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRE SORT CRE-IDLIGCRE ASC [69:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.CPY0315
//SORTIN   DD DISP=SHR,DSN=&CIB.PCRE
//SORTOUT  DD DSN=&MIG.PAI34.FCRE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(69,20,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL3402       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SAFFAFF  DD DSN=&MIG.PAI34.AFFAFF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SAFFAFFR DD DSN=&MIG.PAI34.AFFAFFR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFREGNEG DD DSN=&CIB.FEXCL.FREGNEG,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL3403       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRPAI34,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRPAI34,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRPAI34,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCRE    DD DSN=&MIG.PAI34.FCRE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPEXT01 DD DSN=&MIG.PAI34.FPEXT01.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZAFFAFF  DD DSN=&MIG.PAI34.AFFAFF.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRPAI34
//*----------------------------------------------------------------
//PPRPAI34 EXEC PGM=PPRPAI34,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRPAI34,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRPAI34,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRPAI34,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//*TGESPAI  DD DISP=SHR,DSN=&TAB.TGESPAI
//*TC1RRIB  DD DISP=SHR,DSN=&TAB.TC1RRIB
//SFCRE    DD DISP=SHR,DSN=&MIG.PAI34.FCRE
//SFPEXT01 DD DUMMY
//*--------------<FICHIERS CIBLES>---------------------------------
//SAFFAFF  DD DSN=&MIG.PAI34.AFFAFF,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SAFFAFFR DD DSN=&MIG.PAI34.AFFAFFR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFREGNEG DD DSN=&CIB.FEXCL.FREGNEG,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCRE    DD DSN=&MIG.PAI34.FCRE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPEXT01 DD DSN=&MIG.PAI34.FPEXT01.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZAFFAFF  DD DSN=&MIG.PAI34.AFFAFF.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRPAI35
//*----------------------------------------------------------------
//DEL3501       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SIAFFAFF DD DSN=&MIG.PAI35.IAFFAFF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER IAFFAFF
//*----------------------------------------------------------------
//SORT3501 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SIAFFAFF SORT AFF-IDTECADH ASC [16:20—
//* SIAFFAFF SORT AFF-NOORDADS ASC [36:3—
//* SIAFFAFF SORT AFF-NOORDUEC ASC [39:5—
//* SIAFFAFF SORT AFF-ANEXE ASC [151:4—
//* SIAFFAFF SORT AFF-COPERREF ASC [155:3—
//* SIAFFAFF SORT AFF-FILLER22 ASC [129:22—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.AFF
//SORTIN01 DD DISP=SHR,DSN=&MIG.PAI34.AFFAFF
//SORTIN02 DD DISP=SHR,DSN=&MIG.PAI34.AFFAFFR
//SORTOUT  DD DSN=&MIG.PAI35.IAFFAFF,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,20,CH,A,
               36,3,CH,A,
               39,5,CH,A,
               151,4,CH,A,
               155,3,CH,A,
               129,22,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL3502       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SSAFFAFF DD DSN=&MIG.PAI35.SAFFAFF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL3503       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRPAI35,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRPAI35,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRPAI35,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZIAFFAFF DD DSN=&MIG.PAI35.IAFFAFF.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZSAFFAFF DD DSN=&MIG.PAI35.SAFFAFF.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRPAI35
//*----------------------------------------------------------------
//PPRPAI35 EXEC PGM=PPRPAI35,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRPAI35,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRPAI35,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRPAI35,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SIAFFAFF DD DISP=SHR,DSN=&MIG.PAI35.IAFFAFF
//*--------------<FICHIERS CIBLES>---------------------------------
//SSAFFAFF DD DSN=&MIG.PAI35.SAFFAFF,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZIAFFAFF DD DSN=&MIG.PAI35.IAFFAFF.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZSAFFAFF DD DSN=&MIG.PAI35.SAFFAFF.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRPAI36
//*----------------------------------------------------------------
//DEL3601       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SAFFAFF  DD DSN=&MIG.PAI36.AFFAFF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER AFFAFF
//*----------------------------------------------------------------
//SORT3601 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SAFFAFF SORT AFF-IDPAIRET ASC [49:20—
//* SAFFAFF SORT AFF-IDTECADH ASC [16:20—
//* SAFFAFF SORT AFF-NOORDADS ASC [36:3—
//* SAFFAFF SORT AFF-NOORDUEC ASC [39:5—
//* SAFFAFF SORT AFF-TYINSPAI ASC [44:1—
//* SAFFAFF SORT AFF-NOINSPAI ASC [45:4—
//* SAFFAFF SORT AFF-ANEXE ASC [151:4—
//* SAFFAFF SORT AFF-COPERREF ASC [155:3—
//* SAFFAFF SORT AFF-COORI ASC [101:2—
//* SAFFAFF SORT AFF-COOBJ ASC [103:2—
//* SAFFAFF SORT AFF-DTCRE ASC [194:10—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.AFF
//SORTIN   DD DISP=SHR,DSN=&MIG.PAI35.SAFFAFF
//SORTOUT  DD DSN=&MIG.PAI36.AFFAFF,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,20,CH,A,
               16,20,CH,A,
               36,3,CH,A,
               39,5,CH,A,
               44,1,CH,A,
               45,4,CH,A,
               151,4,CH,A,
               155,3,CH,A,
               101,2,CH,A,
               103,2,CH,A,
               194,10,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL3602       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SAFFFSC  DD DSN=&MIG.PAI36.AFFFSC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL3603       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRPAI36,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRPAI36,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRPAI36,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZAFFAFF  DD DSN=&MIG.PAI36.AFFAFF.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZAFFFSC  DD DSN=&MIG.PAI36.AFFFSC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRPAI36
//*----------------------------------------------------------------
//PPRPAI36 EXEC PGM=PPRPAI36,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRPAI36,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRPAI36,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRPAI36,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SAFFAFF  DD DISP=SHR,DSN=&MIG.PAI36.AFFAFF
//*--------------<FICHIERS CIBLES>---------------------------------
//SAFFFSC  DD DSN=&MIG.PAI36.AFFFSC,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZAFFAFF  DD DSN=&MIG.PAI36.AFFAFF.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZAFFFSC  DD DSN=&MIG.PAI36.AFFFSC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRPAI37
//*----------------------------------------------------------------
//DEL3701       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SAFFFSC  DD DSN=&MIG.PAI37.AFFFSC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER AFFFSC
//*----------------------------------------------------------------
//SORT3701 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SAFFFSC SORT AFF-IDPAIRET ASC [49:20—
//* SAFFFSC SORT AFF-IDTECADH ASC [16:20—
//* SAFFFSC SORT AFF-NOORDADS ASC [36:3—
//* SAFFFSC SORT AFF-NOORDUEC ASC [39:5—
//* SAFFFSC SORT AFF-ANEXE ASC [151:4—
//* SAFFFSC SORT AFF-COPERREF ASC [155:3—
//* SAFFFSC SORT AFF-FILLER22 ASC [129:22—
//* SAFFFSC SORT AFF-DTCRE ASC [194:10—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.AFF
//SORTIN   DD DISP=SHR,DSN=&MIG.PAI36.AFFFSC
//SORTOUT  DD DSN=&MIG.PAI37.AFFFSC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,20,CH,A,
               16,20,CH,A,
               36,3,CH,A,
               39,5,CH,A,
               151,4,CH,A,
               155,3,CH,A,
               129,22,CH,A,
               194,10,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL3702       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SAFFFCU  DD DSN=&MIG.PAI37.AFFFCU,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL3703       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRPAI37,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRPAI37,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRPAI37,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZAFFFSC  DD DSN=&MIG.PAI37.AFFFSC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZAFFFCU  DD DSN=&MIG.PAI37.AFFFCU.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRPAI37
//*----------------------------------------------------------------
//PPRPAI37 EXEC PGM=PPRPAI37,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRPAI37,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRPAI37,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRPAI37,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SAFFFSC  DD DISP=SHR,DSN=&MIG.PAI37.AFFFSC
//*--------------<FICHIERS CIBLES>---------------------------------
//SAFFFCU  DD DSN=&MIG.PAI37.AFFFCU,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZAFFFSC  DD DSN=&MIG.PAI37.AFFFSC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZAFFFCU  DD DSN=&MIG.PAI37.AFFFCU.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRPAI38
//*----------------------------------------------------------------
//DEL3801       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SAFFFCU  DD DSN=&MIG.PAI38.AFFFCU,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER AFFFCU
//*----------------------------------------------------------------
//SORT3801 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SAFFFCU SORT AFF-IDPAIRET ASC [49:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.AFF
//SORTIN   DD DISP=SHR,DSN=&MIG.PAI37.AFFFCU
//SORTOUT  DD DSN=&MIG.PAI38.AFFFCU,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,20,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL3802       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SPAIFCU  DD DSN=&MIG.PAI38.PAIFCU,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL3803       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRPAI38,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRPAI38,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRPAI38,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZAFFFCU  DD DSN=&MIG.PAI38.AFFFCU.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZPAIFCU  DD DSN=&MIG.PAI38.PAIFCU.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRPAI38
//*----------------------------------------------------------------
//PPRPAI38 EXEC PGM=PPRPAI38,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRPAI38,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRPAI38,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRPAI38,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//*TGESPAI  DD DISP=SHR,DSN=&TAB.TGESPAI
//*TC1RRIB  DD DISP=SHR,DSN=&TAB.TC1RRIB
//SAFFFCU  DD DISP=SHR,DSN=&MIG.PAI38.AFFFCU
//*--------------<FICHIERS CIBLES>---------------------------------
//SPAIFCU  DD DSN=&MIG.PAI38.PAIFCU,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZAFFFCU  DD DSN=&MIG.PAI38.AFFFCU.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZPAIFCU  DD DSN=&MIG.PAI38.PAIFCU.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRPAI39
//*----------------------------------------------------------------
//DEL3901       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SAFFFCU1 DD DSN=&MIG.PAI39.AFFFCU1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SAFFFCU2 DD DSN=&MIG.PAI39.AFFFCU2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER AFFFCU1
//*----------------------------------------------------------------
//SORT3901 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SAFFFCU1 SORT FCU1-AFF-IDPAIRET ASC [49:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.AFF
//SORTIN   DD DISP=SHR,DSN=&MIG.PAI31.AFFFCU
//SORTOUT  DD DSN=&MIG.PAI39.AFFFCU1,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER AFFFCU2
//*----------------------------------------------------------------
//SORT3902 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SAFFFCU2 SORT FCU2-AFF-IDPAIRET ASC [49:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.AFF
//SORTIN   DD DISP=SHR,DSN=&MIG.PAI37.AFFFCU
//SORTOUT  DD DSN=&MIG.PAI39.AFFFCU2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,20,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL3902       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SAFFFCU  DD DSN=&MIG.PAI39.AFFFCU,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL3903       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRPAI39,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRPAI39,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRPAI39,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZAFFFCU1 DD DSN=&MIG.PAI39.AFFFCU1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZAFFFCU2 DD DSN=&MIG.PAI39.AFFFCU2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZAFFFCU  DD DSN=&MIG.PAI39.AFFFCU.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRPAI39
//*----------------------------------------------------------------
//PPRPAI39 EXEC PGM=PPRPAI39,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRPAI39,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRPAI39,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRPAI39,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SAFFFCU1 DD DISP=SHR,DSN=&MIG.PAI39.AFFFCU1
//SAFFFCU2 DD DISP=SHR,DSN=&MIG.PAI39.AFFFCU2
//*--------------<FICHIERS CIBLES>---------------------------------
//SAFFFCU  DD DSN=&MIG.PAI39.AFFFCU,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZAFFFCU1 DD DSN=&MIG.PAI39.AFFFCU1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZAFFFCU2 DD DSN=&MIG.PAI39.AFFFCU2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZAFFFCU  DD DSN=&MIG.PAI39.AFFFCU.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRPAI40
//*----------------------------------------------------------------
//DEL4001       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SAFFFCU  DD DSN=&MIG.PAI40.AFFFCU,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SAFFFSC  DD DSN=&MIG.PAI40.AFFFSC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SAFFAFF  DD DSN=&MIG.PAI40.AFFAFF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SAFFTOTR DD DSN=&MIG.PAI40.AFFTOTR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER AFFFCU
//*----------------------------------------------------------------
//SORT4001 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SAFFFCU SORT FCU-AFF-IDPAIRET ASC [49:20—
//* SAFFFCU SORT FCU-AFF-NOINSPAI ASC [45:4—
//* SAFFFCU SORT FCU-AFF-IDLIGCREAFF ASC [105:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.AFF
//SORTIN   DD DISP=SHR,DSN=&MIG.PAI39.AFFFCU
//SORTOUT  DD DSN=&MIG.PAI40.AFFFCU,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE),
//            LRECL=216,RECFM=FB
//SYSIN    DD *
  SORT FIELDS=(49,20,CH,A,
               45,4,CH,A,
               105,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER AFFFSC
//*----------------------------------------------------------------
//SORT4002 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SAFFFSC SORT FSC-AFF-IDPAIRET ASC [49:20—
//* SAFFFSC SORT FSC-AFF-NOINSPAI ASC [45:4—
//* SAFFFSC SORT FSC-AFF-IDLIGCREAFF ASC [105:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.AFF
//SORTIN   DD DISP=SHR,DSN=&MIG.PAI36.AFFFSC
//SORTOUT  DD DSN=&MIG.PAI40.AFFFSC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,20,CH,A,
               45,4,CH,A,
               105,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER AFFAFF
//*----------------------------------------------------------------
//SORT4003 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SAFFAFF SORT AFF-AFF-IDPAIRET ASC [49:20—
//* SAFFAFF SORT AFF-AFF-NOINSPAI ASC [45:4—
//* SAFFAFF SORT AFF-AFF-IDLIGCREAFF ASC [105:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.AFF
//SORTIN   DD DISP=SHR,DSN=&MIG.PAI35.SAFFAFF
//SORTOUT  DD DSN=&MIG.PAI40.AFFAFF,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,20,CH,A,
               45,4,CH,A,
               105,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER AFFTOTR
//*----------------------------------------------------------------
//SORT4004 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SAFFTOTR SORT AFFR-AFF-IDPAIRET ASC [49:20—
//* SAFFTOTR SORT AFFR-AFF-TYINSPAI ASC [44:1—
//* SAFFTOTR SORT AFFR-AFF-FILLER22 ASC [129:22—
//* SAFFTOTR SORT AFFR-AFF-IDLIGCREAFF ASC [105:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.AFF
//SORTIN01 DD DISP=SHR,DSN=&MIG.PAI39.AFFFCU
//SORTIN02 DD DISP=SHR,DSN=&MIG.PAI36.AFFFSC
//SORTIN03 DD DISP=SHR,DSN=&MIG.PAI35.SAFFAFF
//SORTOUT  DD DSN=&MIG.PAI40.AFFTOTR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,20,CH,A,
               44,1,CH,D,
               129,22,CH,A,
               105,20,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL4002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SAFF     DD DSN=&CIB.AFF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL4003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRPAI40,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRPAI40,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRPAI40,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZAFFFCU  DD DSN=&MIG.PAI40.AFFFCU.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZAFFFSC  DD DSN=&MIG.PAI40.AFFFSC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZAFFAFF  DD DSN=&MIG.PAI40.AFFAFF.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZAFFTOTR DD DSN=&MIG.PAI40.AFFTOTR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZAFF     DD DSN=&MIG.PAI40.AFF.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRPAI40
//*----------------------------------------------------------------
//PPRPAI40 EXEC PGM=PPRPAI40,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRPAI40,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRPAI40,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRPAI40,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SAFFFCU  DD DISP=SHR,DSN=&MIG.PAI40.AFFFCU
//SAFFFSC  DD DISP=SHR,DSN=&MIG.PAI40.AFFFSC
//SAFFAFF  DD DISP=SHR,DSN=&MIG.PAI40.AFFAFF
//SAFFTOTR DD DISP=SHR,DSN=&MIG.PAI40.AFFTOTR
//*--------------<FICHIERS CIBLES>---------------------------------
//SAFF     DD DSN=&CIB.AFF,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZAFFFCU  DD DSN=&MIG.PAI40.AFFFCU.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZAFFFSC  DD DSN=&MIG.PAI40.AFFFSC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZAFFAFF  DD DSN=&MIG.PAI40.AFFAFF.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZAFFTOTR DD DSN=&MIG.PAI40.AFFTOTR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZAFF     DD DSN=&MIG.PAI40.AFF.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRPAI41
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL4102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SPAI     DD DSN=&CIB.PAI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL4103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRPAI41,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRPAI41,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRPAI41,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZPAIDIS  DD DSN=&MIG.PAI41.PAIDIS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZPAIRBT  DD DSN=&MIG.PAI41.PAIRBT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZPAIFCU  DD DSN=&MIG.PAI41.PAIFCU.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZPAI     DD DSN=&MIG.PAI41.PAI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRPAI41
//*----------------------------------------------------------------
//PPRPAI41 EXEC PGM=PPRPAI41,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRPAI41,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRPAI41,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRPAI41,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//PAIDIS   DD DISP=SHR,DSN=&MIG.PAI32.PAI
//PAIRBT   DD DISP=SHR,DSN=&MIG.PAI33.PAI
//PAIFCU   DD DISP=SHR,DSN=&MIG.PAI38.PAIFCU
//*--------------<FICHIERS CIBLES>---------------------------------
//SPAI     DD DSN=&CIB.PAI,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZPAIDIS  DD DSN=&MIG.PAI41.PAIDIS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZPAIRBT  DD DSN=&MIG.PAI41.PAIRBT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZPAIFCU  DD DSN=&MIG.PAI41.PAIFCU.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZPAI     DD DSN=&MIG.PAI41.PAI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRPAI50
//*----------------------------------------------------------------
//DEL5001       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//PAI      DD DSN=&MIG.PAI50.PAI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//AFF      DD DSN=&MIG.PAI50.AFF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER PAI
//*----------------------------------------------------------------
//SORT5001 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* PAI SORT PAI-IDSOCENC ASC [107:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.PAI
//SORTIN   DD DISP=SHR,DSN=&CIB.PAI
//SORTOUT  DD DSN=&MIG.PAI50.PAI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(107,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER AFF
//*----------------------------------------------------------------
//SORT5002 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* AFF SORT AFF-IDPAIRET ASC [49:20—
//* AFF SORT AFF-NOORDFIN2 ASC [212:5—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.AFF
//SORTIN   DD DISP=SHR,DSN=&CIB.AFF
//SORTOUT  DD DSN=&MIG.PAI50.AFF,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,20,CH,A,
               212,5,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL5002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SPAIMNT  DD DSN=&CIB.PAIMNT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL5003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRPAI50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRPAI50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRPAI50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZPAI     DD DSN=&MIG.PAI50.PAI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZAFF     DD DSN=&MIG.PAI50.AFF.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZPAIMNT  DD DSN=&MIG.PAI50.PAIMNT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRPAI50
//*----------------------------------------------------------------
//PPRPAI50 EXEC PGM=PPRPAI50,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRPAI50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRPAI50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRPAI50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//PAI      DD DISP=SHR,DSN=&MIG.PAI50.PAI
//AFF      DD DISP=SHR,DSN=&MIG.PAI50.AFF
//*--------------<FICHIERS CIBLES>---------------------------------
//SPAIMNT  DD DSN=&CIB.PAIMNT,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZPAI     DD DSN=&MIG.PAI50.PAI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZAFF     DD DSN=&MIG.PAI50.AFF.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZPAIMNT  DD DSN=&MIG.PAI50.PAIMNT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//* FIN DU JCL DE MIGRATION
