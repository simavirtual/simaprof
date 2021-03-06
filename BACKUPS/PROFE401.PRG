/* SIMA - CONTABILIDAD PROFESORES

MODULO      : RECUPERACIONES
SUBMODULO...: RECUPER                MODULO No. 401

**************************************************************************
* TITULO..: DEFINIR RECUPERACIONES                                       *
**************************************************************************

AUTOR: Nelson Fern�ndez G�mez       FECHA DE CREACION: SEP 07/2001 VIE A
       Colombia, Bucaramanga        INICIO: 11:50 AM   SEP 07/2001 VIE


OBJETIVOS:

1- Permite la definir las recuperaciones pendientes del estudiante

2- Retorna NIL

*------------------------------------------------------------------------*
*                              PROGRAMA                                  *
*------------------------------------------------------------------------*/

FUNCTION Profe_401(aP1,aP2,aP3)

*>>>>DESCRIPCION DE PARAMETROS
/*     aP1                                  // Parametros Generales
       aP2                                  // Parametros Generales
       aP3                                  // Parametros Generales */
*>>>>FIN DESCRIPCION DE PARAMETROS

*>>>>DECLARACION DE VARIABLES
       #INCLUDE "CAMPOS\ARC-SPR.PRG"       // Archivos del Sistema

       LOCAL cSavPan := ''                  // Salvar Pantalla
       LOCAL cSavLin := ''                  // Salvar Linea
       LOCAL lHayErr := .F.                 // .T. Hay Error
     *�Variables generales

       LOCAL cSavCab := ''                  // Salvar Encabezado
       LOCAL       i := 0                   // Contador

       LOCAL nPerAct := 0                   // Periodo Actual

       LOCAL nFilSup := 0                   // Columna Superior
       LOCAL nColSup := 0                   // Columna Superior
       LOCAL nInfFil := 0                   // Fila Inferior
       LOCAL nInfCol := 0                   // Columna Inferior

       LOCAL lOtrFil := .F.                 // .T. Otra Fila
       LOCAL nNroFil := 0                   // Fila de lectura
       LOCAL nNroCol := 1                   // Columna de lectura
       LOCAL nFilCab := 0                   // File de Encabezado

       LOCAL nColIni := 0                   // Columna Inicial
       LOCAL nFilFin := 19                   // Fila Final de Control

       LOCAL lIndNiv := .F.                 // .T. Indicadores de Nivel
       LOCAL cEvaInd := ''                  // Evaluar el Indicador
       LOCAL lNotNum := .F.                 // .T. Nota N�merica .F. Nota Cualitativa
       LOCAL cValNot := ''                  // Validaci�n de la nota
       LOCAL lHayRec := .F.                 // .T. Hay Recuperaciones
       LOCAL  cJvfSi := ""                  // Juicios Valorativos Si Aprobados
       LOCAL  cJvfNo := ""                  // Juicios Valorativos No Aprobados

       LOCAL nTotPer := 5                   // Total de Periodos
       LOCAL nNroPer := 0                   // N�mero del periodo

       LOCAL aNroCol := {}                  // N�meros de Columnas
       LOCAL aLenNom := {}                  // Longitud de nombres
       LOCAL nLenNom := 0                   // Longitud del nombre
       LOCAL nNroInd := 20                   // N�mero de indicadores
       LOCAL nNroRec := 0                   // N�mero de Recuperaciones
       LOCAL nIndMin := 0                   // Indicador M�nimo para control
       LOCAL nNroLog := 0                   // N�mero de logro escogido
       LOCAL lOtrEst := .F.                 // .T. Otro Estudiante

       LOCAL cNalias := ''                  // Alias del Maestro
       LOCAL nLenCod := 6                   // Longitud del C�digo
       LOCAL nLenNtI := 8                   // Longitud del C�digo de la Nota de la Recuperaci�n del Indicador
       LOCAL cMsgTxt := ''                  // Mensaje Temporal
       LOCAL lHayNot := .F.                 // .T. Hay Nota

       LOCAL cLogros := ''                  // Logros aplicados
       LOCAL cCodLog := ''                  // C�digo del logro

       LOCAL cLogNot := ''                  // Logros de Notas
       LOCAL cNotInd := ''                  // Nota de los Indicadores en Notas
       LOCAL cNotDef := ''                  // Nota Definitiva en Notas
       LOCAL cNotRec := ''                  // Nota Recuperaci�n en Notas

       LOCAL cLogNoP := ''                  // Logros de Notas del Profesor
       LOCAL cNtINoP := ''                  // Nota de los Indicadores en Profesor
       LOCAL cNoPDef := ''                  // Nota Definitiva en Profesor
       LOCAL cNoPRec := ''                  // Nota Recuperaci�n en Notas

       LOCAL cRecupe := ''                  // Indicadores por Recuperar
       LOCAL nPosInd := ''                  // Posici�n del Indicador
       LOCAL lRecNot := .F.                 // .T. Recuperar Nota

       LOCAL aGenLog := {}                  // Campos Generales
       LOCAL aCamLog := {}                  // Campos del Log
       LOCAL cTxtLog := ''                  // Texto del Log
       LOCAL cHorIni := ''                  // Hora de Inicio
       LOCAL lHayLog := .F.                 // .T. Hay Log
       LOCAL lLeeRec := .T.                 // .T. Leer Recuperaci�n
     *�Variables del Log

       LOCAL GetList := {}                  // Variable del Sistema
     *�Variables espec�ficas 

       LOCAL cCodigoTpr := ''               // C�digo del Profesor
       LOCAL cCodigoTes := ''               // C�digo del estudiante
       LOCAL cNombreTes := ''               // Nombre del estudiante
       LOCAL cCodigoTma := ''               // C�digo de la Materia
       LOCAL cCodigoTgr := ''               // C�digo del grupo
       LOCAL cCodigoTni := ''               // C�digo del nivel
       LOCAL nTipCarTca := 0                // Tipo de carga

       LOCAL cCamIndNot := ''               // Campo Indicadores en Notas
       LOCAL cCamNtINot := ''               // Campo de Nota de Indicadores
       LOCAL cCamNotDef := ''               // Campo Notas Definitivas
       LOCAL cCamNotRec := ''               // Campo Notas de Recuperaci�n

       LOCAL cCamIndNoP := ''               // Campo Indicadores en Notas Profesor
       LOCAL cCamNtINop := ''               // Campo de Nota de Indicadores
       LOCAL cCamNoPDef := ''               // Campo Nota Definitvia en Notas Profesor
       LOCAL cCamNoPRec := ''               // Campo Nota Recuperaci�n en Notas Profesor

       LOCAL lMatSelTma := .F.              // .T. Materia Selectiva

       LOCAL cIndic1Tlo := ''               // Indicadores Antes
       LOCAL cDefRe1Tlo := ''               // Recuperaci�n Antes
     *�Variables de Campo
*>>>>FIN DECLARACION DE VARIABLES

*>>>>DECLARACION PARAMETROS GENERALES
       LOCAL lShared := xPrm(aP1,'lShared') // .T. Sistema Compartido
       LOCAL nModCry := xPrm(aP1,'nModCry') // Modo de Protecci�n
       LOCAL cCodSui := xPrm(aP1,'cCodSui') // C�digo del Sistema
       LOCAL cNomSis := xPrm(aP1,'cNomSis') // Nombre del Sistema
     *�Detalles del Sistema

       LOCAL cEmpPal := xPrm(aP1,'cEmpPal') // Nombre de la Empresa principal
       LOCAL cNitEmp := xPrm(aP1,'cNitEmp') // Nit de la Empresa
       LOCAL cNomEmp := xPrm(aP1,'cNomEmp') // Nombre de la Empresa
       LOCAL cNomSec := xPrm(aP1,'cNomSec') // Nombre de la Empresa Secundario
       LOCAL cCodEmp := xPrm(aP1,'cCodEmp') // C�digo de la Empresa
     *�Detalles de la Empresa

       LOCAL cNomUsr := xPrm(aP1,'cNomUsr') // Nombre del Usuario
       LOCAL cAnoUsr := xPrm(aP1,'cAnoUsr') // A�o del usuario
       LOCAL cAnoSis := xPrm(aP1,'cAnoSis') // A�o del sistema
       LOCAL cPatSis := xPrm(aP1,'cPatSis') // Path del sistema
     *�Detalles del Usuario

       LOCAL PathW01 := xPrm(aP1,'PathW01') // Sitio del Sistema No.01
       LOCAL PathW02 := xPrm(aP1,'PathW02') // Sitio del Sistema No.02
       LOCAL PathW03 := xPrm(aP1,'PathW03') // Sitio del Sistema No.03
       LOCAL PathW04 := xPrm(aP1,'PathW04') // Sitio del Sistema No.04
       LOCAL PathW05 := xPrm(aP1,'PathW05') // Sitio del Sistema No.05
       LOCAL PathW06 := xPrm(aP1,'PathW06') // Sitio del Sistema No.06
       LOCAL PathW07 := xPrm(aP1,'PathW07') // Sitio del Sistema No.07
       LOCAL PathW08 := xPrm(aP1,'PathW08') // Sitio del Sistema No.08
       LOCAL PathW09 := xPrm(aP1,'PathW09') // Sitio del Sistema No.09
       LOCAL PathW10 := xPrm(aP1,'PathW10') // Sitio del Sistema No.10
     *�Sitios del Sistema

       LOCAL PathUno := xPrm(aP1,'PathUno') // Path de Integraci�n Uno
       LOCAL PathDos := xPrm(aP1,'PathDos') // Path de Integraci�n Dos
       LOCAL PathTre := xPrm(aP1,'PathTre') // Path de Integraci�n Tres
       LOCAL PathCua := xPrm(aP1,'PathCua') // Path de Integraci�n Cuatro
     *�Path de Integraci�n

       LOCAL cMaeAlu := xPrm(aP1,'cMaeAlu') // Maestros habilitados
       LOCAL cMaeAct := xPrm(aP1,'cMaeAct') // Maestro Activo
       LOCAL cJorTxt := xPrm(aP1,'cJorTxt') // Jornada escogida
     *�Detalles Acad�micos

       LOCAL nFilPal := xPrm(aP1,'nFilPal') // Fila Inferior Men� principal
       LOCAL nFilInf := xPrm(aP1,'nFilInf') // Fila Inferior del SubMen�
       LOCAL nColInf := xPrm(aP1,'nColInf') // Columna Inferior del SubMen�
     *�Detalles Tecnicos
*>>>>FIN DECLARACION PARAMETROS GENERALES

*>>>>DECLARACION DE PARAMETROS ESPECIFICOS
       LOCAL cCodPro := xPrm(aP1,'cCodPro') // C�digo del Profesor
       LOCAL cPatPro := xPrm(aP1,'cPatPro') // Path del Profesor
       LOCAL cNomPro := xPrm(aP1,'cNomPro') // Nombre del Profesor
       CloseAll()
*>>>>DECLARACION DE PARAMETROS ESPECIFICOS

*>>>>SELECION DE LAS AREAS DE TRABAJO
       IF !lUseMae(lShared,PathUno+'\'+cPatSis,cMaeAct,cAnoSis)  .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+;
		       FilePro+cAnoSis+ExtFile,'PRO',NIL,lShared) .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FileMat,'MAT',NIL,lShared)                .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FileNiv+cAnoSis+ExtFile,'NIV',NIL,lShared) .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FileInd+cMaeAct+cAnoSis+ExtFile,'IND',;
		       PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FNtxInd+cMaeAct+cAnoSis+cExtNtx,lShared)   .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FileCla,'CLA',NIL,lShared)                 .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FileEva,'EVA',NIL,lShared)                 .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FileJvf,'JVF',NIL,lShared)                 .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FConTbl+cMaeACt+ExtFile,'TCO',NIL,lShared) .OR.;
	  !lUseDbf(.T.,cPatSis+'\'+;
		       fSprAno+cAnoUsr+ExtFile,'PRA',NIL,lShared) .OR.;
	  !lUseDbf(.T.,PathSis+'\'+fSimSpr,'SPR',NIL,lShared)     .OR.;
	  !lUseDbf(.T.,cPatPro+'\'+FileLog,'LOG',NIL,lShared)     .OR.;
	  !lUseDbf(.T.,cPatSis+'\'+cMaeAct+'\'+;
		       FProTbl+cMaeAct+ExtFile,'TPR',NIL,lShared) .OR.;
	  !lUseDbf(.T.,cPatPro+'\'+;
		       FCarPro+cAnoSis+ExtFile,'CAP',NIL,lShared)
	  cError('ABRIENDO ARCHIVOS')
	  CLOSE ALL
	  RETURN NIL
       ENDIF
*>>>>FIN SELECION DE LAS AREAS DE TRABAJO

*>>>>VALIDACION DE CONTENIDOS DE ARCHIVOS
       lHayErr := .T.
       SELECT &cMaeAct
       DO CASE
       CASE RECCOUNT() == 0
	    cError('NO EXISTEN ESTUDIANTS GRABADOS')

       CASE PRO->(RECCOUNT()) == 0
	    cError('NO EXISTEN PROFESORES GRABADOS')

       CASE MAT->(RECCOUNT()) == 0
	    cError('NO EXISTEN MATERIAS GRABADAS')

       CASE NIV->(RECCOUNT()) == 0
	    cError('NO EXISTEN NIVELES GRABADOS')

       CASE IND->(RECCOUNT()) == 0
	    cError('NO EXISTEN INDICADORES DE LA CONTABILIDAD')

       CASE CLA->(RECCOUNT()) == 0
	    cError('NO EXISTEN LAS CLASES DE LOGROS')

       CASE EVA->(RECCOUNT()) == 0
	    cError('NO EXISTEN LOS CODIGOS DE EVALUACION')

       CASE TCO->(RECCOUNT()) == 0
	    cError('NO EXISTEN CONFIGURACION PARA EL NIVEL DE LA CONTABILIDAD')

       CASE SPR->(RECCOUNT()) == 0
	    cError('NO EXISTE CONFIGURACION DEL SISTEMA')

       CASE PRA->(RECCOUNT()) == 0
	    cError('NO EXISTE CONFIGURACION DEL A�O')

       CASE CAP->(RECCOUNT()) == 0
	    cError('NO EXISTE CARGA ACADEMICA DEL PROFESOR')

       OTHERWISE
	    lHayErr :=.F.
       ENDCASE

       IF lHayErr
	  CLOSE ALL
	  RETURN NIL
       ENDIF
*>>>>FIN VALIDACION DE CONTENIDOS DE ARCHIVOS

*>>>>LOCALIZACION DEL PROFESOR
       cCodigoTpr := SUBS(cPatPro,LEN(cPatPro)-5,6)
       IF !lLocCodigo('cCodigoPro','PRO',cCodigoTpr,'DEL PROFESOR')
	  CloseAll()
	  RETURN NIL
       ENDIF
*>>>>FIN LOCALIZACION DEL PROFESOR

*>>>>SELECCION DE LA MATERIA DEL GRUPO
       cJvfSi := cJvfSi()
       cJvfNo := cJvfNo()

       SELECT CAP
       IF CAP->(RECCOUNT()) # 0

**********INICIALIZACION DE LAS COORDENADAS
	    nFilSup := nFilInf+2
	    nColSup := nColInf
	    IF nFilSup+RECCOUNT() > 22
	       nInfFil := 21
	    ELSE
	       nInfFil := nFilSup + RECCOUNT()-1
	    ENDIF
	    nInfCol := nColSup+LEN(CAP->cNombreMat)+2
**********FIN INICIALIZACION DE LAS COORDENADAS

**********BROWSE DE SELECCION
	    DO WHILE .T.

*--------------SELECCION DEL REGISTRO
		 SELECT CAP
		 IF nBrowseDbf(nFilSup,02,nInfFil,78,;
			      {||SUBS(CAP->cCodigoCar,5,4)+' '+;
			       Materia()}) == 0
		    CLOSE ALL
		    RETURN NIL
		 ENDIF
		 lLocCodigo('cCodigoMat','MAT',SUBS(CAP->cCodigoCar,1,4))
		 lMatSelTma := MAT->lMatSelMat
		 IF MAT->lNoRecuMat
		    cError('LA MATERIA ES NO RECUPERABLE')
		    CLOSE ALL
		    RETURN NIL
		 ENDIF
*--------------FIN SELECCION DEL REGISTRO

*--------------PREGUNTA DE DECISION
		 cMsgTxt := 'ESCOGIO '+ALLTRIM(CAP->cNombreMat)+' '+;
			    'DEL GRUPO: '+SUBS(CAP->cCodigoCar,5,4)
		 cMsgTxt := SUBS(cMsgTxt,1,50)

		 IF lPregunta(cMsgTxt+' DESEA CONTINUAR? Si No')
		    EXIT
		 ENDIF
*--------------FIN PREGUNTA DE DECISION

	    ENDDO
**********FIN BROWSE DE SELECCION

       ENDIF
       cCodigoTgr := SUBS(CAP->cCodigoCar,5,4)
       cCodigoTni := SUBS(cCodigoTgr,1,2)
       cCodigoTma := SUBS(CAP->cCodigoCar,1,4)
       nTipCarTca := CAP->nTipCarCar
*>>>>FIN SELECCION DE LA MATERIA DEL GRUPO

/*
*>>>>LECTURA DEL NUMERO DEL BIMESTRE
       nNroPer := nLeeNroPer(nFilInf+1,nColInf,nFilPal,nTotPer)

       lHayErr := .T.
       DO CASE
       CASE nNroPer == 0
       CASE nNroPer == TCO->nPerActTbl .AND. TCO->nRecPerTbl == 0
	    cError('NO SE PERMITEN RECUPERACIONES DEL PERIODO ACTUAL')

       CASE nNroPer > TCO->nPerActTbl
	    cError('NO SE PERMITEN RECUPERACIONES DE PERIODOS POSTERIORES AL PERIODO ACTUAL')

       OTHERWISE
	    lHayErr :=.F.
       ENDCASE
       IF lHayErr
	  CloseAll()
	  RETURN NIL
       ENDIF
*>>>>FIN ANALISIS PARA LA SALIDA DEL PROGRAMA
*/

*>>>>SELECION DE LAS AREAS DE TRABAJO
       IF !lUseDbf(.T.,cPatPro+'\'+;
		       CAP->cCodigoCar+ExtFile,'NOP',NIL,lShared)

	  cError('ABRIENDO EL ARCHIVO DE NOTAS DEL DEL PROFESOR '+;
		 CAP->cCodigoCar)
	  CloseAll()
	  RETURN NIL
       ENDIF
*>>>>FIN SELECION DE LAS AREAS DE TRABAJO

*>>>>IMPRESION DE LOS ENCABEZADOS
       nNroFil := nMarco(nFilPal+1,;
			 ALLTRIM(MAT->cNombreMat),22,'�')
       @ nNroFil,01 SAY 'CODIGO'
       @ nNroFil,08 SAY 'NOMBRE'
       nFilCab := nNroFil
       cSavCab := SAVESCREEN(nFilCab,0,nFilCab,79)
       nNroFil++
*>>>>FIN IMPRESION DE LOS ENCABEZADOS

*>>>>LECTURA DE LA CONFIGURACION DEL NIVEL DE LA CONTABILIDAD
       lNotNum := IF(SUBS(TCO->cNotNumTbl,nNroPer,1)='1',.T.,.F.)
       lIndNiv := TCO->lIndNivTbl
       lRecNot := TCO->lRecNotTbl
       IF lNotNum
	  cValNot := ALLTRIM(TCO->cValNumTbl)+SPACE(01)
       ELSE
	  cValNot := ALLTRIM(TCO->cValNotTbl)+SPACE(01)
       ENDIF
       cEvaInd := SUBS(TCO->cEvaIndTbl,nNroPer,1)
       cSavLin := SAVESCREEN(21,0,22,79)
*>>>>FIN LECTURA DE LA CONFIGURACION DEL NIVEL DE LA CONTABILIDAD

*>>>>GRABACION DE LAS NOTAS DE LOS ESTUDIANTES
       lOtrEst := .T.
       DO WHILE lOtrEst

**********LECTURA DEL CODIGO DEL ESTUDIANTE
	    cHorIni := TIME()
	    RESTSCREEN(nFilCab,0,nFilCab,79,cSavCab)
	    cCodigoTes := cLeeCodEst(nNroFil,01,cMaeAct,@cNalias)
	    RESTSCREEN(21,0,22,79,cSavLin)
	    IF EMPTY(cCodigoTes) .OR. cNalias == NIL
	       lOtrEst := .F.
	       LOOP
	    ENDIF
**********FIN LECTURA DEL CODIGO DEL ESTUDIANTE

**********RECUPERACIONES POR PERIODOS
	    nPerAct := IF(EMPTY(PRO->nNroPerPro),TCO->nPerActTbl,PRO->nNroPerPro)
	    lHayRec := .F.
	    FOR nNroPer := 1 TO IF(EMPTY(TCO->nRecPerTbl),;
				   nPerAct-1,nPerAct)

*===============CALCULO DEL CAMPO DE INDICADORES DE NOTAS
		  IF !lMatSelTma
		     cCamIndNot := cCamJuiNot(lShared,cAnoUsr,cMaeAct,;
					      PathUno+'\'+cPatSis,nNroPer,;
					      cCodigoTgr,cCodigoTma,NIL,;
					      @cCamNotDef,@cCamNotRec,;
					      @cCamNtINot)

		     IF EMPTY(cCamIndNot) .OR.;
			EMPTY(cCamNotDef) .OR. EMPTY(cCamNotRec)
			CloseAll()
			RETURN NIL
		     ENDIF
		  ENDIF
		  cCamIndNoP := 'NOP->cIndic'+STR(nNroPer,1,0)+'NoP'
		  cCamNtINoP := 'NOP->cIndnt'+STR(nNroPer,1,0)+'NoP'
		  cCamNoPDef := 'NOP->cDefin'+STR(nNroPer,1,0)+'NoP'
		  cCamNoPRec := 'NOP->cDefRe'+STR(nNroPer,1,0)+'NoP'
*===============FIN CALCULO DEL CAMPO DE INDICADORES DE NOTAS

*===============CALCULO DEL CAMPO DE INDICADORES DE NOTAS
		  IF lMatSelTma
		     CloseDbf('PLA')
		     CloseDbf('NOT')
		     cCamIndNot := cCamJuiNot(lShared,cAnoUsr,cMaeAct,;
					      PathUno+'\'+cPatSis,;
					      nNroPer,&cNalias->cCodigoGru,;
					      cCodigoTma,.T.,@cCamNotDef,;
					      @cCamNotRec,@cCamNtINot)
		     IF EMPTY(cCamIndNot)
			CloseAll()
			RETURN NIL
		     ENDIF
		  ENDIF
*===============FIN CALCULO DEL CAMPO DE INDICADORES DE NOTAS

*===============LOCALIZACION DEL ESTUDIANTE EN NOTAS
		  cLogNot := ''
		  cNotDef := ''
		  cNotRec := ''
		  IF lLocCodigo('cCodigoEst','NOT',cCodigoTes)

		     cLogNot := &cCamIndNot

		     cNotDef := SUBS(&cCamNotDef,nNroPer*4-3,4)
		     cNotRec := SUBS(&cCamNotRec,nNroPer*4-3,4)
		     IF !EMPTY(cNotRec)
			cNotDef := cNotRec
		     ENDIF

		     IF cEvaInd == '2'
			cNotInd := &cCamNtINot
		     ENDIF
		  ENDIF
		  cRecupe := cIndxRecu(cLogNot,nNroPer)
*===============FIN LOCALIZACION DEL ESTUDIANTE EN NOTAS

*===============LOCALIZACION DEL ESTUDIANTE EN NOTAS PROFESOR
		  cLogNoP := ''
		  cNoPDef := ''
		  cNoPRec := ''
		  IF lLocCodigo('cCodigoEst','NOP',cCodigoTes)

		     cLogNoP := &cCamIndNoP
		     cNtINoP := &cCamNtINoP

		     cNoPDef := &cCamNoPDef
		     cNoPRec := &cCamNoPRec
		     IF !EMPTY(cNoPRec)
			cNoPDef := cNoPRec
		     ENDIF

		  ENDIF
		  IF EMPTY(cLogNoP)
		     IF lMatSelTma
*			cError('NO EXISTEN INDICADORES VERIFIQUE '+;
*			       'SI EL ESTUDIANTE FUE TRASLADADO')
		     ELSE
*			cError('NO EXISTEN INDICADORES APLICADOS PARA EL '+;
*			       'PERIODO No. '+STR(nNroPer,1))
		     ENDIF
		     LOOP
		  ENDIF
*===============FIN LOCALIZACION DEL ESTUDIANTE EN NOTAS PROFESOR

*===============VALIDACION SI ESTA RETIRADO DEL GRUPO
/*	          IF NOP->lRetiroNoP
		     cError('EL ESTUDIANTE ESTA RETIRADO DEL GRUPO')
		     LOOP
		  ENDIF */
*===============FIN VALIDACION SI ESTA RETIRADO DEL GRUPO

*===============VALIDACION DE LOS INDICADORES
		  IF SUBS(cLogNot,1,nNroInd*nLenCod) #;
		     SUBS(cLogNoP,1,nNroInd*nLenCod)
		     cError('EXISTE INCONSISTENCIA EN LOS INDICADORES. '+;
			    'FAVOR INFORMAR.','PERIODO No.'+STR(nNroPer,1))

		     CloseAll()
		     RETURN NIL
		  ENDIF
*===============FIN VALIDACION DE LOS INDICADORES

*===============VALIDACION DEL JUICIO DE LA RECUPERACION
		  IF cNotDef # cNoPDef
		     cError('EXISTE INCONSISTENCIA EN LOS JUICIOS VALORATIVOS. '+;
			    'FAVOR INFORMAR.','PERIODO No.'+STR(nNroPer,1))
		     CloseAll()
		     RETURN NIL
		  ENDIF
		  IF cNotRec # cNoPRec
		     cError('EXISTE INCONSISTENCIA EN LOS JUICIOS DE RECUPERACION. '+;
			    'FAVOR INFORMAR.','PERIODO No.'+STR(nNroPer,1))
		     CloseAll()
		     RETURN NIL
		  ENDIF
*===============FIN VALIDACION DEL JUICIO DE LA RECUPERACION

*===============VALIDACION DE LOS REGISTROS
		  IF NOT->cCodigoEst # NOP->cCodigoEst
		     cError('LOS CODIGOS:'+NOT->cCodigoEst+':'+NOP->cCodigoEst+' '+;
			    'DEL ESTUDIANTE NO SON IGUALES')
		     CloseAll()
		     RETURN NIL
		  ENDIF
*===============FIN VALIDACION DE LOS REGISTROS


*===============VALIDACION DE LA RECUPERACION
		  IF EMPTY(cRecupe) .AND. !lRecNot
		     LOOP
		  ENDIF


		  IF lRecNot
		     IF EMPTY(cRecupe) .AND. (ALLTRIM(cNotDef) $ cJvfSi .OR.;
			EMPTY(cNotDef))
*			cError('NO APARECEN RECUPERACIONES PENDIENTES')
			LOOP
		     ENDIF
		  ELSE
		     IF EMPTY(cRecupe)
*			cError('NO APARECEN INDICADORES POR RECUPERAR')
			LOOP
		     ENDIF
		  ENDIF
		  nNroRec := ROUND(LEN(ALLTRIM(cRecupe))/nLenCod,0)

		  lHayNot := .F.
		  IF lRecNot
		     IF lHayItvJvf()
			IF VAL(cNotDef) > 0
			   lHayNot := IF(cNotDefJvf(VAL(cNotDef)) $ cJvfNo,.T.,.F.)
			ELSE
			   lHayNot := IF(ALLTRIM(cNotDef) $ cJvfNo,.T.,.F.)
			ENDIF
		     ELSE
			lHayNot := IF(ALLTRIM(cNotDef) $ cJvfNo,.T.,.F.)
		     ENDIF
		  ENDIF

		  IF lRecNot .AND. !lHayNot .AND. EMPTY(cRecupe) // Adicionado
		     LOOP
		  ENDIF

		  IF EMPTY(cNotRec)
		     cNotRec := cNotDef
		  ENDIF

		  lHayRec := .T.
*===============FIN VALIDACION DE LA RECUPERACION

*===============VALIDACION DE LAS NOTAS DE LAS INDICADORES
		  IF cEvaInd == '2' .AND. cNotInd # cNtINoP
		     cError('EXISTE INCONSISTENCIA EN LAS NOTAS DE LOS INDICADORES. '+;
			    'FAVOR INFORMAR.','PERIODO No.'+STR(nNroPer,1))
		     CloseAll()
		     RETURN NIL
		  ENDIF
*===============FIN VALIDACION DE LAS NOTAS DE LAS INDICADORES

*===============DETERMINACION DE COLS Y LONGITUD DE NOMBRE
		  IF lHayNot
		     aNroCol := {32,26,20,14,32}
		     aLenNom := {26,20,13,08,23}
		  ELSE
		     aNroCol := {37,31,25,19,37}
		     aLenNom := {31,25,18,13,31}
		  ENDIF

		  nIndMin := IF(nNroRec >10,10,nNroRec)
		  nNroCol := IF(nIndMin < 7,aNroCol[5],aNroCol[nIndMin-6])
		 *Escoge la columna seg�n el n�mero de notas

		  nLenNom := IF(nIndMin < 7,aLenNom[5],aLenNom[nIndMin-6])
		 *Escoge la longitud del nombre

		  nColIni := nNroCol
*===============FIN DETERMINACION DE COLS Y LONGITUD DE NOMBRE

*===============IMPRESION DE LOS ENCABEZADOS DE LAS NOTAS
		  i := 1
		  DO WHILE i <= nIndMin
		     @ nFilCab,nNroCol SAY (i<10,'INDI','IND')+LTRIM(STR(i))
		     i++
		     nNroCol := nNroCol + 6
		  ENDDO

		  IF lHayNot
		     IF cEvaInd == '2'
			@ nFilCab,nNroCol+3 SAY 'NOTA'
		     ELSE
			@ nFilCab,nNroCol SAY IF(lNotNum,'NOTA','JUIC')
		     ENDIF
		  ENDIF
		  nNroCol = nNroCol - (i-1)*6
		  RESTSCREEN(21,0,22,79,cSavLin)
*===============FIN IMPRESION DE LOS ENCABEZADOS DE LAS NOTAS

*===============VISUALIZACION DEL NOMBRE DEL ESTUDIANTE
		  cNombreTes := 'P:'+STR(nNroPer,1)+'=>'+;
				RTRIM(&cNalias->cApelliEst)+' '+;
				RTRIM(&cNalias->cNombreEst)

		  @ nNroFil,08 SAY SUBS(cNombreTes,1,nLenNom) PICT '@A'
*===============FIN VISUALIZACION DEL NOMBRE DEL ESTUDIANTE

*===============LECTURA DE LAS NOTAS ACTUALES
		  cLogros := cRecupe
		  IF !EMPTY(cRecupe)
		     DisplayLog(nNroFil,nColIni,cEvaInd,;
				cLogros,cNotInd,nNroRec,;
				lHayNot,cNotRec)
		  ENDIF
*===============FIN LECTURA DE LAS NOTAS ACTUALES

*===============ANALISIS DEL CAMBIO DE LA FILA
		  IF lOtrFil
		     IF i > 10
			nNroFil--
			nNroCol = nColIni
		     ENDIF
		  ENDIF
*===============FIN ANALISIS DEL CAMBIO DE LA FILA

*===============CAPTURA Y VALIDACION DE LOS LOGROS
		  IF !lLeeEva(cNomSis,@nNroFil,nColIni,nNroPer,lIndNiv,cEvaInd,;
			      cCodigoTgr,cCodigoTni,nNroRec,nNroLog,;
			      @cLogros,@cNotInd,lHayNot,lNotNum,@cNotRec,;
			      cValNot,cSavLin,lLeeRec,cLogNot)
		     LOOP
		  ENDIF
		  IF nNroLog == 0
		     RESTSCREEN(21,0,22,79,cSavLin)
		  ENDIF
*===============FIN CAPTURA Y VALIDACION DE LOS LOGROS

*===============CORREGIR LAS NOTAS
		  IF !lPregunta('Desea Grabar? Si No')

		     IF nNroRec > 10
			nNroFil--
			nNroCol := nColIni
		     ENDIF
		     LOOP
		  ELSE
		     RESTSCREEN(21,0,22,79,cSavLin)
		  ENDIF
*===============FIN CORREGIR LAS NOTAS

*===============REEMPLAZO DE LOS INDICADORES DE NOTAS
		  FOR i := 1 TO nNroRec
		      cCodLog := SUBS(cLogros,nPosIni(i,nLenCod),nLenCod)
		      nPosInd := AT(SUBS(cCodLog,1,4),cLogNot)

*		      IF SUBS(cLogNot,nPosInd+5,1) == SPACE(01)
			 cLogNot := STUFF(cLogNot,nPosInd+5,1,SUBS(cCodLog,6,1))
*		      ENDIF

		  NEXT
*===============FIN REEMPLAZO DE LOS INDICADORES DE NOTAS

*===============REEMPLAZO DE LOS INDICADORES DEL PROFESOR
		  FOR i := 1 TO nNroRec

		      cCodLog := SUBS(cLogros,nPosIni(i,nLenCod),nLenCod)
		      nPosInd := AT(SUBS(cCodLog,1,4),cLogNoP)

*		      IF SUBS(cLogNoP,nPosInd+5,1) == SPACE(01)
			 cLogNoP := STUFF(cLogNoP,nPosInd+5,1,SUBS(cCodLog,6,1))
*		      ENDIF
		  NEXT
*===============FIN REEMPLAZO DE LOS INDICADORES DEL PROFESOR

*===============VALIDACION DE LOS CODIGOS
		  IF SUBS(cLogNot,1,nNroInd*nLenCod) #;
		     SUBS(cLogNoP,1,nNroInd*nLenCod)
		     cError('EXISTE INCONSISTENCIA EN LOS INDICADORES DESPUES DE GRABAR. '+;
			    'FAVOR INFORMAR.')
		     CloseAll()
		     RETURN NIL
		  ENDIF
*===============FIN VALIDACION DE LOS CODIGOS

*===============GRABACION DE LAS RECUPERACIONES EN NOTAS
		  SELECT NOT
		  IF NOT->(lRegLock(lShared,.F.))

		     REPLACE &cCamIndNot WITH cLogNot
		    *Grabaci�n de los Indicadores

		     IF cEvaInd == '2'
			REPLACE &cCamNtINot WITH cNotInd
		     ENDIF
		    *Grabaci�n de los notas de los indicadores

		     REPL &cCamNotRec WITH STUFF(&cCamNotRec,nNroPer*4-3,4,cNotRec)
		    *Grabaci�n de la nota de Recuperaci�n.

		     NOT->(DBCOMMIT())
		     lHayLog := .T.
		  ELSE
		     cError('NO SE GRABA LOS LOGROS DEL ESTUDIANTE')
		  ENDIF
		  IF lShared
		     NOT->(DBUNLOCK())
		  ENDIF
*===============FIN GRABACION DE LAS RECUPERACIONES EN NOTAS

*===============GRABACION DE LAS RECUPERACIONES EN NOTAS DEL PROFESOR
		  SELECT NOP
		  IF NOP->(lRegLock(lShared,.F.))

		     cIndic1Tlo := &cCamIndNoP
		     REPLACE &cCamIndNoP WITH cLogNoP
		    *Grabaci�n de los Indicadores

		     REPLACE &cCamNtINoP WITH cNotInd
		    *Grabaci�n de los notas de los indicadores

		     cDefRe1Tlo := &cCamNoPRec
		     REPLACE &cCamNoPRec WITH cNotRec
		    *Grabaci�n de la nota de Recuperaci�n

		     REPL NOP->cNomUsrNop WITH cNomUsr
		     REPL NOP->dFecUsrNop WITH DATE()
		     REPL NOP->cHorUsrNop WITH TIME()

		     NOP->(DBCOMMIT())
		     lHayLog := .T.
		  ELSE
		     cError('NO SE GRABA LOS LOGROS DEL ESTUDIANTE')
		  ENDIF
		  IF lShared
		     NOP->(DBUNLOCK())
		  ENDIF
*===============FIN GRABACION DE LAS RECUPERACIONES EN NOTAS DEL PROFESOR

*===============GRABACION DEL LOG
		  IF lHayLog
		     AADD(aGenLog,{'nCodigoLog',3})
		     AADD(aGenLog,{'cOpcionLog','<DEFINIR RECUPERACIONES>'})
		     AADD(aGenLog,{'cMaeActLog',cMaeAct})
		     AADD(aGenLog,{'nNroPerLog',nNroPer})

		     AADD(aGenLog,{'cNomUsrLog',cNomUsr})
		     AADD(aGenLog,{'cHorIniLog',cHorIni})
		     AADD(aGenLog,{'cHorFinLog',TIME()})

		     AADD(aGenLog,{'cCodigoPro',SUBS(cPatPro,LEN(cPatPro)-5,6)})
		     AADD(aGenLog,{'cCodigoEst',cCodigoTes})
		     AADD(aGenLog,{'cIndic1Log',cIndic1Tlo})
		     AADD(aGenLog,{'cIndic2Log',cLogNoP})
		     AADD(aGenLog,{'cDefin1Log',cNoPDef})
		     AADD(aGenLog,{'cDefin2Log',cNoPDef})
		     AADD(aGenLog,{'cDefRe1Log',cDefRe1Tlo})
		     AADD(aGenLog,{'cDefRe2Log',cNotRec})

		     cTxtLog := ALLTRIM(MAT->cNombreMat)+' GRUPO: '+cCodigoTgr
		     AADD(aCamLog,{'cObsMatLog',cTxtLog})

/*
		     IF cIndic1Tlo # cLogNoP
			cTxtLog := 'Modific� Indicadores '
		     ELSE
			cTxtLog := 'No Modific� Indicadores '
		     ENDIF

		     IF cDefRe1Tlo # cNotRec
			cTxtLog += 'Modific� Juicio Valorativo'
		     ELSE
			cTxtLog += 'No Modific� Juicio Valorativo'
		     ENDIF
*/
		     cTxtLog := ''
		     AADD(aCamLog,{'cObservLog',cTxtLog})

		     aCamLog := aGraProLog(lShared,'LOG',aCamLog,aGenLog)
		     lHayLog := .F.
		  ENDIF
*===============FIN GRABACION DEL LOG

*===============INCREMENTO DE LAS FILAS
		  lOtrFil := .F.
		  nNroFil++
		  IF nNroFil > nFilFin

*--------------------IMPRESION DE LOS ENCABEZADOS
		       nNroFil := nMarco(nFilPal+1,;
					 ALLTRIM(MAT->cNombreMat)+' ',22,'�')

		       RESTSCREEN(nFilCab,0,nFilCab,79,cSavCab)
*--------------------FIN IMPRESION DE LOS ENCABEZADOS

*--------------------IMPRESION DE LOS DATOS DEL ULTIMO ESTUDIANTE
		       nNroFil := nFilCab + 1
		       @ nNroFil,01 SAY cCodigoTes
		       @ nNroFil,08 SAY SUBS(cNombreTes,1,nLenNom) PICT '@A'
		       SELECT NOT
*--------------------FIN IMPRESION DE LOS DATOS DEL ULTIMO ESTUDIANTE

*--------------------IMPRESION DE LOS LOGROS DEL ULTIMO ESTUDIANTE
		       IF !EMPTY(cRecupe)
			  DisplayLog(@nNroFil,nColIni,cEvaInd,;
				     cLogros,cNotInd,nNroRec,;
				     lHayNot,cNotRec)
		       ENDIF

		       nNroCol := nColIni
		       nNroFil := nFilCab
		       nNroFil := nNroFil + 2
*--------------------FIN IMPRESION DE LOS LOGROS DEL ULTIMO ESTUDIANTE

		  ENDIF
*===============FIN INCREMENTO DE LAS FILAS

	    ENDFOR
	    IF !lHayRec
	       cError('NO APARECEN INDICADORES POR RECUPERAR')
	    ENDIF
**********RECUPERACIONES POR PERIODOS

       ENDDO
       CloseAll()
       RETURN NIL
*>>>>FIN GRABACION DE LAS NOTAS DE LOS ESTUDIANTES

