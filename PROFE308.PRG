/* SIMA - CONTABILIDAD PROFESORES

MODULO      : EVALUAR
SUBMODULO...: EVALUAR

**************************************************************************
* TITULO..: SOLO NOTAS                                                   *
**************************************************************************

AUTOR: Nelson Fern�ndez G�mez       FECHA DE CREACION: NOV 15/2006 MIE
       Colombia, Bucaramanga        INICIO: 03:40 PM   NOV 15/2006 MIE


OBJETIVOS:

1- Permite grabar solo la nota final para cada estudiante

2- Retorna NIL

*------------------------------------------------------------------------*
*                              PROGRAMA                                  *
*------------------------------------------------------------------------*/

FUNCTION Profe_308(aP1,aP2,aP3)

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
       LOCAL lAnoAct := .T.                 // .T. A�o Actual
       LOCAL nLenOpc := 0                   // Longitud mayor de los opciones

       LOCAL nFilSup := 0                   // Columna Superior
       LOCAL nColSup := 0                   // Columna Superior
       LOCAL nInfFil := 0                   // Fila Inferior
       LOCAL nInfCol := 0                   // Columna Inferior

       LOCAL lOtrFil := .F.                 // .T. Otra Fila
       LOCAL nNroFil := 0                   // Fila de lectura
       LOCAL nNroCol := 1                   // Columna de lectura

       LOCAL nFilIni := 0                   // Fila Inicial
       LOCAL nColIni := 0                   // Columna Inicial
       LOCAL nFilFin := 19                  // Fila Final de Control
       LOCAL nMaxInd := 0                   // M�ximo de Indicadores a visualizar

       LOCAL lIndNiv := .F.                 // .T. Indicadores de Nivel
       LOCAL cEvaInd := ''                  // Evaluar el Indicador
       LOCAL lHayNot := .F.                 // .T. Hay nota definitiva
       LOCAL lNotNum := .F.                 // .T. Nota N�merica .F. Nota Cualitativa

       LOCAL nTotGru := 0                   // Total grupos de trabajo
       LOCAL lNroLis := .F.                 // .T. Por n�mero de lista
       LOCAL aMenus  := {}                  // Vector de declaracion de men�
       LOCAL aAyuda  := {}                  // Vector de ayudas para el men�
       LOCAL nOpcion := 0                   // Opci�n del men�
       LOCAL nRegNoP := 0                   // Registro notas del profesor

       LOCAL nTotPer := 5                   // Total de Periodos
       LOCAL nPerAct := 0                   // Periodo Actual
       LOCAL nNroPer := 0                   // N�mero del periodo
       LOCAL cCamNot := ''                  // Campo de las notas
*      LOCAL cCamInd := ''                  // Campo de los indicadores. Marca Error el la Macro al declararse local
*      LOCAL cCamNtI := ''                  // Campo de Nota de Indicadores
       LOCAL cCamDef := ''                  // Campo de definitivas
       LOCAL cCamObs := ''                  // Campo de Observaciones

       LOCAL aNroCol := {}                  // N�meros de Columnas
       LOCAL aLenNom := {}                  // Longitud de nombres
       LOCAL nLenNom := 0                   // Longitud del nombre
       LOCAL nTotInd := 20                  // Total de Indicadores
       LOCAL nNroInd := 0                   // N�mero de indicadores
       LOCAL nIndMin := 0                   // Indicador M�nimo para control
       LOCAL nNroLog := 1                   // N�mero de logro escogido
*      LOCAL cNoNota := ''                  // Espacios de la nota. Marca Error el la Macro al declararse local
*      LOCAL nInicio := 0                   // Puntero de Inicio. Marca Error el la Macro al declararse local
       LOCAL lAllNot := .F.                 // .T. Todas las notas grabadas
       LOCAL lExiNot := .F.                 // .T. Existen notas grabadas
       LOCAL lOtrEst := .F.                 // .T. Otro Estudiante
       LOCAL cCodLog := ''                  // C�digo del logro
       LOCAL cCodEva := ''                  // C�digo de Evaluaci�n
       LOCAL cLogros := ''                  // Logros aplicados
       LOCAL cNotInd := ''                  // Nota de los Indicadores
       LOCAL nRegMat := 0                   // Registro de la Materia
       LOCAL nLenInd := 6                   // Longitud del C�digo del Indicador
       LOCAL nLenNtI := 8                   // Longitud del C�digo de la Nota de la Recuperaci�n del Indicador
       LOCAL lLeeRec := .F.                 // .T. Leer Recuperaci�n
       LOCAL cMsgTxt := ''                  // Mensaje de texto

       LOCAL nNroReg := 0                   // N�mero del Registro
       LOCAL cNotDef := ''                  // Nota definitiva
       LOCAL cPictur := ''                  // Picture de lectura
       LOCAL cValNot := ''                  // VALIDACION DE LA NOTA

       LOCAL GetList := {}                  // Variable del Sistema
     *�Variables espec�ficas 

       LOCAL cCodigoTpr := ''               // C�digo del Profesor
       LOCAL cCodigoTes := ''               // C�digo del estudiante
       LOCAL cNombreTes := ''               // Nombre del estudiante
       LOCAL lRetiroTes := .F.              // Retiro del Estudiante
       LOCAL cCodigoTma := ''               // C�digo de la Materia
       LOCAL cNombreTma := ''               // Nombre de la Materia
       LOCAL cCodigoTgr := ''               // C�digo del grupo
       LOCAL cCodigoTni := ''               // C�digo del nivel
       LOCAL cCodigoTcl := ''               // C�digo Clase del indicador
       LOCAL nTipCarTca := 0                // Tipo de carga
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
       IF !lUseMae(lShared,PathUno+'\'+cPatSis,cMaeAct,cAnoSis)       .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+;
		       FilePro+cAnoSis+ExtFile,'PRO',NIL,lShared) .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FileMat,'MAT',NIL,lShared)                 .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FileNiv+cAnoSis+ExtFile,'NIV',NIL,lShared) .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FileJvf,'JVF',NIL,lShared)                 .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FConTbl+cMaeACt+ExtFile,'TCO',NIL,lShared) .OR.;
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

       CASE TCO->(RECCOUNT()) == 0
	    cError('NO EXISTEN CONFIGURACION PARA EL NIVEL DE LA CONTABILIDAD')

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

*>>>>LOCALIZACION DEL GRUPO
       cCodigoTpr := SUBS(cPatPro,LEN(cPatPro)-5,6)
       IF !lLocCodigo('cCodigoPro','PRO',cCodigoTpr,'DEL PROFESOR')
	  CloseAll()
	  RETURN NIL
       ENDIF
*>>>>FIN LOCALIZACION DEL GRUPO

*>>>>SELECCION DE LA MATERIA DEL GRUPO
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
	  nRegMat := MAT->(RECNO())
       cCodigoTgr := SUBS(CAP->cCodigoCar,5,4)
       cCodigoTni := SUBS(cCodigoTgr,1,2)
       cCodigoTma := MAT->cCodigoMat
       cNombreTma := MAT->cNombreMat
       nTipCarTca := CAP->nTipCarCar
*>>>>FIN SELECCION DE LA MATERIA DEL GRUPO

*>>>>SELECION DE LAS AREAS DE TRABAJO
       IF !('XXXX' $ cCodigoTgr)
	  IF !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
			  cFilePlan(cAnoUsr,cCodigoTgr),'PLA',NIL,lShared)
	     cError('ABRIENDO ARCHIVO DEL PLAN DE ESTUDIOS')
	     CLOSE ALL
	     RETURN NIL
	  ENDIF
       ENDIF
*>>>>FIN SELECION DE LAS AREAS DE TRABAJO

*>>>>SELECION DE LAS AREAS DE TRABAJO
       IF !lUseDbf(.T.,cPatPro+'\'+;
		       CAP->cCodigoCar+ExtFile,'NOP',NIL,lShared)
	  cError('ABRIENDO ARCHIVO DE NOTAS DEL PROFESOR GRUPO '+cCodigoTgr)
	  CLOSE ALL
	  RETURN NIL
       ENDIF
*>>>>FIN SELECION DE LAS AREAS DE TRABAJO

*>>>>VALIDACION DE CONTENIDOS DE ARCHIVOS
       lHayErr := .T.
       DO CASE
       CASE !('XXXX' $ cCodigoTgr) .AND. PLA->(RECCOUNT()) == 0
	    cError('NO EXISTE REGISTROS DEL PLAN ACADEMICO')

       CASE NOP->(RECCOUNT()) == 0
	    cError('NO EXISTEN ESTUDIANTES DE '+ALLTRIM(CAP->cNombreMat)+;
		   ' DE '+cCodigoTgr)
       OTHERWISE
	    lHayErr :=.F.
       ENDCASE

       IF lHayErr
	  CLOSE ALL
	  RETURN NIL
       ENDIF
       cSavPan := SAVESCREEN(0,0,24,79)
*>>>>FIN VALIDACION DE CONTENIDOS DE ARCHIVOS

*>>>>LOCALIZACION DE LA MATERIA EN PLAN
       lMatVarMat(cCodigoTma,.F.)
       IF !('XXXX' $ cCodigoTgr)
	  IF !lLocCodigo('cCodigoMat','PLA',MAT->cCodigoMat)
	     cError('LA MATERIA: '+cCodigoTma+;
		     'NO EXISTE EN PLAN DE: '+cCodigoTgr)
	     CLOSE ALL
	     RETURN NIL
	  ENDIF
       ENDIF
       MAT->(DBGOTO(nRegMat))
*>>>>FIN LOCALIZACION DE LA MATERIA EN PLAN

*>>>>LECTURA DEL NUMERO DEL BIMESTRE
       nTotPer := TCO->nTotPerTbl
       nPerAct := IF(EMPTY(PRO->nNroPerPro),TCO->nPerActTbl,PRO->nNroPerPro)
       nNroPer := nLeeNroPer(nFilInf+1,nColInf,nFilPal,nTotPer,nPerAct,.T.)
       IF nNroPer == 0
	  CLOSE ALL
	  RETURN NIL
       ENDIF
       cCamDef := 'NOP->cDefin'+STR(nNroPer,1,0)+'NoP'
*>>>>FIN ANALISIS PARA LA SALIDA DEL PROGRAMA

*>>>>LECTURA DE LA CONFIGURACION DEL NIVEL DE LA CONTABILIDAD
       lIndNiv := TCO->lIndNivTbl
       cEvaInd := SUBS(TCO->cEvaIndTbl,nNroPer,1)
       IF TCO->lNotPlaTbl
	  lHayNot := IF(SUBS(PLA->cHayNotPla,nNroPer,1)='1',.T.,.F.)
       ELSE
	  lHayNot := IF(SUBS(TCO->cHayNotTbl,nNroPer,1)='1',.T.,.F.)
       ENDIF
       lNotNum := IF(SUBS(TCO->cNotNumTbl,nNroPer,1)='1',.T.,.F.)
       IF lNotNum
	  cValNot := ALLTRIM(TCO->cValNumTbl)+SPACE(01)
       ELSE
	  IF EMPTY(TCO->cValNumTbl)
	     cValNot := ALLTRIM(TCO->cValNotTbl)+SPACE(01)
	  ELSE
	     cValNot := ALLTRIM(TCO->cValNumTbl)+SPACE(01)
	  ENDIF
       ENDIF
*>>>>FIN LECTURA DE LA CONFIGURACION DEL NIVEL DE LA CONTABILIDAD

*>>>>IMPRESION DE LOS ENCABEZADOS
       nNroFil := nMarco(nFilPal+1,ALLTRIM(cNombreTma),22,'�')
       @ nNroFil,01 SAY 'NRO'
       @ nNroFil,05 SAY 'NOMBRE'
*>>>>FIN IMPRESION DE LOS ENCABEZADOS

*>>>>SELECION DE LAS NOTAS A GRABAR
       nNroCol := 46
       IF lNotNum .OR. cEvaInd == '2'
	  @ nNroFil,nNroCol SAY 'NOTA'
       ELSE
	  @ nNroFil,nNroCol SAY 'JUIC'
       ENDIF

       nNroFil = nNroFil + 1
      *Ubica la fila primera de grabacion

       cSavCab := SAVESCREEN(nNroFil-1,0,nNroFil-1,79)
*>>>>FIN SELECION DE LAS NOTAS A GRABAR

*>>>>LOCALIZACION DE LA NOTA POR GRABAR
       nNroReg := 0
       SELECT NOP
       NOP->(DBGOTOP())
       DO WHILE .NOT. NOP->(EOF())

**********ANALISIS DEL RETIRO DEL ESTUDIANTE
	    lRetiroTes := .F.
	    IF lSekCodigo(NOP->cCodigoEst,cMaeAct)

	       lRetiroTes := .T.
	       DO CASE
	       CASE &cMaeAct->lRetiroEst .AND. nNroPer > &cMaeAct->nPerRetEst
	       CASE nTipCarTca == 0 .AND. NOP->lRetiroNoP
	       CASE nTipCarTca # 0 .AND. NOP->lRetiroNoP
	       OTHERWISE
		    lRetiroTes := .F.
	       ENDCASE

	    ENDIF
**********FIN ANALISIS DEL RETIRO DEL ESTUDIANTE

**********ANALISIS DE TIENE NOTA DIGITADA
	    IF !lRetiroTes .AND. &cCamDef == SPACE(04)
	       lAllNot := .F.
	       nNroReg := NOP->(RECNO())
	       EXIT
	    ELSE
	       lAllNot := .T.
	    ENDIF
**********FIN ANALISIS DE TIENE NOTA DIGITADA

	  NOP->(DBSKIP())

       ENDDO
*>>>>FIN LOCALIZACION DE LA NOTA POR GRABAR

*>>>>GRABACION DE LAS NOTAS DE LOS ESTUDIANTES
       lOtrEst := .T.
       lHayErr := .F.
       cSavLin := SAVESCREEN(21,0,22,79)
       DO WHILE lOtrEst

**********CAPTURA Y VALIDACION DEL NUMERO DEL REGISTRO
	     DO WHILE .T.

		@ nNroFil,01 GET nNroReg PICT '99'
		READ

		IF nNroReg == 0
		   CloseAll()
		   RETURN NIL
		ENDIF

		IF nNroReg > NOP->(RECCOUNT())
		   cError('EL NUMERO DE LISTA NO DEBE SER '+;
			  'MAYOR AL NUMERO DE ESTUDIANTES')
		   LOOP
		ENDIF
		NOP->(DBGOTO(nNroReg))
		EXIT
	     ENDDO
**********FIN CAPTURA Y VALIDACION DEL NUMERO DEL REGISTRO

**********LECTURA DE LAS NOTAS ACTUALES
	    IF !lHayErr
	       cNotDef := &cCamDef
	    ENDIF
**********FIN LECTURA DE LAS NOTAS ACTUALES

**********IMPRESION DEL NOMBRE DEL ESTUDIANTE
	    cCodigoTes := NOP->cCodigoEst
	    IF !lSekCodigo(cCodigoTes,cMaeAct)
	       cNombreTes := 'No Existe Estudiante Cod: '+cCodigoTes
	    ENDIF
	    cNombreTes = RTRIM(&cMaeAct->cApelliEst)+' '+;
			 RTRIM(&cMaeAct->cNombreEst)
	    @ nNroFil,05 SAY SUBS(cNombreTes+SPACE(40),1,40)  PICT '@A'
**********FIN IMPRESION DEL NOMBRE DEL ESTUDIANTE

**********VALIDACION DEL ESTUDIANTE SI ESTA RETIRADO
	    lHayErr := .T.
	    DO CASE
	    CASE &cMaeAct->lRetiroEst .AND. nNroPer > &cMaeAct->nPerRetEst
		 cError(cNombreTes,'ESTA RETIRADO')

	    CASE nTipCarTca == 0 .AND. NOP->lRetiroNoP
		 cError(cNombreTes,'PERTENECE A OTRO GRUPO')

	    CASE nTipCarTca # 0 .AND. NOP->lRetiroNoP
		 cError(cNombreTes,'FUE RETIRADO DE LA LISTA')

	    OTHERWISE
		 lHayErr :=.F.
	    ENDCASE

	    IF lHayErr

	       NOP->(DBSKIP())
	       nNroReg := NOP->(RECNO())

	       IF NOP->(EOF())
		  lOtrEst := .F.
	       ENDIF
	       LOOP

	    ENDIF
**********FIN VALIDACION DEL ESTUDIANTE SI ESTA RETIRADO

**********PICTURE PARA LA NOTA DEFINITIVA
	    IF lNotNum .OR. cEvaInd == '2'
	       cPictur := '99.9'
	       cNotDef := VAL(cNotDef)
	    ELSE
	       cPictur := 'A'
	    ENDIF
**********FIN PICTURE PARA LA NOTA DEFINITIVA

**********CAPTURA Y VALIDACION DE LA NOTA DEFINITIVA
	     DO WHILE .T.

*===============LECTURA DE LA NOTA
		  @ nNroFil,nNroCol GET cNotDef PICT cPictur
		  READ
*===============FIN LECTURA DE LA NOTA

*===============VALIDACION DE LA NOTA
		  IF cPictur == '99.9'      .AND.;
		     cNotDef > VAL(cValNot)

		     cError('NOTA FUERA DE RANGO')
		     LOOP

		  ENDIF

		  IF cPictur == 'A' .AND.;
		     !(ALLTRIM(cNotDef) $ cValNot) .AND.;
		     !EMPTY(cNotDef)

		     cError('JUICIO VALORATIVO NO PERMITIDO',;
			    ALLTRIM(cNotDef),cValNot)
		     LOOP

		  ENDIF
		  EXIT
*===============FIN VALIDACION DE LA NOTA

	     ENDDO
**********FIN CAPTURA Y VALIDACION DE LA NOTA DEFINITIVA

**********ANALISIS DE LA SALIDA DEL SISTEMA
	    IF VALTYPE(cNotDef) == 'N'

	       IF cNotDef == 0
		  cNotDef := SPACE(04)
	       ELSE
		  cNotDef := STR(cNotDef,4,1)
	       ENDIF

	       IF VAL(cNotDef) == 11
		  lOtrEst := .F.
		  LOOP
	       ENDIF

	    ENDIF

	    IF VALTYPE(cNotDef) == 'C' .AND. 'X' $ cNotDef
	       lOtrEst := .F.
	       LOOP
	    ENDIF
**********FIN ANALISIS DE LA SALIDA DEL SISTEMA

**********CORREGIR LAS NOTAS
	    IF !lPregunta('Desea Grabar? Si No')
	       lHayErr := .T.
	       LOOP
	    ELSE
	       lHayErr := .F.
	       RESTSCREEN(21,0,22,79,cSavLin)
	    ENDIF
**********FIN CORREGIR LAS NOTAS

**********GRABAR LAS NOTAS
	    SELECT NOP
	    IF NOP->(lRegLock(lShared,.F.))
	       IF !NOP->lRetiroNoP

		  REPLACE &cCamDef WITH cNotDef

		  REPL NOP->cNomUsrNop WITH cNomUsr
		  REPL NOP->dFecUsrNop WITH DATE()
		  REPL NOP->cHorUsrNop WITH TIME()

		  NOP->(DBCOMMIT())
	       ENDIF
	    ELSE
	       cError('NO SE GRABA LOS LOGROS DEL ESTUDIANTE')
	    ENDIF
	    IF lShared
	       NOP->(DBUNLOCK())
	    ENDIF
**********FIN GRABAR LAS NOTAS

**********INCREMENTO DE LAS FILAS
	    lOtrFil := .F.
	    nNroFil++
	    IF nNroFil > nFilFin

*--------------IMPRESION DE LOS ENCABEZADOS
		 nNroFil := nMarco(nFilPal+1,ALLTRIM(cNombreTma),22,'�')

		 @ nNroFil,01 SAY IF(lNroLis,'NRO','GRU')
		 @ nNroFil,05 SAY 'NOMBRE'
		 RESTSCREEN(nNroFil,0,nNroFil,79,cSavCab)
*--------------IMPRESION DE LOS ENCABEZADOS

	       SELECT NOP
	       nNroFil++

	    ENDIF
**********FIN INCREMENTO DE LAS FILAS

**********UBICACION DEL REGISTRO DEL PROXIMO ESTUDIANTE
            DO CASE
	    CASE lNroLis
                 IF RECNO() # RECCOUNT()
                    SKIP
                 ELSE
		    RESTSCREEN(0,0,24,79,cSavPan)
                   *Restaura la pantalla
		    lOtrEst := .F.
		 ENDIF

	     CASE .NOT. lNroLis

                  IF .NOT. EOF()
                     SKIP
                  ENDIF

                  IF EOF()
		     lOtrEst := .F.
		  ENDIF
	     ENDCASE
	     nNroReg := NOP->(RECNO())
**********FIN UBICACION DEL REGISTRO DEL PROXIMO ESTUDIANTE

       ENDDO

       CloseAll()
       RETURN NIL
*>>>>FIN GRABACION DE LAS NOTAS DE LOS ESTUDIANTES