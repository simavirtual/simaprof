/* SIMA - CONTABILIDAD PROFESORES

MODULO      : LOGROS
SUBMODULO...: MODIFICAR

**************************************************************************
* TITULO..: MODIFICACION DE LOS LOGROS                                   *
**************************************************************************

AUTOR: Nelson Fern�ndez G�mez       FECHA DE CREACION: MAR 27/1996 MIE A
       Colombia, Bucaramanga        INICIO: 12:10 AM   MAR 27/1996 MIE

OBJETIVOS:

1- Permite la modificaci�n de los logros aplicados a cada estudiante

2- Retorna NIL

*------------------------------------------------------------------------*
*                              PROGRAMA                                  *
*------------------------------------------------------------------------*/

FUNCTION Profe_302(aP1,aP2,aP3)

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
       LOCAL     i,j := 0                   // Contador
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
       LOCAL nFilFin := 19                   // Fila Final de Control

       LOCAL lIndNiv := .F.                 // .T. Indicadores de Nivel
       LOCAL lEvaInd := .F.                 // .T. Evaluar el Indicador
       LOCAL cEvaInd := ''                  // Evaluar el Indicador
       LOCAL lHayNot := .F.                 // .T. Hay nota definitiva
       LOCAL lNotNum := .F.                 // .T. Nota N�merica .F. Nota Cualitativa
       LOCAL cValNot := ''                  // Validaci�n de la nota

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
       LOCAL nNroLog := 0                   // N�mero de logro escogido
*      LOCAL cNoNota := ''                  // Espacios de la nota. Marca Error el la Macro al declararse local
*      LOCAL nInicio := 0                   // Puntero de Inicio. Marca Error el la Macro al declararse local
       LOCAL lAllNot := .F.                 // .T. Todas las notas grabadas
       LOCAL lExiNot := .F.                 // .T. Existen notas grabadas
       LOCAL lOtrEst := .F.                 // .T. Otro Estudiante
       LOCAL cCodLog := ''                  // C�digo del logro
       LOCAL cLogros := ''                  // Logros aplicados
       LOCAL cNotInd := ''                  // Nota de los Indicadores
       LOCAL nRegMat := 0                   // Registro de la Materia
       LOCAL nLenInd := 6                   // Longitud del C�digo
       LOCAL nLenNtI := 8                   // Longitud del C�digo de la Nota de la Recuperaci�n del Indicador
       LOCAL lLeeRec := .F.                 // .T. Leer Recuperaci�n
       LOCAL cNotDef := ''                  // Nota Defintiva
       LOCAL cNroLis := ''                  // N�mero de Lista
       LOCAL cMsgTxt := ''                  // Mensaje de Texto
       LOCAL nMaxInd := 0                   // M�ximo de indicadores
       LOCAL cJvfCal := ''                  // Juicio Valorativo Calculado
       LOCAL lJvfCal := .F.                 // .T. Juicio Valorativo Calculado
       LOCAL nColDef := 0                   // Columna de la definitiva

       LOCAL lMatVar := .F.                 // .T. Materia Variable
       LOCAL lEstNot := .F.                 // .T. Estudiante en Notas
       LOCAL cIndic1 := ''                  // Indicadores del Periodo No.1
       LOCAL cIndic2 := ''                  // Indicadores del Periodo No.2
       LOCAL cIndic3 := ''                  // Indicadores del Periodo No.3
       LOCAL cIndic4 := ''                  // Indicadores del Periodo No.4
       LOCAL cIndic5 := ''                  // Indicadores del Periodo No.5
       LOCAL cTitul1 := ''                  // T�tulo1 para encabezado de captura

       LOCAL GetList := {}                  // Variable del Sistema
     *�Variables espec�ficas 

       LOCAL cCamIndNot := ''               // Campo Indicadores en Notas
       LOCAL cCamNtINot := ''               // Campo de Nota de Indicadores
       LOCAL cCamNotDef := ''               // Campo Notas Definitivas
       LOCAL cCamNotRec := ''               // Campo Notas de Recuperaci�n
     *�Variables de Campos de Notas en Contabilidad

       LOCAL cCodigoTpr := ''               // C�digo del Profesor
       LOCAL cCodigoTes := ''               // C�digo del estudiante
       LOCAL cNombreTes := ''               // Nombre del estudiante
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
       IF !lUseMae(lShared,PathUno+'\'+cPatSis,cMaeAct,cAnoSis)      .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+;
		       FilePro+cAnoSis+ExtFile,'PRO',NIL,lShared) .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FileMat,'MAT',NIL,lShared)                .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FileNiv+cAnoSis+ExtFile,'NIV',NIL,lShared) .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FileGru+cMaeAct+cAnoSis+ExtFile,'GRU',NIL,lShared) .OR.;
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
	    cError('NO EXISTEN LAS CLASES DE LOGOS')

       CASE EVA->(RECCOUNT()) == 0
	    cError('NO EXISTEN LOS CODIGOS DE EVALUACION')

       CASE TCO->(RECCOUNT()) == 0
	    cError('NO EXISTEN CONFIGURACION PARA EL NIVEL DE LA CONTABILIDAD')

       CASE JVF->(RECCOUNT()) == 0
	    cError('NO EXISTEN JUICIOS VALORATIVOS GRAGADOS PARA '+cMaeAct)

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
       IF !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       cFilePlan(cAnoUsr,cCodigoTgr),'PLA',NIL,lShared)
	  cError('ABRIENDO ARCHIVO DEL PLAN ACADEMICO')
	  CLOSE ALL
	  RETURN NIL
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
*>>>>FIN VALIDACION DE CONTENIDOS DE ARCHIVOS

*>>>>LOCALIZACION DE LA MATERIA EN PLAN
       lMatVar := lMatVarMat(cCodigoTma,.F.)
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
       cCamNot := 'NOP->cNotas'+STR(nNroPer,1,0)+'NoP'
       cCamInd := 'NOP->cIndic'+STR(nNroPer,1,0)+'NoP'
       cCamNtI := 'NOP->cIndnt'+STR(nNroPer,1,0)+'NoP'
       cCamDef := 'NOP->cDefin'+STR(nNroPer,1,0)+'NoP'
       cCamObs := 'NOP->cObsev'+STR(nNroPer,1,0)+'NoP'
*>>>>FIN ANALISIS PARA LA SALIDA DEL PROGRAMA

*>>>>LECTURA DE LA CONFIGURACION DEL NIVEL DE LA CONTABILIDAD
       lIndNiv := TCO->lIndNivTbl
       lEvaInd := IF(SUBS(TCO->cEvaIndTbl,nNroPer,1)='1',.T.,.F.)
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

       IF cEvaInd == '2'
	  lHayNot := .F.
       ENDIF
*>>>>FIN LECTURA DE LA CONFIGURACION DEL NIVEL DE LA CONTABILIDAD


*>>>>VALIDACION DE LA ACTIVACION DE LA EVALUACION
       IF cEvaInd # '1' .AND. cEvaInd # '2'
	  cError('NO ESTA ACTIVADA LA EVALUACION DEL INDICADOR')
	  CloseAll()
	  RETURN NIL
       ENDIF
*>>>>FIN VALIDACION DE LA ACTIVACION DE LA EVALUACION

*>>>>LECTURA DEL NUMERO DEL INDICADORES
       nNroInd := nLeeNroInd(lShared,nFilInf+1,nColInf,nNroPer,nTotInd)
       IF nNroInd == 0
	  CLOSE ALL
	  RETURN NIL
       ENDIF
       nIndMin := IF(nNroInd >10,10,nNroInd)
*>>>>FIN LECTURA DEL NUMERO DEL INDICADORES

*>>>>IMPRESION DE LOS ENCABEZADOS
       nNroFil := nMarco(nFilPal+1,ALLTRIM(cNombreTma),22,'�')
       @ nNroFil,01 SAY 'NRO'
       @ nNroFil,05 SAY 'NOMBRE'
*>>>>FIN IMPRESION DE LOS ENCABEZADOS

*>>>>DETERMINACION DE COLS Y LONGITUD DE NOMBRE
       IF lHayNot
	  aNroCol := {32,26,20,14,32}
	  aLenNom := {26,20,13,08,26}
       ELSE
	  aNroCol := {37,31,25,19,37}
	  aLenNom := {31,25,18,13,31}
       ENDIF

       DO CASE
       CASE cEvaInd == '2'
	    aNroCol := {32,32,32,32,32}
	    aLenNom := {26,26,26,26,26}
       ENDCASE
*>>>>FIN DETERMINACION DE COLS Y LONGITUD DE NOMBRE

*>>>>SELECCION DE LA COLUMNA SEGUN INDICADORES
       DO CASE
       CASE cEvaInd == '0'
	    nIndMin := IF(nNroInd > 10,10,nNroInd)
	    nNroCol := IF(nIndMin < 7,aNroCol[5],aNroCol[nIndMin-6])
	    nMaxInd := 10

       CASE cEvaInd == '1'
	    nIndMin := IF(nNroInd > 10,10,nNroInd)
	    nNroCol := IF(nIndMin < 7,aNroCol[5],aNroCol[nIndMin-6])
	    nMaxInd := 10

       CASE cEvaInd == '2'
	    nIndMin := IF(nNroInd > 4,4,nNroInd)  // Indicadores a Visualizar
	    nNroCol := aNroCol[nIndMin]
	    nMaxInd := 4

       ENDCASE
*>>>>FIN SELECCION DE LA COLUMNA SEGUN INDICADORES

*>>>>CALCULO DE LA LONGITUD DEL NOMBRE
       DO CASE
       CASE cEvaInd == '2'
	    nLenNom := aLenNom[nIndMin]
       OTHERWISE
	    nLenNom := IF(nIndMin < 7,aLenNom[5],aLenNom[nIndMin-6])
       ENDCASE

       nFilIni := 06
       nColIni := nNroCol
       nNroFil := nFilIni
*>>>>FIN CALCULO DE LA LONGITUD DEL NOMBRE

*>>>>IMPRESION DE LOS ENCABEZADOS DE LAS NOTAS
       cTitul1 := IF(EMPTY(PRA->cTitul1Ano),'INDI',PRA->cTitul1Ano)
       cTitul1 := ALLTRIM(cTitul1)

       i := 1
       DO WHILE i <= nIndMin
	  @ nNroFil,nNroCol SAY IF(i<10,cTitul1,SUBS(cTitul1,1,3))+LTRIM(STR(i))
	  i++
	  DO CASE
	  CASE cEvaInd == '0'
	       nNroCol += 6  // 4:Indicador+1:Nota+1:Espacio

	  CASE cEvaInd == '1'
	       nNroCol += 6  // 4:Indicador+1:Nota+1:Espacio

	  CASE cEvaInd == '2'
	       nNroCol += 9  // 4:Indicador+4:Nota+1:Espacio

	  ENDCASE
       ENDDO

       IF lHayNot
	  @ nNroFil,nNroCol SAY IF(lNotNum,'NOTA','JUIC')
       ELSE
	  @ nNroFil,nNroCol SAY IF(cEvaInd == '2','NOTA','')
       ENDIF

       DO CASE
       CASE cEvaInd == '2'
	    nNroCol = nNroCol - (i-1)*9
       OTHERWISE
	    nNroCol = nNroCol - (i-1)*6
       ENDCASE

       nNroFil = nNroFil + 1
      *Ubica la fila primera de grabacion

       cSavCab := SAVESCREEN(nNroFil-1,0,nNroFil-1,79)
       cSavLin := SAVESCREEN(21,0,22,79)
*>>>>FIN IMPRESION DE LOS ENCABEZADOS DE LAS NOTAS

*>>>>GRABACION DE LAS NOTAS DE LOS ESTUDIANTES
       lOtrEst := .T.
       DO WHILE lOtrEst

**********CAPTURA DEL NUMERO DE LA LISTA
	    Centrar('ENTRE EL NUMERO DE LISTA DEL ESTUDIANTE '+;
		    'A MODIFICAR',21)
	    cNroLis := SPACE(03)
	    @ nNroFil,01 GET cNroLis  PICT "999"
	    READ
	    RESTSCREEN(21,0,22,79,cSavLin)

	    IF EMPTY(cNroLis)
	       lOtrEst := .F.
	       LOOP
	    ENDIF
**********FIN CAPTURA DEL NUMERO DE LA LISTA

**********VALIDACION DEL NUMERO DE LA LISTA
	    SELECT NOP
	    IF VAL(cNroLis) >  NOP->(RECCOUNT())
	       cError('NO HAY NINGUN ESTUDIANTE CON ESTE NUMERO '+;
		      'DE LISTA')
	       LOOP
	    ENDIF
	    GO VAL(cNroLis)
	   *Ubicacion en el registro del estudiante a modificar
**********FIN VALIDACION DEL NUMERO DE LA LISTA

**********VISUALIZACION DATOS DEL ESTUDIANTE
	    cCodigoTes := NOP->cCodigoEst
	    IF !lSekCodigo(cCodigoTes,cMaeAct)
	       cError('No Existe Estudiante Cod: '+cCodigoTes)
	    ENDIF
	    cNombreTes = RTRIM(&cMaeAct->cApelliEst)+' '+;
			 RTRIM(&cMaeAct->cNombreEst)
**********FIN VISUALIZACION DATOS DEL ESTUDIANTE

**********VALIDACION DEL ESTUDIANTE SI ESTA RETIRADO
	    lHayErr := .T.
	    DO CASE
	    CASE &cMaeAct->lRetiroEst .AND. nNroPer > &cMaeAct->nPerRetEst
		 cError(SUBS(cNombreTes,1,nLenNom)+' ESTA RETIRADO')

	    CASE nTipCarTca == 0 .AND. NOP->lRetiroNoP
		 cError(SUBS(cNombreTes,1,nLenNom)+' PERTENECE A OTRO GRUPO')

	    CASE nTipCarTca # 0 .AND. NOP->lRetiroNoP
		 cError(SUBS(cNombreTes,1,nLenNom)+' FUE RETIRADO DE LA LISTA')

	    OTHERWISE
		 lHayErr :=.F.
	    ENDCASE

	    IF lHayErr
	       LOOP
	    ENDIF
	    @ nNroFil,05 SAY SUBS(cNombreTes,1,nLenNom)  PICT '@A'
**********FIN VALIDACION DEL ESTUDIANTE SI ESTA RETIRADO

**********LECTURA DE LAS NOTAS ACTUALES
	    DO CASE
	    CASE cEvaInd == '0'
		 cLogros := SUBS(&cCamInd,1,nNroInd*nLenInd)
		 cNotInd := SUBS(&cCamNtI,1,nNroInd*nLenNtI)

	    CASE cEvaInd == '1'
		 cLogros := SUBS(&cCamInd,1,nNroInd*nLenInd)
		 cNotInd := SUBS(&cCamNtI,1,nNroInd*nLenNtI)

	    CASE cEvaInd == '2'
		 cLogros := SUBS(&cCamInd,1,nNroInd*nLenInd)
		 cNotInd := SUBS(&cCamNtI,1,nNroInd*nLenNtI)

	    ENDCASE
	    cNotDef := &cCamDef

	    DisplayLog(nNroFil,nColIni,cEvaInd,;
		       cLogros,cNotInd,nNroInd,;
		       lHayNot,cNotDef)
**********FIN LECTURA DE LAS NOTAS ACTUALES

**********VALIDACION DE LOS LOGROS APLICADOS
	    IF EMPTY(cLogros)
	       cError('NO APLICADO LOS LOGROS')
	       CLOSE ALL
	       RETURN NIL
	    ENDIF
**********FIN VALIDACION DE LOS LOGROS APLICADOS

**********ANALISIS DEL CAMBIO DE LA FILA
	    IF lOtrFil
	       IF i > 10
		  nNroFil--
		  nNroCol = nColIni
	       ENDIF
	    ENDIF
**********FIN ANALISIS DEL CAMBIO DE LA FILA

**********IMPRESION DEL GRUPO
	    @ nNroFil,01 SAY STR(NOP->(RECNO()),3) PICT '999'
**********FIN IMPRESION DEL GRUPO

**********LOCALIZACION DEL GRUPO
	    IF !lLocCodigo('cCodigoGru','GRU',&cMaeAct->cCodigoGru)
	       cError('EL GRUPO '+&cMaeAct->cCodigoGru+' NO EXISTE')
	       lOtrEst := .F.
	       LOOP
	    ENDIF
	    cJvfCal := SUBS(GRU->cJvfCalGru,nNroPer*3-2,3)
	    cJvfCal := IF(MAT->lJvfCalMat,SPACE(03),cJvfCal)
	    lJvfCal := IF(EMPTY(cJvfCal),.F.,.T.)
**********FIN LOCALIZACION DEL GRUPO

**********CAPTURA Y VALIDACION DE LOS LOGROS
	    IF !lLeeEva(cNomSis,@nNroFil,nColIni,nNroPer,lIndNiv,cEvaInd,;
			cCodigoTgr,cCodigoTni,nNroInd,nNroLog,;
			@cLogros,@cNotInd,;
			IF(lHayNot .AND. lJvfCal,.F.,lHayNot),;
			lNotNum,@cNotDef,cValNot,cSavLin,lLeeRec,;
			NIL,@nColDef)
	       lOtrEst := .F.

	       LOOP
	    ENDIF
	    IF nNroLog == 0
	       RESTSCREEN(21,0,22,79,cSavLin)
	    ENDIF
**********FIN CAPTURA Y VALIDACION DE LOS LOGROS

**********CORREGIR LAS NOTAS
	    IF !lPregunta('Desea Grabar? Si No')

	       IF nNroInd > 10
		  nNroFil--
		  nNroCol := nColIni
	       ENDIF

	       LOOP
	    ELSE
	       RESTSCREEN(21,0,22,79,cSavLin)
	    ENDIF
**********FIN CORREGIR LAS NOTAS

**********CALCULO DEL JUICIO VALORATIVO
	    DO CASE
	    CASE lJvfCal .AND. cJvfCal == '201'

		 cNotDef := cJvfCal201(@cLogros,@cNotInd,nNroPer)+SPACE(03)
	       *�Promedio de logros. Dos Niveles

		 @ nNroFil,nColDef SAY cNotDef


	    CASE lJvfCal .AND. cJvfCal == '301'

		 IF nNroPer < nTotPer
		    cNotDef := cJvfCal301(@cLogros,@cNotInd,nNroPer)+SPACE(03)
		 ENDIF
	       *�Promedio de logros. Tres Niveles


		 @ nNroFil,nColDef SAY cNotDef


	    CASE lJvfCal .AND. cJvfCal == '302'

		 IF nNroPer < nTotPer
		    cNotDef := cJvfCal302(@cLogros,@cNotInd,nNroPer)+SPACE(03)
		 ENDIF
	       *�Promedio de logros. Tres Niveles. variable

		 @ nNroFil,nColDef SAY cNotDef

	    ENDCASE
**********FIN CALCULO DEL JUICIO VALORATIVO

**********CAMPO DE INDICADORES EN NOTAS
	    cIndic1 := NOP->cIndic1NoP
	    cIndic2 := NOP->cIndic2NoP
	    cIndic3 := NOP->cIndic3NoP
	    cIndic4 := NOP->cIndic4NoP
	    cIndic5 := NOP->cIndic5NoP

	    // Indice de Cumplimiento
	    IF lJvfCal .AND. (cJvfCal == '102' .OR. cJvfCal == '103')

*==============LECTURA DE LOS CAMPOS DE NOTAS
		 cCamIndNot := cCamJuiNot(lShared,cAnoUsr,cMaeAct,;
					  PathUno+'\'+cPatSis,nNroPer,;
					  &cMaeAct->cCodigoGru,;
					  cCodigoTma,lMatVar,;
					  @cCamNotDef,@cCamNotRec,;
					  @cCamNtINot)
*==============FIN LECTURA DE LOS CAMPOS DE NOTAS

*===============LOCALIZACION DEL ESTUDIANTE EN NOTAS
		  lEstNot := lLocCodigo('cCodigoEst','NOT',cCodigoTes)
		  IF !lEstNot
		     cError('EL CODIGO:'+cCodigoTes+' NO EXISTE EN '+;
			    'NOTAS DEL GRUPO:'+&cMaeAct->cCodigoGru)
		  ENDIF
*===============FIN LOCALIZACION DEL ESTUDIANTE EN NOTAS

*===============CAMPO DE INDICADORES
		  IF lEstNot

		     FOR j := 1 TO 5

			 cCamIndNot := STUFF(cCamIndNot,8,1,STR(j,1))
			*Campo para cade periodo

			 DO CASE
			 CASE j == 1

			      IF nNroPer == 1
				 cIndic1 := SPACE(LEN(&cCamIndNot))
				 cIndic1 := STUFF(cIndic1,1,nNroInd*nLenInd,cLogros)
			      ELSE
                                 cIndic1 := &cCamIndNot
			      ENDIF

			 CASE j == 2

			      IF nNroPer == 2
				 cIndic2 := SPACE(LEN(&cCamIndNot))
				 cIndic2 := STUFF(cIndic2,1,nNroInd*nLenInd,cLogros)
			      ELSE
				 cIndic2 := &cCamIndNot
			      ENDIF

			 CASE j == 3

			      IF nNroPer == 3
				 cIndic3 := SPACE(LEN(&cCamIndNot))
				 cIndic3 := STUFF(cIndic3,1,nNroInd*nLenInd,cLogros)
			      ELSE
				 cIndic3 := &cCamIndNot
			      ENDIF

			 CASE j == 4

			      IF nNroPer == 4
				 cIndic4 := SPACE(LEN(&cCamIndNot))
				 cIndic4 := STUFF(cIndic4,1,nNroInd*nLenInd,cLogros)
			      ELSE
				 cIndic4 := &cCamIndNot
			      ENDIF

			 CASE j == 5

			      IF nNroPer == 5
				 cIndic5 := SPACE(LEN(&cCamIndNot))
				 cIndic5 := STUFF(cIndic5,1,nNroInd*nLenInd,cLogros)
			      ELSE
				 cIndic5 := &cCamIndNot
			      ENDIF

			 ENDCASE

		     ENDFOR

		  ENDIF
		  CloseDbf('NOT')
*===============FIN CAMPO DE INDICADORES

	    ENDIF
**********FIN CAMPO DE INDICADORES EN NOTAS

**********GRABAR LAS NOTAS
	    SELECT NOP
	    IF NOP->(lRegLock(lShared,.F.))

	       REPLACE &cCamInd WITH ;
		       STUFF(&cCamInd,1,nNroInd*nLenInd,cLogros)

	       REPLACE &cCamNtI WITH ;
		       STUFF(&cCamNtI,1,nNroInd*nLenNtI,cNotInd)
	      *Notas n�mericas de los indicadores de las recuperciones

*=============CALCULO DEL JUICIO VALORATIVO
		DO CASE
		CASE lJvfCal .AND. cJvfCal == '102'

		     cNotDef := cJvfCal102(cIndic1,;
					   cIndic2,;
					   cIndic3,;
					   cIndic4,;
					   cIndic5,;
					   nNroPer,nTotPer)+SPACE(03)
		   *�Indice de Cumplimiento. La Salle Bucaramaga

		     @ nNroFil,nColDef SAY cNotDef

		CASE lJvfCal .AND. cJvfCal == '103'

		     cNotDef := cJvfCal103(cIndic1,;
					   cIndic2,;
					   cIndic3,;
					   cIndic4,;
					   cIndic5,;
					   nNroPer,nTotPer)+SPACE(03)
		   *�Indice de Cumplimiento. La Salle Bucaramaga.Variable

		     @ nNroFil,nColDef SAY cNotDef

		ENDCASE
*=============FIN CALCULO DEL JUICIO VALORATIVO

	       REPLACE &cCamDef WITH cNotDef

	       REPL NOP->cNomUsrNop WITH cNomUsr
	       REPL NOP->dFecUsrNop WITH DATE()
	       REPL NOP->cHorUsrNop WITH TIME()

	       NOP->(DBCOMMIT())
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

		 @ nNroFil,01 SAY 'NRO'
		 @ nNroFil,05 SAY 'NOMBRE'
		 RESTSCREEN(nNroFil,0,nNroFil,79,cSavCab)
*--------------IMPRESION DE LOS ENCABEZADOS

	       SELECT NOP
	       nNroCol := nColIni
	       nNroFil++

	    ENDIF
**********FIN INCREMENTO DE LAS FILAS

       ENDDO
       CloseAll()
       RETURN NIL
*>>>>FIN GRABACION DE LAS NOTAS DE LOS ESTUDIANTES