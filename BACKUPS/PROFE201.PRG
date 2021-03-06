/* SIMA - CONTABILIDAD PROFESORES

MODULO      : LOGROS
SUBMODULO...: GRABAR

**************************************************************************
* TITULO..: GRABACION DE LOS LOGROS                                      *
**************************************************************************

AUTOR: Nelson Fern�ndez G�mez       FECHA DE CREACION: SEP 14/96 SAB
       Colombia, Bucaramanga        INICIO: 10:50 AM   SEP 14/96 SAB

OBJETIVOS:

1- Permite grabar los logros para cada estudiante

2- Retorna NIL

*------------------------------------------------------------------------*
*                              PROGRAMA                                  *
*------------------------------------------------------------------------*/

FUNCTION Profe_201(aP1,aP2,aP3)

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

       LOCAL lIndNiv := .F.                 // .T. Indicadores de Nivel
       LOCAL cEvaInd := ''                  // Evaluar el Indicador
       LOCAL lHayNot := .F.                 // .T. Hay nota definitiva
       LOCAL lNotNum := .F.                 // .T. Nota N�merica .F. Nota Cualitativa
       LOCAL cValNot := ''                  // Validaci�n de la nota

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
       LOCAL cCamDef := ''                  // Campo de definitivas
       LOCAL cCamObs := ''                  // Campo de Observaciones
       LOCAL cNotDef := ''                  // Nota Definitiva

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
       LOCAL cLogros := ''                  // Logros aplicados
       LOCAL cNotInd := ''                  // Nota de los Indicadores
       LOCAL nRegMat := 0                   // Registro de la Materia
       LOCAL nLenCod := 6                   // Longitud del C�digo
       LOCAL nTipPla := 0                   // Tipo del Plan
       LOCAL cJvfCal := ''                  // Juicio Valorativo Calculado
       LOCAL lJvfCal := .F.                 // .T. Juicio Valorativo Calculado
       LOCAL cTitul1 := ''                  // T�tulo1 para encabezado de captura
       LOCAL GetList := {}                  // Variable del Sistema
     *�Variables espec�ficas 

       LOCAL cCodigoTpr := ''               // C�digo del Profesor
       LOCAL cCodigoTes := ''               // C�digo del estudiante
       LOCAL cNombreTes := ''               // Nombre del estudiante
       LOCAL cCodigoTma := ''               // C�digo de la Materia
       LOCAL cNombreTma := ''               // Nombre de la Materia
       LOCAL cCodigoTgr := ''               // C�digo del grupo
       LOCAL cCodigoTni := ''               // C�digo del nivel
       LOCAL cCodigoTcl := ''               // C�digo Clase del indicador
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
		       FileGru+cMaeAct+cAnoSis+ExtFile,'GRU',NIL,lShared) .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FileMat,'MAT',NIL,lShared)                 .OR.;
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
		       FConTbl+cMaeACt+ExtFile,'TCO',NIL,lShared) .OR.;
	  !lUseDbf(.T.,cPatSis+'\'+;
		       fSprAno+cAnoUsr+ExtFile,'PRA',NIL,lShared) .OR.;
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

       CASE GRU->(RECCOUNT()) == 0
	    cError('NO EXISTEN GRUPOS GRABADOS')

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
*>>>>FIN SELECCION DE LA MATERIA DEL GRUPO

*>>>>ANALISIS DEL TIPO DE PLAN ACADEMICO
       nTipPla := 0
       IF !('XXXX' $ cCodigoTgr)
	  nTipPla := nTipoPlan(SUBS(cCodigoTgr,1,2))
	  IF nTipPla == 0
	     CloseAll()
	     RETURN NIL
	  ENDIF
       ENDIF
*>>>>FIN LOCALIZACION DEL NIVEL

*>>>>SELECION DE LAS AREAS DE TRABAJO
       IF !('XXXX' $ cCodigoTgr) .AND. nTipPla == 1
	  IF !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
			  cFilePlan(cAnoUsr,cCodigoTgr),'PLA',NIL,lShared)
	     cError('ABRIENDO ARCHIVO DEL PLAN ACADEMICO')
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
       CASE !('XXXX' $ cCodigoTgr) .AND. nTipPla == 1 .AND.;
	    PLA->(RECCOUNT()) == 0
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
       IF !('XXXX' $ cCodigoTgr) .AND. nTipPla == 1
	  IF !lLocCodigo('cCodigoMat','PLA',MAT->cCodigoMat)
	     cError('LA MATERIA: '+cCodigoTma+;
		     'NO EXISTE EN PLAN DE: '+cCodigoTgr)
	     CLOSE ALL
	     RETURN NIL
	  ENDIF
       ENDIF
       MAT->(DBGOTO(nRegMat))
*>>>>FIN LOCALIZACION DE LA MATERIA EN PLAN

*>>>>SELECCION DEL METODO DE GRABACION
       SELECT NOP
       SUM ALL NOP->nGruTraNoP TO nTotGru
       GO TOP
       lNroLis := .T.

       IF nTotGru # 0

	  cSavPan := SAVESCREEN(0,0,24,79)

	  aMenus  := {"1<POR ORDEN DE LISTA>",;
		      "2<POR ORDEN DE GRUPO>"}
	  aAyuda  := {'Permite Grabar los logros por orden de lista.',;
		      'Permite Grabar los logros por orden de grupo.'}

	  nOpcion := nMenu(aMenus,aAyuda,nFilInf+1,;
			   IF(nColInf+23>78,nColInf-7,nColInf),;
			   NIL,NIL,1,.F.)

	  RESTSCREEN(0,0,24,79,cSavPan)
	  IF nOpcion == 0
	     CLOSE ALL
	     RETURN NIL
	  ENDIF

	  lNroLis := .T.
	  IF nOpcion == 2
	     lNroLis := .F.
	  ENDIF

       ENDIF
*>>>>FIN SELECCION DEL METODO DE GRABACION

*>>>>INDEXACION DEL ARCHIVO DE NOTAS DEL PROFESOR
       IF !lNroLis

**********INDEXACCION
	    SELECT NOP
	    fNtxNoP := cFileTempo(cPatPro+'\',cCodigoTgr,cExtNtx)
	    INDEX ON NOP->nGruTraNoP TO &fNtxNoP
	    CloseDbf('NOP')
**********FIN INDEXACCION

**********SELECION DE LAS AREAS DE TRABAJO
	    IF !lUseDbf(.T.,cPatPro+'\'+;
			    CAP->cCodigoCar+ExtFile,'NOP',fNtxNoP,lShared)
	       cError('ABRIENDO ARCHIVO DE NOTAS DEL PROFESOR GRUPO '+cCodigoTgr)
	       CLOSE ALL
	       RETURN NIL
	    ENDIF
**********FIN SELECION DE LAS AREAS DE TRABAJO

       ENDIF
       SELECT NOP
       GO TOP
       nRegNoP = RECNO()
*>>>>FIN INDEXACION DEL ARCHIVO DE NOTAS DEL PROFESOR

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
       cCamDef := 'NOP->cDefin'+STR(nNroPer,1,0)+'NoP'
       cCamObs := 'NOP->cObsev'+STR(nNroPer,1,0)+'NoP'
*>>>>FIN ANALISIS PARA LA SALIDA DEL PROGRAMA

*>>>>LECTURA DE LA CONFIGURACION DEL NIVEL DE LA CONTABILIDAD
       lIndNiv := TCO->lIndNivTbl
       cEvaInd := SUBS(TCO->cEvaIndTbl,nNroPer,1)

       lHayNot := IF(SUBS(TCO->cHayNotTbl,nNroPer,1)='1',.T.,.F.)
       IF TCO->lNotPlaTbl
	  IF !('XXXX' $ cCodigoTgr)
	     lHayNot := IF(SUBS(PLA->cHayNotPla,nNroPer,1)='1',.T.,.F.)
	  ENDIF
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

*>>>>LECTURA DEL NUMERO DEL INDICADORES
       nNroInd := nLeeNroInd(lShared,nFilInf+1,nColInf,nNroPer,nTotInd)
       IF nNroInd == 0
	  CLOSE ALL
	  RETURN NIL
       ENDIF
*>>>>FIN LECTURA DEL NUMERO DEL INDICADORES

*>>>>IMPRESION DE LOS ENCABEZADOS
       nNroFil := nMarco(nFilPal+1,ALLTRIM(cNombreTma),22,'�')
       @ nNroFil,01 SAY IF(lNroLis,'NRO','GRU')
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

       nIndMin := IF(nNroInd >10,10,nNroInd)
       nNroCol := IF(nIndMin < 7,aNroCol[5],aNroCol[nIndMin-6])
      *Escoge la columna seg�n el n�mero de notas

       nLenNom := IF(nIndMin < 7,aLenNom[5],aLenNom[nIndMin-6])
      *Escoge la longitud del nombre

       nFilIni := 06
       nColIni := nNroCol
       nNroFil := nFilIni
*>>>>FIN DETERMINACION DE COLS Y LONGITUD DE NOMBRE

*>>>>SELECION DE LAS NOTAS A GRABAR
       cSavLin := SAVESCREEN(21,0,22,79)
       Centrar('<ESC> Graba Horizontalmente. '+;
	       '<ENTER> Graba Verticalmente seg�n la elecci�n',21)

       cTitul1 := IF(EMPTY(PRA->cTitul1Ano),'INDI',PRA->cTitul1Ano)
       cTitul1 := ALLTRIM(cTitul1)

       i := 1
       DO WHILE i <= nIndMin
	  @ nNroFil,nNroCol PROMPT IF(i<10,cTitul1,SUBS(cTitul11,3))+LTRIM(STR(i))
	  i++
	  nNroCol := nNroCol + 6
       ENDDO
       MENU TO nNroLog

       IF lHayNot
	  @ nNroFil,nNroCol SAY IF(lNotNum,'NOTA','JUIC')
       ENDIF

       nNroCol = nNroCol - (i-1)*6
       nNroFil = nNroFil + 1
      *Ubica la fila primera de grabacion

       cSavCab := SAVESCREEN(nNroFil-1,0,nNroFil-1,79)
       RESTSCREEN(21,0,22,79,cSavLin)
*>>>>FIN SELECION DE LAS NOTAS A GRABAR

*>>>>LOCALIZACION ULTIMA NOTA GRABADA
       cNoNota = SPACE(nLenCod)
       SELECT NOP

       IF nNroLog == 0

**********BUSQUEDA SOBRE TODOS LOS LOGROS
	    DO WHILE .NOT. NOP->(EOF())

*--------------BUSQUEDA DEL ESTUDIANTE QUE LE FALTA LOGROS
		 FOR i := 1 TO nNroInd

*                   ANALISIS SI TIENE LOGRO APLICADO
		      nInicio := nPosIni(i,nLenCod)

		      IF SUBS(&cCamInd,nInicio,nLenCod) == cNoNota
			 lAllNot := .F.
			 EXIT
		      ELSE
			 lAllNot := .T.
		      ENDIF
*                   FIN ANALISIS SI TIENE LOGRO APLICADO

		 NEXT
*--------------FIN BUSQUEDA DEL ESTUDIANTE QUE LE FALTA LOGROS

*--------------ANALISIS PARA LA SALIDA DE LA BUSQUEDA
		 IF .NOT. lAllNot
		    EXIT
		 ENDIF
*--------------FIN ANALISIS PARA LA SALIDA DE LA BUSQUEDA

	       NOP->(DBSKIP())

	    ENDDO
**********FIN BUSQUEDA SOBRE TODOS LOS LOGROS

       ELSE

**********BUSQUEDA SOBRE EL NUMERO DE LOGRO ESCOGIDO
	    nInicio := nPosIni(nNroLog,nLenCod)
	   *Determina la posicion de la nota en el bimestre

	    LOCATE FOR SUBS(&cCamInd,nInicio,6) == cNoNota

	    IF .NOT. FOUND()
	       lAllNot := .T.
	    ELSE
	       lAllNot := .F.
	    ENDIF
**********FIN BUSQUEDA SOBRE EL NUMERO DE LOGRO ESCOGIDO

       ENDIF
*>>>>FIN LOCALIZACION ULTIMA NOTA GRABADA

*>>>>ANALISIS SI EXISTEN NOTAS GRABADAS
       IF nRegNoP == RECNO()
	  lExiNot := .F.
       ELSE
	  lExiNot := .T.
          SKIP-1
         *Ubica el estudiante ultimo a imprimir
       ENDIF
*>>>>FIN ANALISIS SI EXISTEN NOTAS GRABADAS

*>>>>ANALISIS SI ESTAN TODAS LAS NOTAS GRABADAS
       IF lAllNot
	  IF nNroLog == 0
	     cError('TODOS LOS LOGROS YA ESTAN GRABADOS')
	  ELSE
	     cError('LOS LOGROS No.'+STR(nNroLog,2)+' YA ESTAN GRABADOS')
	  ENDIF
	  lOtrEst := .F.
       ELSE
	  lOtrEst := .T.
       ENDIF
*>>>>FIN ANALISIS SI ESTAN TODAS LAS NOTAS GRABADAS

*>>>>IMPRESION DE LOS DATOS DEL ESTUDIANTE POR GRABAR O ULTIMO GRABADO
       IF !lAllNot

***********IMPRESION DEL NUMERO DE LISTA O GRUPO
	     @ nNroFil,01 SAY IF(lNroLis,STR(RECNO(),2),;
					 NOP->nGruTraNoP) PICT '99'
***********FIN IMPRESION DEL NUMERO DE LISTA O GRUPO

***********IMPRESION DEL NOMBRE DEL ESTUDIANTE
	     cCodigoTes := NOP->cCodigoEst

	     IF !lSekCodigo(cCodigoTes,cMaeAct)
		cNombreTes := 'No Existe Estudiante Cod: '+cCodigoTes
	     ENDIF
	   *�B�squeda del c�digo del estudiante

	     cNombreTes := RTRIM(&cMaeAct->cApelliEst)+' '+;
			   RTRIM(&cMaeAct->cNombreEst)

	     @ nNroFil,05 SAY SUBS(cNombreTes,1,nLenNom)  PICT '@A'
	     SELECT NOP
***********FIN IMPRESION DEL NOMBRE DEL ESTUDIANTE

**********IMPRESION DE LOS LOGROS Y NOTA DEFINITIVA
	    DisplayLog(@nNroFil,nColIni,cEvaInd,;
		       &cCamInd,cNotInd,nNroInd,;
		       lHayNot,&cCamDef)
**********FIN IMPRESION DE LOS LOGROS Y NOTA DEFINITIVA

       ENDIF
*>>>>FIN IMPRESION DE LOS DATOS DEL ESTUDIANTE POR GRABAR O ULTIMO GRABADO

*>>>>UBICACION DEL REGISTRO DEL ESTUDIANTE A GRABAR
      IF lExiNot .AND. !lAllNot
	 SKIP
	 nNroFil++
	*Ubica la fila segunda de la grabaci�n
      ELSE
	 IF nNroInd > 10
	    nNroFil--
	    nNroCol := nColIni
	 ENDIF
      ENDIF
*>>>>FIN UBICACION DEL REGISTRO DEL ESTUDIANTE A GRABAR

*>>>>GRABACION DE LAS NOTAS DE LOS ESTUDIANTES
       lHayErr := .F.
       cSavLin := SAVESCREEN(21,0,22,79)
       DO WHILE lOtrEst

**********LECTURA DE LAS NOTAS ACTUALES
	    IF !lHayErr
	       cLogros := SUBS(&cCamInd,1,nNroInd*nLenCod)
	       cNotDef := &cCamDef
	    ENDIF
**********FIN LECTURA DE LAS NOTAS ACTUALES

**********IMPRESION DE LOS LOGROS Y NOTA DEFINITIVA
	    IF !lHayErr
	       DisplayLog(nNroFil,nColIni,cEvaInd,;
			  cLogros,cNotInd,nNroInd,;
			  lHayNot,cNotDef)
	    ENDIF
**********FIN IMPRESION DE LOS LOGROS Y NOTA DEFINITIVA

**********DETERMINACION DE LA COLUNA DE CAPTURA
	    IF nNroLog # 0
	       nNroCol := nColIni + (nNroLog-1)*6
	    ENDIF
**********FIN DETERMINACION DE LA COLUNA DE CAPTURA

**********ANALISIS DEL TOTAL DE INDICADORES A LEER
	    IF nNroLog == 0
	       nTotInd := nNroInd
	    ELSE
	       nTotInd := 1
	    ENDIF
**********FIN ANALISIS DEL TOTAL DE INDICADORES A LEER

**********IMPRESION DEL GRUPO
	    @ nNroFil,01 SAY IF(lNroLis,STR(NOP->(RECNO()),2),;
					NOP->nGruTraNoP) PICT '99'
**********FIN IMPRESION DEL GRUPO

**********IMPRESION DEL NOMBRE DEL ESTUDIANTE
	    cCodigoTes := NOP->cCodigoEst
	    IF !lSekCodigo(cCodigoTes,cMaeAct)
	       cNombreTes := 'No Existe Estudiante Cod: '+cCodigoTes
	    ENDIF
	    cNombreTes = RTRIM(&cMaeAct->cApelliEst)+' '+;
			 RTRIM(&cMaeAct->cNombreEst)

	    @ nNroFil,05 SAY SUBS(cNombreTes,1,nLenNom)  PICT '@A'
**********FIN IMPRESION DEL NOMBRE DEL ESTUDIANTE

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
	    IF !lLeeLogro(cNomSis,@nNroFil,nColIni,nNroPer,lIndNiv,;
			  cEvaInd,cCodigoTgr,cCodigoTni,nNroInd,;
			  nNroLog,@cLogros,cNotInd,;
			  IF(lHayNot .AND. lJvfCal,.F.,lHayNot),;
			  lNotNum,@cNotDef,cValNot,cSavLin)
	       lOtrEst := .F.
	       LOOP
	    ENDIF
	    IF nNroLog == 0
*	       RESTSCREEN(21,0,22,79,cSavLin)
	    ENDIF
**********FIN CAPTURA Y VALIDACION DE LOS LOGROS

**********CORREGIR LAS NOTAS
	    IF !lPregunta('Desea Grabar? Si No')
	       lHayErr := .T.
	       IF nNroInd > 10
		  nNroFil--
		  nNroCol := nColIni
	       ENDIF
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
		  REPLACE &cCamInd WITH ;
			  STUFF(&cCamInd,1,nNroInd*nLenCod,cLogros)

*-----------------CALCULO DEL JUICIO VALORATIVO
		    DO CASE
		    CASE lJvfCal .AND. cJvfCal == '101'

			 cNotDef := cIndicJv01(NOP->cIndic1NoP,;
					       NOP->cIndic2NoP,;
					       NOP->cIndic3NoP,;
					       NOP->cIndic4NoP,;
					       NOP->cIndic5NoP,;
					       nNroPer)+SPACE(03)
		    ENDCASE
*-----------------FIN CALCULO DEL JUICIO VALORATIVO

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
*	    nNroFil := nNroFil + IF(nNroLog # 0 .AND. nNroInd > 10,2,1)
	    nNroFil++
	    IF nNroFil > nFilFin

*--------------IMPRESION DE LOS ENCABEZADOS
		 nNroFil := nMarco(nFilPal+1,ALLTRIM(cNombreTma),22,'�')

		 @ nNroFil,01 SAY IF(lNroLis,'NRO','GRU')
		 @ nNroFil,05 SAY 'NOMBRE'
		 RESTSCREEN(nNroFil,0,nNroFil,79,cSavCab)
*--------------IMPRESION DE LOS ENCABEZADOS

*--------------IMPRESION DE LOS DATOS DEL ULTIMO ESTUDIANTE
		 nNroFil := nFilIni + 1
		 @ nNroFil,01 SAY IF(lNroLis,STR(RECNO(),2),;
					     NOP->nGruTraNoP) PICT '99'
		 @ nNroFil,05 SAY SUBS(cNombreTes,1,nLenNom) PICT '@A'
		 SELECT NOP
*--------------FIN IMPRESION DE LOS DATOS DEL ULTIMO ESTUDIANTE

*--------------IMPRESION DE LOS LOGROS DEL ULTIMO ESTUDIANTE
		 DisplayLog(@nNroFil,nColIni,cEvaInd,;
			    &cCamInd,cNotInd,nNroInd,;
			    lHayNot,&cCamDef)
		 nNroCol := nColIni
		 nNroFil++
*--------------FIN IMPRESION DE LA NOTA DEFINITIVA

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
**********FIN UBICACION DEL REGISTRO DEL PROXIMO ESTUDIANTE

       ENDDO

       CLOSE ALL
       IF !lNroLis
	  lFileDelete(fNtxNoP)
       ENDIF
       RETURN NIL
*>>>>FIN GRABACION DE LAS NOTAS DE LOS ESTUDIANTES


