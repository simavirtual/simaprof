/* SIMA - CONTABILIDAD PROFESORES

MODULO      : EVALUAR
SUBMODULO...: APLICAR

**************************************************************************
* TITULO..: APLICACION DE LOS LOGROS A EVALUAR                           *
**************************************************************************

AUTOR: Nelson Fern�ndez G�mez       FECHA DE CREACION: NOV 11/1996 DOM A
       Colombia, Bucaramanga        INICIO: 05:20 PM   NOV 11/1996 DOM

OBJETIVOS:

1- Permite aplicar logros que se van a evaluar

2- Retorna NIL

*------------------------------------------------------------------------*
*                              PROGRAMA                                  *
*------------------------------------------------------------------------*/

FUNCTION Profe_307(aP1,aP2,aP3)

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

       LOCAL       i := 0                   // Contador
       LOCAL nNroFil := 0                   // N�mero de Fila
       LOCAL nNroCol := 0                   // N�mero de Columna
       LOCAL nFilSup := 0                   // Columna Superior
       LOCAL nColSup := 0                   // Columna Superior
       LOCAL nInfFil := 0                   // Fila Inferior
       LOCAL nInfCol := 0                   // Columna Inferior
       LOCAL nFilDif := 0                   // Fila para la Dificultad
       LOCAL nFilRec := 0                   // Fila para la Recuperaci�n
       LOCAL cMsgTxt := ''                  // Texto variable
       LOCAL nNroPer := 0                   // N�mero del periodo
       LOCAL nTotPer := 5                   // Total de Periodos
       LOCAL nPerAct := 0                   // Periodo Actual
       LOCAL nTotInd := 20                  // Total de Indicadores
       LOCAL nNroInd := 0                   // N�mero de indicadores
       LOCAL cLogros := ''                  // Logros aplicados
       LOCAL cNotInd := ''                  // Nota de los Indicadores
       LOCAL cDificu := ''                  // Dificultades aplicados
*      LOCAL cLogCaP := ''                  // Campo de logros en Carga del Profesor. Marca Error el la Macro al declararse local
*      LOCAL cDifCaP := ''                  // Campo de Dificultades en Carga del Profesor. Marca Error el la Macro al declararse local
*      LOCAL cRecCaP := ''                  // Campo de Recomendaciones en Carga del Profesor. Marca Error el la Macro al declararse local
       LOCAL cRecome := ''                  // Recomendaciones aplicados
       LOCAL lSalir  := .F.                 // .T. Salir del Programa
       LOCAL lIndNiv := .F.                 // .T. Indicadores por nivel
       LOCAL lEvaInd := .F.                 // .T. Evaluar el indicador
       LOCAL lHayNot := .F.                 // .T. Hay Notas
       LOCAL lNotNum := .F.                 // .T. Nota Numerica .F. Nota Cualitativa
       LOCAL cValNot := ''                  // Validaci�n de la nota
       LOCAL nNroLog := 0                   // N�mero de logros
       LOCAL nNroDif := 0                   // N�mero de Dificultades asociadas al logro
       LOCAL cNotDef := ''                  // Nota Definitiva
       LOCAL nLenNtI := 8                   // Longitud del C�digo de la Nota de la Recuperaci�n del Indicador
       LOCAL GetList := {}                  // Variable del Sistema
     *�Variables espec�ficas 

       LOCAL cCodigoTpr := ''               // C�digo del Profesor
       LOCAL cCodigoTgr := ''               // C�digo del Grupo
       LOCAL cCodigoTni := ''               // C�digo del Nivel
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
       IF !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FConTbl+cMaeACt+ExtFile,'TCO',NIL,lShared) .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+;
		       FilePro+cAnoSis+ExtFile,'PRO',NIL,lShared) .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FileMat,'MAT',NIL,lShared)                 .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FileInd+cMaeAct+cAnoSis+ExtFile,'IND',;
		       PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FNtxInd+cMaeAct+cAnoSis+cExtNtx,lShared)   .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FileCla,'CLA',NIL,lShared)                 .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FileEva,'EVA',NIL,lShared)                 .OR.;
	  !lUseDbf(.T.,cPatPro+'\'+;
		       FCarPro+cAnoSis+ExtFile,'CAP',NIL,lShared)
	  cError('ABRIENDO ARCHIVOS')
	  CLOSE ALL
	  RETURN NIL
       ENDIF
*>>>>FIN SELECION DE LAS AREAS DE TRABAJO

*>>>>VALIDACION DE CONTENIDOS DE ARCHIVOS
       lHayErr := .T.
       DO CASE
       CASE TCO->(RECCOUNT()) == 0
	    cError('NO EXISTEN CONFIGURACION PARA EL NIVEL EN LA CONTABILIDAD')

       CASE PRO->(RECCOUNT()) == 0
	    cError('NO EXISTEN PROFESORES GRABADOS')

       CASE MAT->(RECCOUNT()) == 0
	    cError('NO EXISTEN MATERIAS GRABADAS')

       CASE IND->(RECCOUNT()) == 0
	    cError('NO EXISTEN INDICADORES DE LA CONTABILIDAD')

       CASE CLA->(RECCOUNT()) == 0
	    cError('NO EXISTEN LAS CLASES DE LOGOS')

       CASE EVA->(RECCOUNT()) == 0
	    cError('NO EXISTEN LOS CODIGOS DE EVALUACION')

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
*>>>>FIN SELECCION DE LA MATERIA DEL GRUPO

*>>>>LECTURA DEL NUMERO DEL BIMESTRE
       nTotPer := TCO->nTotPerTbl
       nPerAct := IF(EMPTY(PRO->nNroPerPro),TCO->nPerActTbl,PRO->nNroPerPro)
       nNroPer := nLeeNroPer(nFilInf+1,nColInf,nFilPal,nTotPer,nPerAct,.T.)
       IF nNroPer == 0
	  CLOSE ALL
	  RETURN NIL
       ENDIF
*>>>>FIN ANALISIS PARA LA SALIDA DEL PROGRAMA

*>>>>VALIDACION DE LA ACTIVACION DE LA EVALUACION
       IF SUBS(TCO->cEvaIndTbl,nNroPer,1) # '1' .AND.;
	  SUBS(TCO->cEvaIndTbl,nNroPer,1) # '2'
	  cError('NO ESTA ACTIVADA LA EVALUACION DEL INDICADOR')
	  CloseAll()
	  RETURN NIL
       ENDIF
*>>>>FIN VALIDACION DE LA ACTIVACION DE LA EVALUACION

*>>>>CONSULTA DE LOS LOGROS RELACIONADOS
       IF lPregunta('DESEA CONSULTAR LOS LOGROS RELACIONADOS? No Si')
	  VerLogDif(nFilPal,nNroPer)
	  CLOSE ALL
	  RETURN NIL
       ENDIF
*>>>>FIN CONSULTA DE LOS LOGROS RELACIONADOS

*>>>>LECTURA DEL NUMERO DEL INDICADORES
       nNroInd := nLeeNroInd(lShared,nFilInf+1,nColInf,nNroPer,nTotInd)
       IF nNroInd == 0
	  CLOSE ALL
	  RETURN NIL
       ENDIF
*>>>>FIN LECTURA DEL NUMERO DEL INDICADORES

*>>>>SELECION DE LAS NOTAS A GRABAR
       nNroFil := nMarco(nFilPal+1,'GRUPO: '+SUBS(CAP->cCodigoCar,5,4)+'. '+;
				   ALLTRIM(MAT->cNombreMat),22,'�')
       nNroCol := 1
       @ nNroFil++,01 SAY 'LOGROS'

       i := 1
       DO WHILE i <= IF(nNroInd >10,10,nNroInd)
	  @ nNroFil,nNroCol SAY IF(i<10,'INDI','IND')+LTRIM(STR(i))
	  i++
	  nNroCol := nNroCol + 6
       ENDDO

       nNroFil = nNroFil + 1
      *Ubica la fila primera de grabacion
*>>>>FIN SELECION DE LAS NOTAS A GRABAR

*>>>>INICIALIZACION DE LAS VARIABLES
       cCodigoTgr := SUBS(CAP->cCodigoCar,5,4)
       cCodigoTni := SUBS(cCodigoTgr,1,2)

       lIndNiv := TCO->lIndNivTbl
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

       lEvaInd := IF(SUBS(TCO->cEvaIndTbl,nNroPer,1)='1',.T.,.F.)
       lHayNot := .F.
       nNroLog := 0

       cLogCaP := 'CAP->cLogro'+STR(nNroPer,1,0)+'CaP'
       cDifCaP := 'CAP->cDific'+STR(nNroPer,1,0)+'CaP'
       cRecCaP := 'CAP->cRecom'+STR(nNroPer,1,0)+'CaP'
       cNotInd := SUBS(SPACE(160),1,nNroInd*nLenNtI)
       cNotDef := SPACE(04)
       cSavLin := SAVESCREEN(21,0,22,79)
*>>>>FIN INICIALIZACION DE LAS VARIABLES

*>>>>LECTURA Y VALIDACION DE LOS LOGROS
       DO WHILE .T.

**********LECTURA DE LOS LOGROS
	    cLogros := SUBS(&cLogCaP,1,nNroInd*5)
	    cNotInd := SUBS(SPACE(160),1,nNroInd*nLenNtI)

	    IF !lLeeLogro(cNomSis,nNroFil,1,nNroPer,lIndNiv,'00000',;
			  cCodigoTgr,cCodigoTni,nNroInd,nNroLog,;
			  @cLogros,cNotInd,lHayNot,lNotNum,;
			  @cNotDef,cValNot,cSavLin)
	       CLOSE ALL
	       RETURN NIL
	    ENDIF
**********FIN LECTURA DE LOS LOGROS

**********VALIDACION DE LOS LOGROS
	    lSalir := .T.
	    FOR i := 1 TO  nNroInd

		IF EMPTY(SUBS(cLogros,i*5-4,5))

		   cError('DEBE APLICAR TODOS LOS INDICADORES')
		   IF !lPregunta('DESEA CONTINUAR? No Si')
		      lSalir := .T.
		      CLOSE ALL
		      RETURN NIL
		   ELSE
		      lSalir := .F.
		   ENDIF

		   EXIT
		ENDIF

	    ENDFOR
	    IF !lSalir
	       LOOP
	    ENDIF
	    EXIT
**********FIN VALIDACION DE LOS LOGROS

       ENDDO
*>>>>FIN LECTURA Y VALIDACION DE LOS LOGROS

*>>>>LECTURA DEL TOTAL DE DIFICULTADES
       IF TCO->nNroDifTbl # 0
	  nNroFil += 2
	  cMsgTxt := 'RELACIONES POR LOGRO: '
	  @ nNroFil,01 SAY cMsgTxt

	  nNroDif := nLeeTotDif(lShared,nNroFil,;
				LEN(cMsgTxt),nNroPer,TCO->nNroDifTbl)

	  @ nNroFil,LEN(cMsgTxt) SAY STR(nNroDif,2) COLOR("I")
       ENDIF
*>>>>FIN LECTURA DEL TOTAL DE DIFICULTADES

*>>>>LECTURA DE LAS RELACIONES PARA CADA LOGRO
       nNroFil += 2
       FOR i := 1 TO nNroInd

***********LECTURA DE LAS DIFICULTADES
	     IF !EMPTY(TCO->cReeUnoTbl) .AND. TCO->nNroDifTbl # 0
		@ nNroFil,01 SAY 'RELACIONES PARA EL LOGRO: '+;
				 SUBS(cLogros,i*5-4,5)

		nFilDif := nNroFil+2
		@ nFilDif++,01 SAY ALLTRIM(TCO->cReeUnoTbl)

		cDificu := SUBS(&cDifCaP,i*25-24,25)
		IF !lLeeLogro(cNomSis,nFilDif,1,nNroPer,lIndNiv,'00000',;
			      cCodigoTgr,cCodigoTni,nNroDif,nNroLog,;
			      @cDificu,cNotInd,lHayNot,lNotNum,;
			      @cNotDef,cValNot,cSavLin)
		   CLOSE ALL
		   RETURN NIL
		ENDIF

	     ENDIF
***********FIN LECTURA DE LAS DIFICULTADES

***********LECTURA DE LAS RECOMENDACIONES
	     IF !EMPTY(TCO->cReeDosTbl) .AND. TCO->nNroDifTbl # 0

		nFilRec := nFilDif+2
		@ nFilRec++,01 SAY ALLTRIM(TCO->cReeDosTbl)
		cRecome := SUBS(&cRecCaP,i*25-24,25)
		IF !lLeeLogro(cNomSis,nFilRec,1,nNroPer,lIndNiv,'00000',;
			      cCodigoTgr,cCodigoTni,nNroDif,nNroLog,;
			      @cRecome,cNotInd,lHayNot,lNotNum,@cNotDef,;
			      cValNot,cSavLin)
		   CLOSE ALL
		   RETURN NIL
		ENDIF

	     ENDIF
***********FIN LECTURA DE LAS RECOMENDACIONES

***********ANALISIS DE DECISION
	     IF !lPregunta('DESEA GRABAR? Si No')
		CLOSE ALL
		RETURN NIL
	     ENDIF
***********FIN ANALISIS DE DECISION

***********GRABAR LOS LOGROS
	     SELECT CAP
	     IF CAP->(lRegLock(lShared,.F.))

		REPLACE &cLogCaP WITH ;
			STUFF(&cLogCaP,1,nNroInd*5,cLogros)

		IF TCO->nNroDifTbl == 0
		   CAP->(DBCOMMIT())
		   EXIT
		ENDIF

		IF !EMPTY(TCO->cReeUnoTbl)
		   REPLACE &cDifCaP WITH ;
			   STUFF(&cDifCaP,i*25-24,25,cDificu)
		ENDIF

		IF !EMPTY(TCO->cReeDosTbl)
		   REPLACE &cRecCaP WITH ;
			   STUFF(&cRecCaP,i*25-24,25,cRecome)
		ENDIF
		CAP->(DBCOMMIT())

	     ELSE
		cError('NO SE GRABA EL REGISTRO')
	     ENDIF

	     IF lShared
		CAP->(DBUNLOCK())
	     ENDIF
***********FIN GRABAR LOS LOGROS

       ENDFOR
       CLOSE ALL
       RETURN NIL
*>>>>FIN LECTURA DE LAS RELACIONES PARA CADA LOGRO

***************************************************************************
*-------------------- DECLARACION DE LAS FUNCIONES -----------------------*
**************************************************************************/

/*************************************************************************
* TITULO DE LA FUNCION..: CONSULTAR LA RELACION DE LOS LOGROS            *
**************************************************************************

AUTOR: Nelson Fern�ndez G�mez       FECHA DE CREACION: ABR 28/98 MAR A
       Colombia, Bucaramanga        INICIO: 10:30 PM   ABR 28/98 MAR

OBJETIVOS:

1)- Permite consultar las dificultades o recomendaciones asociadas a
    cada logro.

2)- Retorna Nil

SINTAXIS:


*------------------------------------------------------------------------*
*                              PROGRAMA                                  *
*------------------------------------------------------------------------*/

FUNCTION VerLogDif(nFilPal,nNroPer)

*>>>>PARAMETROS DE LA FUNCION
/*     nFilPal                              // Fila Principal
       nNroPer                              // N�mero del Periodo. */
*>>>>FIN PARAMETROS DE LA FUNCION

*>>>>DECLARACION DE VARIABLES
       LOCAL     i,j := 0                   // Contadores
       LOCAL nNroFil := 0                   // N�mero de Fila
       LOCAL nNroCol := 0                   // N�mero de Columna
       LOCAL nNroInd := 0                   // N�mero de Indicadores
       LOCAL nNroDif := 0                   // N�mero de Dificultades
       LOCAL cLogCaP := ''                  // Campo con Logros
       LOCAL cDifCaP := ''                  // Campo con Dificultades
       LOCAL cRecCaP := ''                  // Campo con Recomendaciones
       LOCAL cLogros := ''                  // Logros aplicados
       LOCAL cDificu := ''                  // Dificultadas asociadas
       LOCAL cRecome := ''                  // Recomendaciones asociadas
*>>>>FIN DECLARACION DE VARIABLES

*>>>>IMPRESION DE TITULOS
       nNroFil := nMarco(nFilPal+1,'GRUPO: '+SUBS(CAP->cCodigoCar,5,4)+'. '+;
				   ALLTRIM(MAT->cNombreMat),22,'�')

       @ nNroFil,01 SAY 'No. LOGROS->'

       IF !EMPTY(TCO->cReeUnoTbl) .AND. TCO->nNroDifTbl # 0
	  @ nNroFil,14 SAY ALLTRIM(TCO->cReeUnoTbl)+'->'
       ENDIF

       IF !EMPTY(TCO->cReeDosTbl) .AND. TCO->nNroDifTbl # 0
	  @ nNroFil,47 SAY ALLTRIM(TCO->cReeDosTbl)
       ENDIF
       nNroFil++
*>>>>FIN IMPRESION DE TITULOS

*>>>>CONSULTAR LAS DIFICULTADES ASOCIADAS A LOS LOGROS
	nNroInd := VAL(SUBS(CAP->cNroIndCaP,nNroPer*2-1,2))
	nNroDif := VAL(SUBS(CAP->cNroDifCaP,nNroPer*2-1,2))
	cLogCaP := 'CAP->cLogro'+STR(nNroPer,1,0)+'CaP'
	cDifCaP := 'CAP->cDific'+STR(nNroPer,1,0)+'CaP'
	cRecCaP := 'CAP->cRecom'+STR(nNroPer,1,0)+'CaP'

	cLogros := SUBS(&cLogCaP,1,nNroInd*5)
	FOR i := 1 TO nNroInd
	    @ nNroFil,01 SAY STR(i,2)
	    @ nNroFil,05 SAY SUBS(&cLogCap,i*5-4,5) COLOR("I")

	   IF !EMPTY(TCO->cReeUnoTbl) .AND. TCO->nNroDifTbl # 0
	      cDificu := SUBS(&cDifCaP,i*25-24,25)
	      nNroCol := 14
	      FOR j := 1 TO nNroDif
		  @ nNroFil,nNroCol SAY SUBS(cDificu,j*5-4,5) COLOR("I")
		  nNroCol := nNroCol + 6
	      ENDFOR
	   ENDIF

	   IF !EMPTY(TCO->cReeDosTbl) .AND. TCO->nNroDifTbl # 0
	      cRecome := SUBS(&cRecCaP,i*25-24,25)
	      nNroCol := 47
	      FOR j := 1 TO nNroDif
		  @ nNroFil,nNroCol SAY SUBS(cRecome,j*5-4,5) COLOR("I")
		  nNroCol := nNroCol + 6
	      ENDFOR
	   ENDIF

	   nNroFil++
	   IF nNroFil > 19
	      cPausa()
	      nNroFil := nMarco(nFilPal+1,'GRUPO: '+;
					  SUBS(CAP->cCodigoCar,5,4)+'. '+;
					  ALLTRIM(MAT->cNombreMat),22,'�')

	      @ nNroFil,01 SAY 'No. LOGROS->'

	      IF !EMPTY(TCO->cReeUnoTbl) .AND. TCO->nNroDifTbl # 0
		 @ nNroFil,14 SAY ALLTRIM(TCO->cReeUnoTbl)+'->'
	      ENDIF

	      IF !EMPTY(TCO->cReeDosTbl) .AND. TCO->nNroDifTbl # 0
		 @ nNroFil,47 SAY ALLTRIM(TCO->cReeDosTbl)
	      ENDIF
	      nNroFil++
	   ENDIF

	ENDFOR

	cPausa()
*>>>>FIN CONSULTAR LAS DIFICULTADES ASOCIADAS A LOS LOGROS

