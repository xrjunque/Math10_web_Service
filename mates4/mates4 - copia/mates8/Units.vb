Imports System.Text.RegularExpressions

Public Class Units
    'Table 1: The prefix symbols	 	 	 	 
    'name print	c/s	c/i  value      (c/s=case sensitive, c/i=case insensitive)
    'yotta 	Y 	Y	YA	1 × 10^24 
    'zetta 	Z 	Z	ZA	1 × 10^21 
    'exa 	E 	E	EX	1 × 10^18 
    'peta 	P 	P	PT	1 × 10^15 
    'tera 	T 	T	TR	1 × 10^12 
    'giga 	G 	G	GA	1 × 10^9 
    'mega 	M 	M	MA	1 × 10^6 
    'kilo 	k 	k	K	1 × 10^3 
    'hecto 	h 	h	H	1 × 10^2 
    'deka 	da 	da	DA	1 × 10^1 
    'deci 	d 	d	D	1 × 10^-1 
    'centi 	c 	c	C	1 × 10^-2 
    'milli 	m 	m	M	1 × 10^-3 
    'micro 	μ 	u	U	1 × 10^-6 
    'nano 	n 	n	N	1 × 10^-9 
    'pico 	p 	p	P	1 × 10^-12 
    'femto 	f 	f	F	1 × 10^-15 
    'atto 	a 	a	A	1 × 10^-18 
    'zepto 	z 	z	ZO	1 × 10^-21 
    'yocto 	y 	y	YO	1 × 10^-24 
    Friend Shared preCS() As String = {"Y", "Z", "E", "P", "T", _
                                "G", "M", "k", "h", "da", _
                                "d", "c", "m", "u", "n", _
                                "p", "f", "at", "z", "y"}
    'Shared preCI() As String = {"YA", "ZA", "EX", "PT", "TR", _
    '                            "GA", "MA", "K", "H", "DA", _
    '                            "D", "C", "M", "U", "N", _
    '                            "P", "F", "A", "ZO", "YO"}
    Friend Shared preValue() As Double = {10 ^ 24, 10 ^ 21, 10 ^ 18, 10 ^ 15, 10 ^ 12, _
                                10 ^ 9, 10 ^ 6, 10 ^ 3, 10 ^ 2, 10, _
                                10 ^ -1, 10 ^ -2, 10 ^ -3, 10 ^ -6, 10 ^ -9, _
                                10 ^ -12, 10 ^ -15, 10 ^ -18, 10 ^ -21, 10 ^ -24}
    ' name	 kind       print c/s c/i
    '        of 	
    '        quantity	
    '--------------------------------
    'meter 	length 	    m 	m	M
    'second time 	    s 	s	S
    'gram 	mass 	    g 	g	G
    'radian plane angle rad	rad	RAD
    'Kelvin temperature K 	K	K
    'Coulomb electric 
    '        charge	    C 	C	C
    'candela luminous 
    '        intensity 	cd 	cd	CD
    Friend Shared qCS() As String = {"m", "s", "g", "rad", "K", "C", "cd", "c"}
    Friend Shared sqCS = "(?<varfnducs>(" + Join(qCS, "|") + ")+)"
    'Friend Shared qCI() As String = {"M", "S", "G", "RAD", "K", "C", "CD"}
    'Friend Shared sqCI = "(?<varfnduci>(" + Join(qCI, "|") + ")+)"

    'Table 4: SI units							
    'name	kind of quantity  print c/s	c/i	M	Definition
    'mole 	amount of Substance mol mol MOL yes	6.0221367*10^23 
    'steradian 	solid angle 	sr 	sr 	SR 	yes	rad^2 
    'Hertz 	frequency 	        Hz 	Hz 	HZ 	yes	s^-1 
    'Newton force 	            N 	N 	N 	yes	kg*m/s^2 
    'Pascal pressure 	        Pa 	Pa 	PAL yes	N/m2 
    'Joule 	energy 	            J 	J 	J 	yes	N*m 
    'Watt 	power 	            W 	W 	W 	yes	J/s 
    'Ampère electric current 	A 	A 	A 	yes	C/s 
    'Volt 	electric potential 	V 	V 	V 	yes	J/C 
    'Farad 	electric capacitanceF 	F 	F 	yes	C/V 
    'Ohm 	electric resistance Ω 	Ohm OHM yes	V/A 
    'Siemenselectric conductanceS 	S 	SIE yes	Ohm^-1 
    'Weber 	magentic flux    	Wb 	Wb 	WB 	yes	V*s 
    'degree Celsius temperature °C 	Cel CEL yes	K + 273.15
    'Tesla 	magnetic flux densityT 	T 	T 	yes	Wb/m^2 
    'Henry 	inductance 	        H 	H 	H 	yes	Wb/A 
    'lumen 	luminous flux 	    lm 	lm 	LM 	yes	cd*sr 
    'lux 	illuminance 	    lx 	lx 	LX 	yes	lm/m^2 
    'Becquerel 	radioactivity 	Bq 	Bq 	BQ 	yes	s^-1 
    'Gray 	energy dose 	    Gy 	Gy 	GY 	yes	J/kg 
    'Sievert dose equivalent 	Sv 	Sv 	SV 	yes	J/kg 
    Friend Shared uCS() As String = {"mol", "sr", "Hz", "N", "Pa", _
                              "J", "W", "A", "V", "F", _
                              "Ohm", "S", "Wb", "Cel", "T", _
                              "H", "lm", "lx", "Bq", "Gy", _
                              "Sv"}
    'Friend Shared uCI() As String = {"mol", "SR", "HZ", "N", "PAL", _
    '                          "J", "W", "A", "V", "F", _
    '                          "OHM", "SIE", "WB", "CEL", "T", _
    '                          "H", "LM", "lX", "BQ", "GY", _
    '                          "SV"}
    Shared unitVal() As String = { _
                "6.0221367e23", "rad^2", "c/s", "kg*m/s^2", "N/m^2", _
                              "N/m", "J/s", "C/s", "J/C", "C/V", _
                              "V/A", "1/Ohm", "V*s", "K+273.15", "Wb/m^2", _
                              "Wb/A", "cd*sr", "lm/m^2", "1/s", "J/kg", _
                              "J/kg"}
    Friend Shared vUCS As String = "(?<varucs>(?<preucs>" + _
       Join(preCS, "|") + "){1}(" + Join(qCS, "|") + "|" + Join(uCS, "|") + ")" + _
       "|(" + Join(qCS, "|") + "|" + Join(uCS, "|") + "))"

    'Friend Shared sUCI As String = "(?<varuci>" + Join(uCI, "|") + ")"
    Friend Shared oVar As VarsAndFns
    Shared Sub New()
        Try
            oVar = New VarsAndFns(Config.cfg)
            Dim i As Int32
            For i = 0 To qCS.Length - 1
                oVar.AddVarDoNotValidate(qCS(i), Nothing)
            Next
            For i = 0 To unitVal.Length - 1
                oVar.AddVarDoNotValidate(uCS(i), _
                    Expression.parseExpression(unitVal(i)))
            Next
        Catch ex As Exception

        End Try
    End Sub
    Shared Function findUnits(polyToSearch As Polynomial) As String
        Dim e1 As String = String.Empty
        Static oVar As New VarsAndFns(Units.oVar)
        Dim polyToSearch1 As New Polynomial(polyToSearch)
        Dim i As Int32
        Dim vVar(-1) As String
        Dim vPrefix(-1) As String
        Dim bDetail As Boolean
        Try
            'vVar = polyToSearch1.varAll
            'Dim vV2(vVar.Length - 1) As String
            'For i = 0 To vVar.Length - 1
            '    Dim m As Match = Regex.Match( _
            '       vVar(i), vUCS)
            '    If m.Groups("preucs").Success Then
            '        ReDim Preserve vPrefix(i)
            '        vPrefix(i) = m.Groups("preucs").ToString
            '        ' remove K,M,m,... (MHz, Km, mm,...) prefix:
            '        vV2(i) = Mid(vVar(i), _
            '                      Len(vPrefix(i)) + 1)
            '    Else
            '        vV2(i) = vVar(i)
            '    End If
            'Next
            'polyToSearch1.setVars(vV2)
            'polyToSearch1 = supressInternalVars(New Expression( _
            '       polyToSearch1), oVar).getPolynomial
            bDetail = Config.cfg.bDetail
            Config.cfg.bDetail = False
            Dim idC As Int32 = oVar.getVarIDByName("c")
            oVar.setValue(idC, New ExprMatrix(1.0))
            Dim pS As Polynomial = _
                polyToSearch1.evalMultiCjoToExpr(oVar).getPolynomial
            For i = 0 To qCS.Length - 1
                Dim iPoly As Polynomial = Polynomial.GetPolynomial(qCS(i))
                If (polyToSearch1 / iPoly).isReal Then
                    e1 = qCS(i)
                    Exit Try
                End If
            Next
            For i = 0 To uCS.Length - 1
                Dim iPoly As Polynomial = Polynomial.GetPolynomial(uCS(i))
                iPoly = supressInternalVars( _
                    New Expression(iPoly), oVar).getPolynomial
                If Not iPoly.isReal Then
                    Dim iDiv As Polynomial = polyToSearch1 / iPoly
                    iDiv = supressInternalVars( _
                        New Expression(iDiv), oVar).getPolynomial
                    If iDiv.isReal Then
                        e1 = uCS(i)
                        Exit Try
                    End If
                    Dim iMul As Polynomial = polyToSearch1 * iPoly
                    iMul = iMul.evalMultiCjoToExpr(oVar).getPolynomial
                    If iMul.isReal Then
                        If uCS(i) = "S" Then
                            e1 = "Ohm"
                        ElseIf uCS(i) = "Ohm" Then
                            e1 = "S" ' Siemens
                        Else
                            e1 = uCS(i) + "^-1"
                        End If
                        Exit Try
                    End If
                End If
            Next

        Catch ex As Exception
            Throw ex
        End Try
        If i < vPrefix.Length Then
            e1 = vPrefix(i) + e1
        End If
        Config.cfg.bDetail = False
        Return e1
    End Function
    Private Shared Function supressInternalVars(expr As Expression, _
                        oVar As VarsAndFns) As Expression
        Static intVar As VarsAndFns = Nothing
        Dim i As Int32
        If intVar Is Nothing Then
            intVar = New VarsAndFns(oVar)
            If intVar.getNamesList.Length > 1 Then
                For i = intVar.getNamesList.Length - 1 To 1 Step -1
                    For j As Int32 = 0 To intVar.getNamesList.Length - 1
                        Dim eM As ExprMatrix = intVar.getValueByID(i)
                        If eM IsNot Nothing Then
                            Dim expr1 As Expression = New Expression(eM.getCurExpr)
                            expr1 = expr1.evalExprToExpr(intVar)
                            intVar.setValue(i, New ExprMatrix(expr1))
                        End If
                    Next
                Next
            End If
        End If
        Return expr.evalExprToExpr(intVar)
    End Function

    Shared Function getPrefix(ByRef db As Double) As String
        Dim sPre As String = String.Empty
        Try
            Dim sgn As Int32 = Math.Sign(db)
            db = Math.Abs(db)
            Dim j As Int32
            If db >= 1 Then
                For j = 1 To Units.preValue.Length - 1
                    If Math.Log10(Units.preValue(j)) Mod 3 = 0 Then
                        Dim db1 As Double = db / Units.preValue(j)
                        If db1 >= 1 AndAlso db1 < 1000 Then
                            Exit For
                        End If
                    End If
                Next
                If j < Units.preValue.Length Then
                    db /= Units.preValue(j)
                    sPre = Units.preCS(j)
                End If
            Else
                For j = 1 To Units.preValue.Length - 1
                    If Math.Log10(Units.preValue(j)) Mod 3 = 0 Then
                        Dim db1 As Double = db / Units.preValue(j)
                        If db1 >= 1 AndAlso db1 < 1000 Then
                            Exit For
                        End If
                    End If
                Next
                If j < Units.preValue.Length Then
                    db /= Units.preValue(j)
                    sPre = Units.preCS(j)
                End If
            End If
            db *= sgn
        Catch ex As Exception
            Throw ex
        End Try
        Return sPre
    End Function
    Public Shared Function ToStringPrefixes() As String
        Dim e1 As String = String.Empty
        Try
            e1 = Join(New String() {
            "Name  Prefix Value", _
            "----  ------ -----", _
            "yotta 	Y     10^24 ", _
            "zetta 	Z     10^21 ", _
            "exa  	E     10^18 ", _
            "peta 	P     10^15 ", _
            "tera 	T     10^12 ", _
            "giga 	G     10^9 ", _
            "mega 	M     10^6 ", _
            "kilo 	k     10^3 ", _
            "hecto 	h     10^2 ", _
            "deka 	da    10^1 ", _
            "deci 	d     10^-1 ", _
            "centi 	c     10^-2 ", _
            "milli 	m     10^-3 ", _
            "micro 	u     10^-6 ", _
            "nano 	n     10^-9 ", _
            "pico 	p     10^-12 ", _
            "femto 	f     10^-15 ", _
            "atto 	a     10^-18 ", _
            "zepto 	z     10^-21 ", _
            "yocto 	y     10^-24 " _
            }, _
            vbCrLf)

        Catch ex As Exception
            Throw ex
        End Try
        Return e1
    End Function
    Public Shared Function ToStringFundamentalMagnitudes() As String
        Dim e1 As String = String.Empty
        Try
            e1 = Join(New String() { _
            "Name    (Physical Magnitude) Nomenclature ", _
            "----    -------------------- -----", _
            "meter   (length)              m", _
            "second  (time)                s", _
            "gram    (mass)                g", _
            "radian  (plane angle)        rad", _
            "Kelvin  (temperature)         K", _
            "Coulomb (electric charge)     C", _
            "candela (luminous intensity)  cd" _
            }, _
            vbCrLf)

        Catch ex As Exception
            Throw ex
        End Try
        Return e1
    End Function
    Public Shared Function reduceUnits(expr As Expression) As Expression
        Dim retExpr As Expression = Nothing
        Try
            Dim unit As Expression
            Dim sUnit As String = Units.findUnits(expr.getPolynomial)
            If Len(sUnit) Then
                unit = New Expression( _
                    Polynomial.GetPolynomial(sUnit))
                Dim db As Double = _
                    expr.getPolynomial.An.pRe.ToDouble
                If db = 0 AndAlso _
                expr.getPolynomial.PolyResto IsNot Nothing Then
                    db = expr.getPolynomial.PolyResto.An.pRe.ToDouble
                End If
                Dim sPre As String = Units.getPrefix(db)
                Dim sPrefixAndUnit As String = sPre + sUnit
                unit = New Expression( _
                   db * Polynomial.GetPolynomial(sPrefixAndUnit))
                retExpr = unit
            Else
                retExpr = New Expression(expr)
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return retExpr
    End Function
    Public Shared Sub initUnitVars(ByRef oVars As VarsAndFns)
        Dim bDetail As Boolean
        Try
            bDetail = Config.cfg.bDetail
            oVars.AddVar("kg", _
               New Expression( _
                New Polynomial(1000) * Polynomial.GetPolynomial("g")))
            For i As Int32 = 0 To Units.oVar.getNamesList.Length - 1
                Dim eMtx As ExprMatrix = _
                    Units.oVar.getValueByID(i)
                Dim expr As Expression = Nothing
                If eMtx IsNot Nothing Then
                    expr = eMtx.getExpr(0, 0)
                    expr = Units.reduceUnits(expr)
                End If
                oVars.AddVarDoNotValidate( _
                    Units.oVar.getVarNameByID(i), _
                    expr)
            Next
            Config.cfg.bDetail = bDetail
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
End Class
