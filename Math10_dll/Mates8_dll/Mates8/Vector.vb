Public Class Vector
    Public Sub New()
        vPoly = New Polynomial() {New Polynomial(0.0)}
    End Sub


    Friend vPoly() As Polynomial
    Dim dbMod As Double
    Public Sub New(dbl As Double)
        vPoly = New Polynomial() {New Polynomial(dbl)}
    End Sub
    Public Sub New(ByVal Re As Double, ByVal Im As Double)
        vPoly = New Polynomial() {New Polynomial(Re, Im)}
    End Sub
    Public Sub New(ByVal cjo As Complex)
        vPoly = New Polynomial() {New Polynomial(cjo)}
    End Sub
    Public Sub New(ByVal polyA As Polynomial)
        vPoly = New Polynomial() {New Polynomial(polyA)}
    End Sub
    Public Sub New(ByVal Va As Vector)
        Try
            ReDim vPoly(Va.vPoly.Length - 1)
            Dim i As Int64
            For i = 0 To Va.vPoly.Length - 1
                vPoly(i) = New Polynomial(Va.vPoly(i))
            Next
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
    End Sub
    Public Sub New(ByVal vDbl() As Double)
        ReDim vPoly(vDbl.Length - 1)
        Dim i As Int64
        Try
            For i = 0 To vDbl.Length - 1
                vPoly(i) = New Polynomial(vDbl(i))
            Next
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
    End Sub
    Public Sub New(ByVal vCjo() As Complex)
        ReDim vPoly(vCjo.Length - 1)
        Dim i As Int64
        Try
            For i = 0 To vCjo.Length - 1
                vPoly(i) = New Polynomial(vCjo(i))
            Next
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
    End Sub
    Public Property dbModuloInOperations() As Double
        Get
            Return dbMod
        End Get
        Set(ByVal value As Double)
            If dbMod <> value Then
                dbMod = value
                Dim i As Int64
                For i = 0 To Me.vPoly.Length - 1
                    vPoly(i).dbModuloInOperations = dbMod
                Next
            End If
        End Set
    End Property
    Public Overloads Shared Operator +(ByVal Va As Vector, ByVal Vb As Vector) As Vector
        Dim Vc As New Vector(0)
        Dim i As Int64
        Try
            If Va.vPoly.Length = Vb.vPoly.Length Then
                ReDim Vc.vPoly(Va.vPoly.Length - 1)
                For i = 0 To Vc.vPoly.Length - 1
                    Vc.vPoly(i) = Va.vPoly(i) + Vb.vPoly(i)
                Next
            Else
                Throw New Exception(msg8.msg(1013)) ' different vectors' lenghts
            End If
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
        Return Vc
    End Operator
    Public Overloads Shared Operator -(ByVal Va As Vector) As Vector
        Dim Vc As New Vector(0)
        Dim i As Int64
        Try
            ReDim Vc.vPoly(Va.vPoly.Length - 1)
            For i = 0 To Vc.vPoly.Length - 1
                Vc.vPoly(i) = -Va.vPoly(i)
            Next
        Catch ex As Exception
            Throw New Exception(msg8.msg(1003)) ' unknown error
        End Try
        Return Vc
    End Operator
    Public Overloads Shared Operator -(ByVal Va As Vector, ByVal Vb As Vector) As Vector
        Dim Vc As New Vector(0)
        Dim i As Int64
        Try
            If Va.vPoly.Length = Vb.vPoly.Length Then
                ReDim Vc.vPoly(Va.vPoly.Length - 1)
                For i = 0 To Vc.vPoly.Length - 1
                    Vc.vPoly(i) = Va.vPoly(i) - Vb.vPoly(i)
                Next
            Else
                Throw New Exception(msg8.msg(1013)) ' different vectors' lenghts
            End If
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
        Return Vc
    End Operator
    Public Shared Function opConjugate(ByVal Va As Vector) As Vector
        Dim Vc As New Vector(Va)
        Dim i As Int64
        For i = 0 To Vc.vPoly.Length - 1
            Vc.vPoly(i) = Polynomial.opConjugate(Vc.vPoly(i))
        Next
        Return Vc
    End Function
    Public Overloads Shared Operator *(ByVal Va As Vector, ByVal db As Double) As Vector
        Dim i As Int64
        Dim Vc As New Vector(Va)
        Try
            For i = 0 To Vc.vPoly.Length - 1
                Vc.vPoly(i) *= db
            Next
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
        Return Va * New Polynomial(db)
    End Operator
    Public Overloads Shared Operator *(ByVal db As Double, ByVal Va As Vector) As Vector
        Return Va * db
    End Operator
    Public Overloads Shared Operator *(ByVal Va As Vector, ByVal cjob As Complex) As Vector
        Return Va * New Polynomial(cjob)
    End Operator
    Public Overloads Shared Operator *(ByVal cjob As Complex, ByVal Va As Vector) As Vector
        Return Va * New Polynomial(cjob)
    End Operator
    Public Overloads Shared Operator *(ByVal Va As Vector, ByVal Pb As Polynomial) As Vector
        Dim Vc As New Vector(Va)
        Dim i As Int64
        Try
            For i = 0 To Va.vPoly.Length - 1
                Vc.vPoly(i) *= Pb
            Next
        Catch ex As Exception
            Throw New Exception(msg8.msg(1003)) ' unknown error
        End Try
        Return Vc
    End Operator
    Public Overloads Shared Operator *(ByVal Pb As Polynomial, ByVal Va As Vector) As Vector
        Return Va * Pb
    End Operator
    Public Overloads Shared Operator *(ByVal Va As Vector, ByVal Vb As Vector) As Vector
        Dim Vc As New Vector(0)
        Dim i As Int64
        Try
            If Va.vPoly.Length = 1 OrElse Vb.vPoly.Length = 1 Then
                If Va.vPoly.Length = 1 Then
                    ReDim Vc.vPoly(Vb.vPoly.Length - 1)
                    For i = 0 To Vb.vPoly.Length - 1
                        Vc.vPoly(i) = Va.vPoly(0) * Vb.vPoly(i)
                    Next
                ElseIf Vb.vPoly.Length = 1 Then
                    ReDim Vc.vPoly(Va.vPoly.Length - 1)
                    For i = 0 To Va.vPoly.Length - 1
                        Vc.vPoly(i) = Va.vPoly(i) * Vb.vPoly(0)
                    Next
                End If
            ElseIf Va.vPoly.Length = Vb.vPoly.Length Then
                ' Vc = Va * conjugate(Vb)
                ReDim Vc.vPoly(0)
                Vc.vPoly(0) = New Polynomial(0)

                Dim VbConj As Vector = Vector.opConjugate(Vb)
                'Dim VbNotConj As Vector = Vb
                For i = 0 To Va.vPoly.Length - 1
                    Vc.vPoly(0) += Va.vPoly(i) * VbConj.vPoly(i)
                Next

                'Dim VbConj As Vector = Vector.opConjugate(Vb)
                'For i = 0 To Va.vPoly.Length - 1
                '    ReDim Preserve Vc.vPoly(i)
                '    Vc.vPoly(i) = Va.vPoly(i) * VbConj.vPoly(i)
                'Next
            ElseIf Va.vPoly.Length = 1 Then
                ReDim Vc.vPoly(Vb.vPoly.Length - 1)
                For i = 0 To Vb.vPoly.Length - 1
                    Vc.vPoly(i) = Va.vPoly(0) * Vb.vPoly(i)
                Next
            ElseIf Vb.vPoly.Length = 1 Then
                ReDim Vc.vPoly(Va.vPoly.Length - 1)
                For i = 0 To Va.vPoly.Length - 1
                    Vc.vPoly(i) = Va.vPoly(i) * Vb.vPoly(0)
                Next
            Else
                Throw New Exception(msg8.msg(1013)) ' Different vectors' lengths
            End If
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
        Return Vc
    End Operator
    Public Overloads Shared Operator /(ByVal Va As Vector, ByVal Vb As Vector) As Vector
        Dim Vc As Vector = Nothing
        Dim i As Int64
        Try
            If Vb.IsComplex Then
                Vc = New Vector(Va)
                For i = 0 To Vc.vPoly.Length - 1
                    Vc.vPoly(i) /= Va.vPoly(0).cf(0)
                Next
            ElseIf Vb.IsPolynomial AndAlso Va.IsPolynomial Then
                Vc = New Vector(Va.vPoly(0) / Vb.vPoly(0))
            Else
                Throw New Exception(msg8.msg(1014)) ' divisor is a vector: operation n/a
            End If
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
        Return Vc
    End Operator
    Public Overloads Shared Operator ^(ByVal Va As Vector, ByVal Vb As Vector) As Vector
        Dim Vc As New Vector(0)
        Dim i As Int64
        Try
            Dim VbIsCjo As Boolean = Vb.IsComplex
            If Va.IsComplex AndAlso VbIsCjo Then
                Vc = New Vector(Va.vPoly(0).cf(0) ^ Vb.vPoly(0).cf(0))
            ElseIf Va.IsPolynomial AndAlso VbIsCjo Then
                Vc = New Vector(Va.vPoly(0) ^ Vb.vPoly(0))
            ElseIf VbIsCjo Then
                If Vb.vPoly(0).cf(0).pIm.IsZero Then
                    Dim exp As Double = Vb.vPoly(0).cf(0).pRe.ToDouble
                    If exp = Math.Floor(exp) Then
                        Vc = New Vector(Va)
                        If exp > 0 Then
                            For i = 2 To exp
                                Vc *= Va
                            Next
                        Else
                            Throw New Exception(msg8.msg(1015)) ' negative exponent: operation n/a
                        End If
                    Else
                        Throw New Exception(msg8.msg(1016)) ' non integer exponent: operation n/a
                    End If
                Else
                    Throw New Exception(msg8.msg(1017)) ' imaginary exponent: operation n/a
                End If
            Else
                Throw New Exception(msg8.msg(1018)) ' polynomial exponent: operation n/a
            End If
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
        Return Vc
    End Operator
    Public Function RootsToPolynomial() As Polynomial
        Dim polyC As New Polynomial(1.0)
        Dim i As Int64
        Try
            Dim polyX As Polynomial = Polynomial.GetPolynomial("x")
            For i = 0 To Me.vPoly.Length - 1
                polyC *= (polyX - New Polynomial(Me.vPoly(i).cf(0)))
            Next
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
        Return polyC
    End Function
    Public ReadOnly Property IsComplex() As Boolean
        Get
            If vPoly.Length = 1 AndAlso vPoly(0).isComplex Then
                Return True
            End If
            Return False
        End Get
    End Property
    Public ReadOnly Property IsPolynomial() As Boolean
        Get
            If vPoly.Length = 1 Then
                Return True
            End If
            Return False
        End Get
    End Property
    Public ReadOnly Property IsVector() As Boolean
        Get
            If vPoly.Length > 1 Then
                Return True
            End If
            Return False
        End Get
    End Property
    Public Sub sort(Optional ByVal bAsc As Boolean = True)
        Try
            Dim i As Int64
            For i = 0 To vPoly.Length - 1
                If vPoly(i).cf.Length > 1 Then
                    vPoly(i).opReduceCommonExponents() ' also does sort
                End If
            Next
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Public ReadOnly Property Var() As String()
        Get
            Dim iVars(-1) As String
            Dim i As Int64
            If vPoly.Length = 0 Then
                Return iVars
            End If
            iVars = vPoly(0).var
            For i = 1 To vPoly.Length - 1
                Dim vCur() As String = vPoly(i).var
                If iVars.Length = 0 Then
                    iVars = vCur
                ElseIf vCur.Length Then
                    Dim j As Int64
                    For j = 0 To vCur.Length - 1
                        Dim pos As Int64 = Array.IndexOf(iVars, vCur(j))
                        If pos = -1 Then
                            ReDim Preserve iVars(iVars.Length)
                            iVars(iVars.Length - 1) = vCur(j)
                        End If
                    Next
                End If
            Next
            Return iVars
        End Get
    End Property
    Public Function toStrVect(cfg As Config) As String
        Dim e1(vPoly.Length - 1) As String
        Try
            Dim i As Int64
            For i = 0 To vPoly.Length - 1
                e1(i) = vPoly(i).toStringPoly(cfg)
            Next
        Catch ex As Exception

        End Try
        Return Join(e1, " ;")
    End Function
    Public Function tovCjo() As Complex()
        Dim cjo(vPoly.Length - 1) As Complex
        Try
            Dim i As Int64
            For i = 0 To vPoly.Length - 1
                cjo(i) = vPoly(i).cf(0)
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return cjo
    End Function
    Public Function sortRoots() As Vector
        Dim roots() As Complex = Nothing
        Try
            roots = Me.tovCjo
            roots = Complex.sortRoots(roots)
        Catch ex As Exception

        End Try
        Return New Vector(Roots)
        'Dim sdb(Me.vPoly.Length - 1) As String
        'Dim cjo(Me.vPoly.Length - 1) As Complex
        'Dim re(-1) As Double
        'Dim cjo3(-1)
        'Dim retVector As New Vector(Me)
        'Try
        '    Dim i, k As Int64
        '    Dim i0(Me.vPoly.Length - 1) As Int64
        '    For i = 0 To Me.vPoly.Length - 1
        '        Dim c0 As Double = Me.vPoly(i).cf(0).pRe.ToDouble
        '        Dim c1 As Double = Me.vPoly(i).cf(0).pIm.ToDouble
        '        If c1 = 0 Then
        '            ReDim Preserve re(re.Length), i0(re.Length - 1)
        '            re(re.Length - 1) = c0

        '            'i0(i) = i comentado el 2013/07/29, sustituído por:
        '            i0(re.Length - 1) = i
        '        Else
        '            Dim alfa As Double = Math.Atan2(c1, Math.Abs(c0))
        '            Dim aux As Double = 100000 - Math.Floor(Math.Abs(alfa) * 10000)
        '            aux = aux * 10 + 2 + Math.Sign(c0)
        '            sdb(k) = ""
        '            If c1 Then
        '                sdb(k) = "1"
        '            End If
        '            Dim db As Double = Me.vPoly(i).cf(0).opModulo
        '            Dim abs As Double = Math.Abs(c0) ' Math.Floor(db)
        '            Dim fra As Double = abs - c0 '  db - ent
        '            If c0 < 0 Then abs = 10000000000000 - abs
        '            sdb(k) += aux.ToString(MathGlobal8.us) + "." + _
        '                String.Format("{0:00000000000000.0000000000000}.{1:0000000000000.0000000000000}", abs, fra)
        '            cjo(k) = New Complex(Me.vPoly(i).cf(0))
        '            ReDim Preserve cjo3(cjo3.Length)
        '            cjo3(cjo3.Length - 1) = New Complex(Me.vPoly(i).cf(0))
        '            k += 1
        '        End If
        '    Next
        '    'If re.Length Then
        '    Dim cjo2(cjo.Length - 1) As Complex
        '    i = 0
        '    If re.Length Then
        '        Array.Sort(re, i0)
        '        For i = 0 To re.Length - 1
        '            'cjo2(i) = New Complex(re(i))
        '            cjo2(i) = vPoly(i0(i)).cf(0)
        '        Next
        '    End If
        '    If cjo3.Length Then
        '        ReDim Preserve sdb(k - 1)
        '        Array.Sort(sdb, cjo3)
        '        For j As Int64 = 0 To cjo3.Length - 1
        '            cjo2(i) = cjo3(j)
        '            i += 1
        '        Next
        '    End If
        '    cjo = cjo2
        '    'End If
        'Catch ex As Exception
        '    Throw ex
        'End Try
        'Return New Vector(cjo)
    End Function
    Public Function ToStringFactors(cfg As Config, _
                    ByVal An As Complex, ByVal sVar As String, _
                    Optional ByVal Pa As Polynomial = Nothing) As String
        ' Converts a roots vector into a string ( (-1,1)--> (x+1)*(x-1) )
        Dim e1(1) As String
        Dim polyFactors(Me.vPoly.Length - 1) As Polynomial
        Dim i As Int64
        Dim polyOrig As New Polynomial(1.0)
        Try
            Dim sortedVector As Vector = Me.sortRoots
            Me.vPoly = sortedVector.vPoly
            Dim var As String = sVar ' Me.vPoly(0).var(0)(0)
            Dim x As Polynomial = Polynomial.GetPolynomial(sVar)
            Dim e2(Me.vPoly.Length - 1) As String
            Dim bHasCffCjos As Boolean = False
            For i = 0 To vPoly.Length - 1
                polyFactors(i) = (x - vPoly(i))
                polyOrig *= (x - vPoly(i))
                e2(i) = polyFactors(i).toStringPoly(cfg)
                If e2(i) <> var Then
                    e2(i) = "(" + e2(i) + ")"
                End If
            Next
            If Pa IsNot Nothing Then
                polyOrig = New Polynomial(Pa)
            End If
            For i = 0 To polyOrig.cf.Length - 1
                Dim sCff As String = polyOrig.cf(i).toStringComplex(cfg)
                If InStr(sCff, cfg.sImg) Then
                    bHasCffCjos = True
                    Exit For
                End If
            Next
            e1(0) = Join(e2, "*")
            If bHasCffCjos Then
                Return e1(0)
            End If
            Dim e3(Me.vPoly.Length - 1) As String
            Dim j As Int64 = 0
            Dim bUnited As Boolean = False
            Dim i1(e3.Length - 1) As Int64
            For i = 0 To Me.vPoly.Length - 1
                For j = 0 To Me.vPoly.Length - 1
                    If i1(j) = 0 Then
                        e3(j) = polyFactors(j).toStringPoly(cfg)
                        If e3(j) <> var Then
                            e3(j) = "(" + e3(j) + ")"
                        End If
                        If InStr(e3(j), cfg.sImg) > 0 Then
                            ' si es raíz compleja intentar emparejar
                            ' con una raíz conjugada:
                            Dim polyX2 As Polynomial = _
                                        (polyFactors(i) * polyFactors(j))
                            If cfg.bRounding Then
                                If polyX2.An.opModulo >= 1.0 Then
                                    polyX2.cf(1).pRe = New Rational(Math.Round(polyX2.cf(1).pRe.ToDouble, 3))
                                    polyX2.cf(1).pIm = New Rational(Math.Round(polyX2.cf(1).pIm.ToDouble, 3))
                                    polyX2.cf(0).pRe = New Rational(Math.Round(polyX2.cf(0).pRe.ToDouble, 3))
                                    polyX2.cf(0).pIm = New Rational(Math.Round(polyX2.cf(0).pIm.ToDouble, 3))
                                End If
                            End If
                            Dim coef1_re As Double = polyX2.cf(1).pRe.ToDouble
                            Dim coef1_Im As Double = polyX2.cf(1).pIm.ToDouble
                            Dim coef0_re As Double = polyX2.cf(0).pRe.ToDouble
                            Dim coef0_Im As Double = polyX2.cf(0).pIm.ToDouble
                            bUnited = False
                            If (coef1_re <> 0 AndAlso Math.Abs(coef1_Im / _
                                    coef1_re) * 10 ^ 10 < 1) AndAlso _
                                (coef0_re <> 0 AndAlso Math.Abs(coef0_Im / _
                                    coef0_re) * 10 ^ 10 < 1) Then
                                bUnited = True
                            ElseIf (coef0_Im <> 0 AndAlso Math.Abs(coef0_re / _
                                    coef0_Im) * 10 ^ 10 < 1) AndAlso _
                                (coef1_Im <> 0 AndAlso Math.Abs(coef1_re / _
                                    coef1_Im) * 10 ^ 10 < 1) Then
                                bUnited = True
                            ElseIf (coef1_re ^ 2 + coef1_Im ^ 2) / coef0_re ^ 2 * 10 ^ 10 < 1 Then
                                polyX2.cf(1) = New Complex(0.0)
                                If Math.Abs(coef0_Im) < 10 ^ -8 Then
                                    polyX2.cf(0).pIm = New Rational(0.0)
                                End If
                                bUnited = True
                            End If
                            If bUnited Then
                                e3(i) = "(" + polyX2.toStringPoly(cfg) + ")"
                                i1(i) = j
                                i1(j) = i + 1
                                If cfg.bRounding Then
                                    ' suprimir ceros extra:
                                    'Dim expr As New exprParser
                                    'expr.parse(e3(i), "")
                                    'e3(i) = "(" + expr.ToStringExprParser(cfg) + ")"
                                    Exit For
                                End If
                            End If
                        End If
                    End If
                Next

            Next
            e1(0) = ""
            Dim n As Int64 = 0
            j = 0
            Dim e4(i1.Length - 1) As String
            If cfg.bRounding = False Then
                Array.Copy(e2, e4, e2.Length)
                j = e2.Length
            Else
                For i = 0 To i1.Length - 1
                    If i1(i) > 0 Then
                        If InStr(e3(i), cfg.sImg) = 0 Then
                            e4(j) += e3(i)
                            n += 2
                            j += 1
                        End If
                    Else
                        If InStr(e3(i), cfg.sImg) = 0 Then
                            e4(j) += e2(i)
                            n += 1
                            j += 1
                        Else
                        End If
                    End If
                    If n >= polyFactors.Length Then
                        Exit For
                    End If
                Next
            End If
            ReDim Preserve e4(j - 1)
            e1(0) = Join(e4, "*")
            If An IsNot Nothing Then
                Dim sAn As String = An.toStringComplex(cfg)
                If sAn = "1" Then
                ElseIf sAn = "-1" Then
                    If e1(0).Chars(0) = "-" Then
                        e1(0) = Mid(e1(0), 2)
                    Else
                        e1(0) = "-" + e1(0)
                    End If
                Else
                    If sAn = "0" Then
                        sAn = An.toStringComplex(cfg) ' avoid displaying a zero
                    End If
                    e1(0) = sAn + "*" + e1(0)
                End If
            End If
        Catch ex As Exception
            Throw ex
        End Try
        If Not cfg.bRounding Then
            Return e1(0)
        End If
        Dim bRounding As Boolean = True
        Try
            Dim mP As New matrixParser
            mP.bCaseS = cfg.bCaseSensitive
            mP.parse(Replace(e1(0), " ", ""))
            Dim factorsPoly As Polynomial = mP.ret.curExpr.getPolynomial
            If Pa IsNot Nothing AndAlso _
            factorsPoly.getDegree <> Pa.getDegree Then
                bRounding = False
            End If
        Catch ex As Exception
            bRounding = False
        End Try
        Try
            If Not bRounding Then
                cfg.bRounding = False
                'e1(0) = ToStringFactors(cfg, An, sVar, Pa)
                e1(0) = Pa.ToStringFactors(cfg)
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return e1(0)
    End Function
    Public Shared Function toPolynomialFromRoots(ByVal Va As Vector, Optional ByVal var As String = "x") As Polynomial
        Dim Pc As Polynomial = Nothing
        Try
            If Va IsNot Nothing AndAlso Va.vPoly.Length Then
                Dim x As Polynomial = Polynomial.GetPolynomial(var)
                Dim i As Int64
                Pc = New Polynomial(1.0)
                For i = 0 To Va.vPoly.Length - 1
                    Pc *= (x - Va.vPoly(i))
                Next
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return Pc
    End Function
    'Public ReadOnly Property Var() As String()
    '    Get
    '        Dim sVars(-1) As String
    '        Dim i As Int64
    '        If vE.Length = 0 Then
    '            Return sVars
    '        End If
    '        sVars = vE(0).var
    '        For i = 1 To vE.Length - 1
    '            Dim vCur() As String = vE(i).var
    '            If sVars.Length = 0 Then
    '                sVars = vCur
    '            ElseIf vCur.Length Then
    '                Dim j As Int64
    '                For j = 0 To vCur.Length - 1
    '                    Dim pos As Int64 = Array.IndexOf(sVars, vCur(j))
    '                    If pos = -1 Then
    '                        ReDim Preserve sVars(sVars.Length)
    '                        sVars(sVars.Length - 1) = vCur(j)
    '                    End If
    '                Next
    '            End If
    '        Next
    '        Return sVars
    '    End Get
    'End Property
End Class
