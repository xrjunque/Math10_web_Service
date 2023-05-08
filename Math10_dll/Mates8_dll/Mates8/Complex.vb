Imports System.Text.RegularExpressions
Imports System.Text
Imports System.Collections

<Serializable()> _
Public Class Complex
    Public Sub New()
        MyBase.New()
    End Sub
    Shared us As System.Globalization.CultureInfo = MathGlobal8.us
    Public pRe, pIm As Rational
    Public Sub New(ByVal Re As Double)
        Me.pRe = New Rational(Re)
        Me.pIm = New Rational(0)
    End Sub
    Public Sub New(ByVal Re As Double, ByVal Im As Double)
        Me.pRe = New Rational(Re)
        Me.pIm = New Rational(Im)
    End Sub
    Public Sub New(ByVal cjo As Complex)
        Me.pRe = New Rational(cjo.pRe)
        Me.pIm = New Rational(cjo.pIm)
    End Sub
    Public Sub New(ByVal prRe As Rational, ByVal prIm As Rational)
        Me.pRe = New Rational(prRe)
        Me.pIm = New Rational(prIm)
    End Sub
    Public Shared ReadOnly Property zero() As Complex
        Get
            Return New Complex(0.0)
        End Get
    End Property
    Public Shared ReadOnly Property one() As Complex
        Get
            Return New Complex(1.0)
        End Get
    End Property
    Public Shared ReadOnly Property minusOne() As Complex
        Get
            Return New Complex(-1.0)
        End Get
    End Property
    Public Shared ReadOnly Property oneHalf() As Complex
        Get
            Return New Complex(0.5)
        End Get
    End Property
    Public Shared ReadOnly Property i() As Complex
        Get
            Return New Complex(0.0, 1.0)
        End Get
    End Property
    Public Shared ReadOnly Property minusi() As Complex
        Get
            Return New Complex(0.0, -1.0)
        End Get
    End Property
    Public Shared Function TryParseComplex(
                                          ByVal sImg As String,
                                          ByVal input As String,
                                          ByRef result As Complex,
                                          Optional ByRef err As Exception = Nothing) As Boolean

        Try
            result = New Complex(0.0)
            input = Replace(input, " ", "")
            input = Replace(input, MathGlobal8.sLP, "(")
            input = Replace(input, MathGlobal8.sRP, ")")
            input = Regex.Replace(input, "(?<izq>\)|" + MathGlobal8.sNum + ")" + sImg, "${izq}*" + sImg)
            input = Regex.Replace(input, sImg + "(?<dch>" + MathGlobal8.sNum + "|\()", sImg + "*${dch}")
            Dim sPat As String = "((?<!e)\-|(?<!e)\+|\*|\/|\^|\))+"
            Dim vM() As String = Regex.Split(input,
             "((?<!e)\-|(?<!e)\+|\*|\/|\^|!)+" + "|" + MathGlobal8.sLP + "|" + MathGlobal8.sRP)
            Dim vExpr(vM.Length - 1) As Complex
            vExpr(0) = New Complex(0.0)
            Dim vOp() As String = {"(", "^", "/", "*", "+", "-"}
            Dim i As Int32
            For i = 0 To vM.Length - 1
                If Len(vM(i)) AndAlso Not Regex.IsMatch(vM(i), sPat) Then
                    Dim db As Double
                    If vM(i) = "(" Then ' a left parenthesis
                        Dim posRP As Int32 = Array.IndexOf(vM, ")", i + 1)
                        Dim sSubExpr As String = String.Empty
                        Dim j As Int64 = i + 1
                        Do
                            If vM(j) = "(" Then
                                ' there are more parentheses enclosed, search
                                ' for the outer RP:
                                posRP = Array.IndexOf(vM, ")", posRP + 1)
                            End If
                            sSubExpr += vM(j)
                            vM(j) = ""
                            j += 1
                        Loop While j < posRP
                        If Complex.TryParseComplex(sImg, sSubExpr, vExpr(i)) Then
                            vM(i) = "" : vM(j) = ""
                        Else
                            Exit Try
                        End If
                    ElseIf MathGlobal8.TryParseDbl(vM(i), db) Then
                        vExpr(i) = New Complex(db)
                    Else
                        Dim nFn As Int64 = Array.IndexOf(MathGlobal8.vFn, vM(i))
                        If nFn = -1 Then
                            If vM(i) = sImg Then ' usually sImg="i"
                                vExpr(i) = New Complex(0.0, 1.0)
                            Else
                                Dim m As Match = Regex.Match(vM(i), MathGlobal8.sFn)
                                If Not m.Success Then
                                    Throw New Exception(msg8.num(0)) ' not a real number
                                End If
                                Dim mNum As Match = Regex.Match(vM(i), MathGlobal8.sNum)
                                Dim cjo As Complex = Nothing
                                If TryParseComplex(sImg, mNum.ToString, cjo, err) Then
                                    vExpr(i) = EvalFn(m.ToString, cjo)
                                End If
                                vM(i) = ""
                            End If
                        ElseIf vM(i + 1) = "(" Then
                            ' Argument enclosed by parentheses
                            ' as in cos(x). 
                            Dim LP As Int32 = 1
                            Dim RP As Int32 = 0
                            Dim subexpr As New StringBuilder(50)
                            Dim j As Int32
                            For j = i + 2 To vM.Length - 1
                                If vM(j) = ")" Then RP += 1
                                If vM(j) = "(" Then LP += 1
                                If RP = LP Then Exit For
                                subexpr.Append(vM(j))
                                vM(j) = ""
                            Next
                            Dim cjo As Complex = Nothing
                            If TryParseComplex(sImg, subexpr.ToString, cjo, err) Then
                                vExpr(i) = EvalFn(vM(i), cjo)
                                vM(i) = ""
                                vM(j) = ""
                                i = j
                            End If
                        Else
                            Exit Try
                        End If
                    End If
                End If
            Next
            Dim pos As Int32 = -1
            For Each e1 As String In vOp
                Do
                    If e1 = "^" Then
                        If pos = -1 Then pos = vM.Length
                        pos = Array.LastIndexOf(vM, e1, pos - 1)
                    Else
                        pos = Array.IndexOf(vM, e1, pos + 1)
                    End If
                    If pos > -1 Then
                        Select Case e1
                            Case "("
                                Dim posRP As Int32 = Array.IndexOf(vM, ")", pos + 1)
                                Dim sSubExpr As String = String.Empty
                                For j As Int64 = pos + 2 To posRP - 1
                                    sSubExpr += vM(j)
                                    vM(j) = ""
                                Next
                                vExpr(pos) = New Complex(Rational.ParseNumber(sSubExpr))
                                vM(pos) = ""
                            Case "-"
                                If pos = 0 Then
                                    vExpr(0) = -vExpr(1)
                                Else
                                    Dim L As Int64 = pos - 1
                                    Do While L > 0 AndAlso vExpr(L) Is Nothing
                                        L -= 1
                                    Loop
                                    Dim R As Int64 = pos + 1
                                    Do While vExpr(R) Is Nothing
                                        R += 1
                                    Loop
                                    vExpr(L) -= vExpr(R)
                                    vExpr(R) = Nothing
                                End If
                                vM(pos) = ""
                            Case Else
                                Dim L As Int64 = pos - 1
                                Do While vExpr(L) Is Nothing
                                    L -= 1
                                Loop
                                Dim sgn As Int64 = 1
                                If L AndAlso vM(L - 1) = "-" Then
                                    L -= 1
                                    If e1 = "^" Then
                                        sgn = -1
                                        vExpr(L) = vExpr(L + 1)
                                    Else
                                        vExpr(L) = -vExpr(L + 1)
                                    End If
                                    vExpr(L + 1) = Nothing
                                    vM(L + 1) = vM(L) + vM(L + 1)
                                    vM(L) = ""

                                End If
                                Dim R As Int64 = pos + 1
                                Do While vExpr(R) Is Nothing
                                    R += 1
                                Loop
                                If R > pos + 1 AndAlso vM(R - 1) = "-" Then
                                    vM(R - 1) = ""
                                    vExpr(R) = -vExpr(R)
                                End If
                                Select Case e1
                                    Case "-" : vExpr(L) -= vExpr(R)
                                    Case "+" : vExpr(L) += vExpr(R)
                                    Case "*" : vExpr(L) *= vExpr(R)
                                    Case "/" : vExpr(L) /= vExpr(R)
                                    Case "^" : vExpr(L) ^= vExpr(R)
                                        If sgn = -1 Then vExpr(L) = -vExpr(L)
                                End Select
                                vExpr(R) = Nothing
                                vM(pos) = ""
                        End Select
                    End If
                Loop While pos <> -1
            Next
            For i = 0 To vExpr.Length - 1
                If vExpr(i) IsNot Nothing Then
                    result += vExpr(i)
                End If
            Next
        Catch ex As Exception
            err = ex
            Return False
        End Try
        If result Is Nothing Then
            Return False
        End If
        Return True
    End Function
    Public Shared Operator +(ByVal cjoA As Complex, ByVal db As Double) As Complex
        Return New Complex(cjoA.pRe + New Rational(db), cjoA.pIm)
    End Operator
    Public Shared Operator +(ByVal db As Double, ByVal cjoA As Complex) As Complex
        Return New Complex(cjoA.pRe + db, cjoA.pIm)
    End Operator
    Public Shared Operator +(ByVal cjoA As Complex, ByVal cjoB As Complex) As Complex
        Dim prRe As Rational = cjoA.pRe + cjoB.pRe
        Dim prIm As Rational = cjoA.pIm + cjoB.pIm
        Return New Complex(prRe, prIm)
    End Operator
    Public Shared Operator -(ByVal cjoA As Complex) As Complex
        Return New Complex(-cjoA.pRe, -cjoA.pIm)
    End Operator
    Public Shared Operator -(ByVal cjoA As Complex, ByVal db As Double) As Complex
        Return cjoA - New Complex(db)
    End Operator
    Public Shared Operator -(ByVal db As Double, ByVal cjoA As Complex) As Complex
        Return New Complex(db) - cjoA
    End Operator
    Public Shared Operator -(ByVal cjoA As Complex, ByVal cjoB As Complex) As Complex
        Dim Re As Rational = cjoA.pRe - cjoB.pRe
        Dim Im As Rational = cjoA.pIm - cjoB.pIm
        Return New Complex(Re, Im)
    End Operator
    Public Shared Operator *(ByVal cjoA As Complex, ByVal db As Double) As Complex
        Return New Complex(cjoA.pRe * db, cjoA.pIm * db)
    End Operator
    Public Shared Operator *(ByVal db As Double, ByVal cjoB As Complex) As Complex
        Return New Complex(cjoB.pRe * db, cjoB.pIm * db)
    End Operator
    Public Shared Operator *(ByVal cjoA As Complex, ByVal cjoB As Complex) As Complex
        If cjoA.pIm.IsZero AndAlso cjoB.pIm.IsZero Then
            Return New Complex(cjoA.pRe * cjoB.pRe, New Rational(0.0))
        End If
        Dim prRe As Rational = cjoA.pRe * cjoB.pRe - cjoA.pIm * cjoB.pIm
        Dim prIm As Rational = cjoA.pRe * cjoB.pIm + cjoA.pIm * cjoB.pRe
        Return New Complex(prRe, prIm)
    End Operator
    Public Shared Operator /(ByVal num As Double, ByVal cjoA As Complex) As Complex
        Return New Complex(num, 0.0) / cjoA
    End Operator
    Public Shared Operator /(ByVal cjoA As Complex, ByVal den As Double) As Complex
        Dim prD As New Rational(1.0, den)
        Return cjoA * New Complex(prD, New Rational(0.0))
    End Operator
    Public Shared Operator /(ByVal cjoA As Complex, ByVal cjoB As Complex) As Complex
        If cjoB.IsZero Then
            Throw New DivideByZeroException
        End If
        If cjoA.pIm.IsZero AndAlso cjoB.pIm.IsZero Then
            Dim pRe As Rational = cjoA.pRe / cjoB.pRe
            Dim pZero As New Rational(0.0)
            Return New Complex(pRe, pZero)
        End If
        '  x       a + b i      ac + bd        bc - ad
        ' ---   = ---------  = --------- + i* --------- 
        '  y       c + d i      c^2+d^2        c^2 +d^2
        Dim a As Rational = cjoA.pRe
        Dim b As Rational = cjoA.pIm
        Dim c As Rational = cjoB.pRe
        Dim d As Rational = cjoB.pIm
        Dim Den As Rational = c * c + d * d
        If Den.IsZero Then
            Throw New Exception("Division by zero.")
        End If
        Return New Complex((a * c + b * d) / Den, _
                           (b * c - a * d) / Den)
    End Operator
    Public Function opAbs() As Complex
        Return New Complex((pRe * pRe + pIm * pIm) ^ New Rational(0.5), New Rational(0.0))
    End Function
    Public Function opModulo() As Double
        Dim pr As Rational = Nothing
        Try
            pr = pRe * pRe + pIm * pIm
            Return Math.Sqrt(pr.ToDouble)
        Catch ex As Exception
            Dim db As Double = Double.MaxValue / 2.0
            If pr IsNot Nothing Then
                db *= pr.sgn
            End If
            Return db
        End Try
    End Function
    Public Shared Operator ^(ByVal cjoA As Complex, ByVal cjoB As Complex) As Complex
        If cjoA.pIm.IsZero AndAlso cjoB.pIm.IsZero Then ' AndAlso cjoA.pRe.sgn > 0.0 Then
            Dim rB As Double = cjoB.pRe.ToDouble
            Dim cA As New Complex(cjoA)
            If rB < 0 Then
                cA = 1 / cA
                rB = -rB
            End If
            If cA.IsZero Then
                Dim sgn As Int64 = Math.Sign(rB)
                If sgn = 1 Then
                    Return New Complex(0.0)
                ElseIf sgn = 0 Then
                    Return New Complex(1.0)
                Else
                    Throw New DivideByZeroException
                End If
            End If
            If cA.pRe.sgn > 0 Then
                Dim powAB As Rational = cA.pRe ^ rB
                Return New Complex(powAB, New Rational(0.0))
            ElseIf rB = 0.5 Then
                'if x>0, (-x)^0.5=sqr(-x)=sqr(x)*sqr(-1)=sqr(x)*i
                Dim powAB As Rational = New Rational(Math.Sqrt(-cA.pRe.ToDouble))
                Return New Complex(New Rational(0.0), powAB)
            ElseIf rB >= 1.0 Then
                If rB = Math.Floor(rB) AndAlso rB Mod 2 = 0 Then
                    Dim powAB As Rational = cA.pRe ^ rB
                    Return New Complex(powAB, New Rational(0.0))
                End If
            End If
        End If
        If cjoB.IsZero Then
            Return Complex.one
        ElseIf cjoA.IsZero Then
            Return Complex.zero
        End If
        If cjoA.opModulo = 1 AndAlso cjoB.IsReal Then
            ' z = a+b*i ={forma polar} = (|z|)alpha = (|z|)atan(b/a) = (|z|)atan2(b,a)
            ' 
            Dim alpha As Double = Math.Atan2(cjoA.pIm.ToDouble, cjoA.pRe.ToDouble)
            ' (Zalpha)^n = Z(n*alpha) si el módulo |z|=1:
            Dim n As Double = (cjoB.pRe.ToDouble * alpha) Mod (2 * Math.PI)
            If Math.Abs(n) = Math.PI / 2 OrElse Math.Abs(n) = 3 * Math.PI / 2 Then
                Return New Complex(0.0, Math.Sin(n))
            ElseIf Math.Abs(n) = Math.PI Then
                Return New Complex(Math.Cos(n), 0.0)
            End If
            Return New Complex(Math.Cos(n), Math.Sin(n))
        End If
        Dim expB As Double
        If cjoB.pIm.IsZero Then
            expB = cjoB.pRe.ToDouble
            If expB = Math.Floor(expB) Then ' AndAlso expB <= 64 Then
                Dim exp As Int64 = Math.Abs(expB)
                Try
                    Dim cjoC As Complex = Nothing
                    If exp > 0 Then
                        Dim log2 As Int64 = Math.Floor(Math.Log10(exp) / Math.Log10(2.0))
                        Dim vNum(log2) As Complex
                        Dim vExp(log2) As Int64
                        Dim curExp As Int64 = 2
                        vNum(0) = New Complex(cjoA)
                        vExp(0) = 1
                        For i As Int64 = 1 To log2
                            vNum(i) = vNum(i - 1) * vNum(i - 1)
                            vExp(i) = curExp
                            curExp *= 2
                        Next
                        curExp = vExp(log2)
                        cjoC = vNum(log2)
                        For i As Int64 = log2 - 1 To 0 Step -1
                            If curExp + vExp(i) <= exp Then
                                cjoC *= vNum(i)
                                curExp += vExp(i)
                                If curExp = exp Then
                                    Exit For
                                End If
                            End If
                        Next
                        If expB < 0 Then
                            cjoC = Complex.one / cjoC
                        End If
                        Return cjoC
                    End If
                Catch ex As Exception

                End Try
            End If
        End If














        ' if a= (a1,a2) = (r,alfa) (r=modulus, alfa=argument=atan2(a2,a1))
        ' and b=(c,d) then
        ' a^b = r^c * exp(-d*alfa) * [ cos(d*log(r) + c * alfa + i * sin(d*log(r)+c*alfa)
        Dim c As Double = cjoB.pRe.ToDouble
        Dim d As Double = cjoB.pIm.ToDouble
        Dim r As Double = cjoA.opModulo
        If r = 0 Then
            Return Complex.zero
            'ElseIf d = 0 Then
            '    Dim modulo As New Rational(r * r)
            '    r /= 2.0
            '    Dim alfa2 As New Rational(Math.Atan2(cjoA.pIm.ToDouble, cjoA.pRe.ToDouble))
            '    Dim rcTimesExpdalfa2 As Rational = New Rational(Math.Pow(r, c)) * New Rational(Math.E)
            '    Dim dlogrPluscalfa2 As Rational = cjoB.pRe * alfa2
            '    Return New Complex(rcTimesExpdalfa2 * New Rational(Math.Cos(dlogrPluscalfa2.ToDouble)),
            '                     rcTimesExpdalfa2 * New Rational(Math.Sin(dlogrPluscalfa2.ToDouble)))
        End If
        Dim alfa As Double = Math.Atan2(cjoA.pIm.ToDouble, cjoA.pRe.ToDouble)
        Dim rcTimesExpdalfa As Double = Math.Pow(r, c) * Math.Exp(-d * alfa)
        If alfa = Math.PI Then
            Return New Complex(-rcTimesExpdalfa, 0.0)
        End If
        Dim dlogrPluscalfa As Double = d * Math.Log(r) + c * alfa
        Return New Complex(rcTimesExpdalfa * Math.Cos(dlogrPluscalfa), _
                             rcTimesExpdalfa * Math.Sin(dlogrPluscalfa))
    End Operator
    Public Shared Function opCos(ByVal x As Complex) As Complex
        If x.pIm.IsZero Then
            Dim re As Double = x.pRe.ToDouble Mod (2 * Math.PI)
            Dim re2 As Double = re / (Math.PI / 2.0)
            If re2 = Math.Floor(re2) AndAlso re2 Mod 2 Then
                Return New Complex(0.0)
            End If
            Return New Complex(Math.Cos(re))
        End If
        '           e^(ix)+e^(-ix)
        ' cos(x) = ---------------
        '                2 
        ''
        Dim ix As Complex = New Complex(-x.pIm, x.pRe)
        Dim minusix As New Complex(x.pIm, -x.pRe)
        Dim e As New Complex(Math.Exp(1), 0.0)
        Dim eix As Complex = e ^ ix
        Dim eminusix As Complex = e ^ minusix
        Dim num As Complex = eix + eminusix
        Dim ret As New Complex(num.pRe / 2.0, num.pIm / 2.0)
        Return ret
    End Function
    Public Shared Function opArg(ByVal x As Complex) As Complex
        Return New Complex(Math.Atan2(x.pIm.ToDouble, x.pRe.ToDouble), 0.0)
    End Function
    Public Shared Function opExp(ByVal x As Complex) As Complex
        If x.pIm.IsZero Then
            If x.pRe.IsZero Then
                Return New Complex(1.0)
            End If
            Return New Complex(Math.E ^ x.pRe.ToDouble, 0.0)
        End If
        Return New Complex(Math.E, 0.0) ^ x
    End Function
    Public Shared Function opLn(ByVal x As Complex) As Complex
        If x.pIm.IsZero AndAlso x.pRe.sgn = 1 Then
            Return New Complex(Math.Log(x.pRe.ToDouble), 0.0)
        End If

        ' ln z = ln |z| + i*arg(z)
        Dim arg As Double = Math.Atan2(x.pIm.ToDouble, x.pRe.ToDouble)
        'Dim re As Double = Math.Log(Math.Sqrt(x.pRe * x.pRe + x.pIm * x.pIm))
        Dim re As Double = Math.Log(x.opModulo)
        Return New Complex(re, arg)

    End Function
    Public Shared Function opACsc(ByVal z As Complex) As Complex
        '           
        ' acsc(z) = asin(1.0/z)
        '           
        '
        Dim arg As Complex = one / z
        Return opASin(z)
    End Function
    Public Shared Function opACscH(ByVal z As Complex) As Complex
        '           
        ' acscH(z) = ln(1/z + sqr(1+1/x^2) )
        '           
        '
        Dim arg As Complex = one / z + (one + one / (z * z)) ^ oneHalf
        Return opLn(arg)
    End Function
    Public Shared Function opACos(ByVal z As Complex) As Complex
        '           
        ' acos(z) = -i * log[z + i*(1.0 - z*z))^0.5]
        '           
        '
        Dim arg As Complex = z + i * (one - z * z) ^ oneHalf
        Return -Complex.i * opLn(arg)
    End Function
    Public Shared Function opACosH(ByVal z As Complex) As Complex
        '           
        ' acosh(z) = ln(z + sqrt(z * z - 1.0))
        '           
        '
        Dim arg As Complex = z + (z * z - one) ^ oneHalf
        Return opLn(arg)
    End Function
    Public Shared Function opACot(ByVal z As Complex) As Complex
        '           
        ' acot(z) = atan(1.0 / z)
        '           
        '
        Return opAtan(one / z)
    End Function
    Public Shared Function opACotH(ByVal z As Complex) As Complex
        '           
        ' acoth(z) = atanh(1.0 / z)
        '           
        '
        Return opAtanH(one / z)
    End Function
    Public Shared Function opASec(ByVal z As Complex) As Complex
        '           
        ' asec(z) = asin(sqr(x^2-1)/x)
        '           
        '
        Return opASin((z * z - one) ^ oneHalf / z)
    End Function
    Public Shared Function opASecH(ByVal z As Complex) As Complex
        '           
        ' asech(z) = acosh(1.0 / z)
        '           
        '
        Return opACosH(one / z)
    End Function
    Public Shared Function opASin(ByVal z As Complex) As Complex
        '
        '
        '
        Dim r As Complex = Nothing
        If z.IsReal AndAlso Math.Abs(z.pRe.ToDouble) < 1.0 Then
            '  asin(z) = -i*ln(i*z +(|1-z^2|)^0.5*e^((i/2)*arg(1-z^2)))
            ' if |z|<1, z real => arg(1-z^2)=0 =>
            ' => asin(z)=0.5*ln(i*z+(|1-z^2|)^0.5)
            r = -i * opLn(i * z + (1 - z * z) ^ oneHalf)
            r.pIm = New Rational(0.0)
        Else
            Dim z1 As Complex = one - z * z
            Dim absZ1 As Double = Math.Abs(z1.pRe.ToDouble)
            r = -i * opLn(i * z + z1 ^ oneHalf)
        End If
        Return r
    End Function

    Public Shared Function opASinH(ByVal z As Complex) As Complex
        '           
        ' asinh(z) = log(z +(z*z + 1.0)^0.5)
        '           
        '
        Dim arg As Complex = z + (z * z + one) ^ oneHalf
        Return opLn(arg)
    End Function

    Public Shared Function opAtan(ByVal z As Complex) As Complex
        '           
        ' atan(z)= 1/(2i) * ln ((1 + iz)/(1 - iz)) ' 2014/03/06 http://mathforum.org/library/drmath/view/72732.html
        '
        If z.IsReal Then
            Return New Complex(Math.Atan2(z.pRe.num, z.pRe.den), 0.0) ' 2017/02/24
        End If
        Return opLn((i - z) / (i + z)) * i * -0.5
    End Function
    Public Shared Function opAtanH(ByVal z As Complex) As Complex
        '           
        ' atanh(z) = log((1.0 + z) / (1.0 - z)) /2  2014/03/06
        '           
        '
        Return opLn((one + z) / (one - z)) * 0.5
    End Function
    Public Shared Function opCosH(ByVal x As Complex) As Complex
        '           e^(x)+e^(-x)
        ' cos(x) = ---------------
        '               2
        '
        Dim ex As Complex = New Complex(Math.E) ^ x
        Dim eminusx As Complex = New Complex(Math.E) ^ -x
        Dim num As Complex = ex + eminusx
        Dim ret As New Complex(num.pRe / 2.0, num.pIm / 2.0)
        Return ret
    End Function
    Public Shared Function opCot(ByVal z As Complex) As Complex
        ' 
        ' cot(z) = cos(z) / sin(z)
        ' 
        '
        Return opCos(z) / opSin(z)
    End Function
    Public Shared Function opCotH(ByVal z As Complex) As Complex
        ' 
        ' coth(z) = cosh(z) / sinh(z)
        ' 
        '
        Return opCosH(z) / opSinH(z)
    End Function
    Public Shared Function opCsc(ByVal z As Complex) As Complex
        ' 
        ' csc(z) = 1.0 / sin(z)
        ' 
        '
        Return one / opSin(z)
    End Function
    Public Shared Function opCscH(ByVal z As Complex) As Complex
        ' 
        ' csch(z) = 1.0 / sinh(z)
        ' 
        '
        Return one / opSinH(z)
    End Function
    Public Shared Function opSec(ByVal z As Complex) As Complex
        ' 
        ' csc(z) = 1.0 / cos(z)
        ' 
        '
        Return one / opCos(z)
    End Function
    Public Shared Function opSecH(ByVal z As Complex) As Complex
        ' 
        ' sech(z) = 1.0 / cosh(z)
        ' 
        '
        Return one / opCosH(z)
    End Function
    Public Shared Function opSin(ByVal x As Complex) As Complex
        If x.pIm.IsZero Then
            If Math.IEEERemainder(x.pRe.ToDouble, Math.PI) = 0 Then
                Return New Complex(0.0)
            End If
            ' Needed because if arg>> then math.sin(arg) fails:
            Dim re As Double = x.pRe.ToDouble Mod (2 * Math.PI)
            Return New Complex(Math.Sin(re))
        End If
        '           e^(ix)-e^(-ix)          e^(ix)-e^(-ix)
        ' sin(x) = --------------- = - i * ----------------
        '               2 * i                     2
        ''
        Dim ix As Complex = New Complex(-x.pIm, x.pRe)
        Dim minusix As New Complex(x.pIm, -x.pRe)
        Dim e As New Complex(Math.Exp(1), 0.0)
        Dim eix As Complex = e ^ ix
        Dim eminusix As Complex = e ^ minusix
        Dim num As Complex = eix - eminusix
        Dim ret As New Complex(num.pRe / 2.0, num.pIm / 2.0)
        Dim retTimesMinusI As New Complex(ret.pIm, -ret.pRe)
        Return retTimesMinusI
    End Function
    Public Shared Function opSinH(ByVal x As Complex) As Complex
        '           e^(z)-e^(-z)
        ' sinh(z) = ---------------
        '               2
        '
        Dim minusx As New Complex(x)
        minusx.opChgSgn()
        Dim e As New Complex(Math.Exp(1), 0)
        Dim eix As Complex = opExp(x)
        Dim eminusx As Complex = opExp(minusx)
        Dim num As Complex = eix - eminusx
        Dim ret As New Complex(num.pRe / 2.0, num.pIm / 2.0)
        Return ret
    End Function
    Public Shared Function optan(ByVal z As Complex) As Complex
        ' 
        ' tan(z) = sin(z) / cos(z)
        ' 
        '
        Return opSin(z) / opCos(z)
    End Function
    Public Shared Function optanh(ByVal z As Complex) As Complex
        ' 
        ' tanh(z) = sinh(z) / cosh(z)
        ' 
        '
        Return opSinH(z) / opCosH(z)
    End Function

    Public Function opNorm() As Double
        Return (pRe * pRe + pIm * pIm).ToDouble
    End Function
    Public Shared Function opConjugate(ByVal x As Complex) As Complex
        Return New Complex(x.pRe, -x.pIm)
    End Function
    Public Shared Function opConjugateAndSwapReIm(ByVal x As Complex) As Complex
        Return New Complex(-x.pIm, x.pRe)
    End Function
    Public Shared Function opIgual(ByVal x As Complex, ByVal y As Complex) As Boolean
        Return ((x.pRe - y.pRe).IsZero AndAlso (x.pIm - y.pIm).IsZero)
    End Function
    Public Sub opChgSgn()
        pRe *= -1.0
        pIm *= -1.0
    End Sub
    Public ReadOnly Property IsReal() As Boolean
        Get
            Return (pIm.ToDouble = 0.0)
        End Get
    End Property
    Public Function IsZero() As Boolean
        Return (pRe.IsZero AndAlso pIm.IsZero)
    End Function
    Public Function esCero() As Boolean
        Return (pRe.IsZero AndAlso pIm.IsZero)
    End Function
    Public Function ToStringComplex(Optional cfg As Config = Nothing) As String
        Dim nDec As Int32 = 3
        Dim sI As String = "i"
        Dim bRounding As Boolean = False
        Dim bFractions As Boolean = False
        Dim Base As MathGlobal8.outputBase =
             MathGlobal8.outputBase.decimal
        Dim bEngNotation As Boolean = True
        If cfg IsNot Nothing Then
            sI = cfg.sImg
            nDec = cfg.nDecimals
            bRounding = cfg.bRounding
            bFractions = cfg.bFractions
            Base = cfg.Base
            bEngNotation = cfg.bEngNotation
        End If
        If nDec < 0 OrElse nDec > 15 Then
            nDec = 15
        End If
        Dim s As String = ""
        Dim iNotZero As Int64 = 0
        Dim Re As Double = Me.pRe.ToDouble
        Dim Im As Double = Me.pIm.ToDouble
        Dim ReAbs As Double = Math.Abs(Re)
        Dim ImAbs As Double = Math.Abs(Im)
        If bRounding Then
            If ImAbs < 10 ^ -6 AndAlso
             ReAbs > ImAbs * 10 ^ 6 Then
                Im = 0.0
            ElseIf ReAbs < 10 ^ -6 AndAlso
            ImAbs > ReAbs * 10 ^ 6 Then
                Re = 0.0
            End If
        End If

        Dim sRe As String = Re.ToString("e15", us)
        Dim vRe() As String = Split(sRe, "e")
        Dim fraRe As Double = Re - Math.Floor(Re)
        Dim sFraRe As String = ""
        'Dim i As Int64
        If bFractions Then
            'Dim raRe As New Rational(pRe.num * pRe.sgn, pRe.den)
            sRe = pRe.ToString(cfg)
            If Len(sRe) Then
                ' sRe contains an integer or a fraction
                GoTo doIm
            Else
                ' if sRe="" value should be shown "normally"
                ' because num. and/or denominator aren't
                ' integers
            End If
        End If
        If Base <> MathGlobal8.outputBase.decimal Then
            sRe = Rational.toStringBase(cfg, Re)
        Else
            If bEngNotation AndAlso vRe.Length > 1 AndAlso
            (Math.Abs(Re) < 0.001 OrElse Math.Abs(Re) > 100000) Then
                Dim exp As Int64 = Int64.Parse(vRe(1))
                Dim mant As Double = Double.Parse(vRe(0), us)
                If bEngNotation AndAlso
                (Math.Abs(Re) < 0.1 OrElse Math.Abs(Re) >= 1000) Then
                    If exp < 0 OrElse mant < 10 Then
                        Do While Math.Abs(exp) Mod 3
                            mant *= 10 : exp -= 1
                        Loop
                    Else
                        Do While Math.Abs(exp) Mod 3
                            mant /= 10 : exp += 1
                        Loop
                    End If
                ElseIf bEngNotation Then
                    Do While exp > 0
                        mant *= 10 : exp -= 1
                    Loop
                    Do While exp < 0
                        mant /= 10 : exp += 1
                    Loop
                End If
                If bRounding AndAlso Math.Round(mant, nDec) Then
                    mant = Math.Round(mant, nDec)
                    If mant = 1000 OrElse mant = -1000 Then
                        exp += 3
                        mant /= 1000
                    End If
                End If
                If exp = 0 Then
                    sRe = mant.ToString(us)
                Else
                    sRe = mant.ToString(us) + "e" + exp.ToString
                End If
            ElseIf bRounding Then
                sRe = Math.Round(Re, nDec).ToString(us)
            Else
                sRe = Re.ToString(us)
            End If
        End If
doIm:
        Dim sIm As String = Im.ToString("e12", us)
        Dim vim() As String = Split(sIm, "e")
        Dim fraIm As Double = Im - Math.Floor(Im)
        Dim sFraIm As String = ""
        If bFractions Then
            'Dim raIm As New Rational(pIm.num * pIm.sgn, pIm.den)
            sIm = pIm.ToString(cfg)
            If Len(sIm) Then
                ' sIm contains an integer or a fraction
                GoTo endIm
            Else
                ' if sIm="" value should be shown "normally"
                ' because num. and/or denominator aren't
                ' integers
            End If
        End If
        If Base <>
            MathGlobal8.outputBase.decimal Then

            sIm = Rational.toStringBase(cfg, Im)

            'Dim Im1 As Double = Math.Floor(Im)
            'sIm = Im1.ToString
        Else
            If bEngNotation AndAlso vim.Length > 1 AndAlso
            (Math.Abs(Im) < 0.001 OrElse Math.Abs(Im) > 100000) Then
                Dim exp As Int64 = Int64.Parse(vim(1))
                Dim mant As Double = Double.Parse(vim(0), us)
                If bEngNotation AndAlso
                (Math.Abs(Im) < 0.1 OrElse Math.Abs(Im) >= 1000) Then
                    If exp < 0 OrElse mant <= 10 Then
                        Do While Math.Abs(exp) Mod 3
                            mant *= 10 : exp -= 1
                        Loop
                    Else
                        Do While Math.Abs(exp) Mod 3
                            mant /= 10 : exp += 1
                        Loop
                    End If
                ElseIf bEngNotation Then
                    Do While exp > 0
                        mant *= 10 : exp -= 1
                    Loop
                    Do While exp < 0
                        mant /= 10 : exp += 1
                    Loop
                End If
                If bRounding AndAlso Math.Round(mant, nDec) Then
                    mant = Math.Round(mant, nDec)
                    If mant = 1000 OrElse mant = -1000 Then
                        exp += 3
                        mant /= 1000
                    End If
                End If
                If exp = 0 Then
                    sIm = mant.ToString(us)
                Else
                    sIm = mant.ToString(us) + "e" + exp.ToString
                End If
            ElseIf bRounding Then
                sIm = Math.Round(Im, nDec).ToString(us)
            Else
                sIm = Im.ToString(us)
            End If
        End If
endIm:
        s = sRe + "+" + sI + "*" + sIm
        If Re = 0.0 AndAlso Im = 0.0 Then
            s = "0"
        ElseIf Re = 0.0 Then
            If Im > 0.0 Then
                'If Im = 1.0 Then
                If sIm = "1" Then
                    s = sI ' "i"
                Else
                    's = "i*" + Im.ToString(us)
                    's = "i*" + sIm
                    s = sI + "*" + sIm
                End If
            Else
                'If Im = -1.0 Then
                If sIm = "-1" Then
                    s = "-" + sI ' "-i"
                Else
                    's = "-i*" + Math.Abs(Im).ToString(us)
                    If sIm.Chars(0) = "-" Then
                        sIm = Mid(sIm, 2)
                    End If
                    's = "-i*" + sIm
                    s = "-" + sI + "*" + sIm
                End If
            End If
        ElseIf Im = 0.0 Then
            s = sRe
        ElseIf Im > 0.0 Then
            'If Im = 1.0 Then
            If sIm = "1" Then
                's = Re.ToString(us) + "+" + sI ' "+i"
                s = sRe + "+" + sI
            ElseIf sIm = "-1" Then
                s = sRe + "-" + sI
            Else
                's = Re.ToString(us) + "+i*" + Im.ToString(us)
                s = sRe + "+" + sI + "*" + sIm
            End If
        Else
            'If Im = -1.0 Then
            If sIm = "-1" Then
                's = Re.ToString(us) + "-" + sI ' "-i"
                s = sRe + "-" + sI
            Else
                's = Re.ToString(us) + "-i*" + Math.Abs(Im).ToString(us)
                If sIm.Chars(0) = "-" Then
                    sIm = Mid(sIm, 2)
                End If
                s = sRe + "-" + sI + "*" + sIm
            End If
        End If
        Return s
    End Function
    Public Overrides Function toString() As String
        Return ToStringComplex()
    End Function
    Public Overloads Function toString(cfg As Config) As String
        Return ToStringComplex(cfg)
    End Function
    Public Shared Function toStringV(ByVal vCjo() As Complex) As String
        Dim e1 As String = ""
        Try
            Dim i As Int64
            For i = 0 To vCjo.Length - 1
                e1 += String.Format("({0}) ", i)
                If Not vCjo(i) Is Nothing Then
                    e1 += vCjo(i).toString
                End If
            Next
            e1 += vbCrLf
        Catch ex As Exception

        End Try
        Return e1
    End Function
    Public Shared Function toStringV2(ByVal vCjo() As Complex) As String
        Dim e1 As String = "("
        Try
            Dim i As Int64
            For i = 0 To vCjo.Length - 1
                If Not vCjo(i) Is Nothing Then
                    e1 += vCjo(i).toString
                Else
                    e1 += "---"
                End If
                If i < vCjo.Length - 1 Then
                    e1 += ";"
                End If
            Next
            e1 += ")" + vbCrLf
        Catch ex As Exception

        End Try
        Return e1
    End Function
    Public Shared Function toStringM(ByVal mtxCjo()() As Complex) As String
        Dim e1 As String = ""
        Try
            Dim i, j As Int64
            For i = 0 To mtxCjo.Length - 1
                For j = 0 To mtxCjo(i).Length - 1
                    e1 += String.Format("({0},{1}) ", i, j)
                    If Not mtxCjo(i)(j) Is Nothing Then
                        e1 += mtxCjo(i)(j).toString
                    End If
                Next
                e1 += vbCrLf
            Next
        Catch ex As Exception

        End Try
        Return e1
    End Function
    Public Shared Function toStringM(ByVal mtxCjo(,) As Complex) As String
        Dim e1 As String = ""
        Try
            Dim i, j As Int64
            For i = 0 To mtxCjo.GetLength(0) - 1
                For j = 0 To mtxCjo.GetLength(1) - 1
                    e1 += String.Format("({0},{1}) ", i, j)
                    If Not mtxCjo(i, j) Is Nothing Then
                        e1 += mtxCjo(i, j).toString
                    End If
                Next
                e1 += vbCrLf
            Next
        Catch ex As Exception

        End Try
        Return e1
    End Function
    Public Shared Function sortRootsByType(vCjo() As Complex, _
                                           cfg As Config, _
                                     Optional ByRef o() As String = Nothing) As Complex()()
        Dim vRetCjo()() As Complex = Nothing
        Try
            Dim bSortO As Boolean
            If o IsNot Nothing AndAlso _
            o.Length = vCjo.Length Then
                bSortO = True
            End If
            Dim vRoots(vCjo.Length - 1) As Complex
            Array.Copy(vCjo, vRoots, vCjo.Length)
            Dim vR(-1) As Complex
            Dim i, j As Int32
            Dim vRnd(vCjo.Length - 1) As Complex
            For i = 0 To vRoots.Length - 1
                Dim re As Double = vRoots(i).pRe.ToDouble
                Dim im As Double = vRoots(i).pIm.ToDouble
                Dim reRnd As Double = Math.Round(re, 3)
                Dim imRnd As Double = Math.Round(im, 3)
                If cfg.bRounding Then
                    If reRnd AndAlso imRnd = 0 Then
                        vRoots(i) = New Complex(re)
                        vRnd(i) = New Complex(reRnd)
                    End If
                    If reRnd = 0 AndAlso imRnd Then
                        vRoots(i) = New Complex(0.0, im)
                        vRnd(i) = New Complex(0.0, imRnd)
                    End If
                    If vRnd(i) Is Nothing Then
                        vRnd(i) = New Complex(vRoots(i))
                    End If
                Else
                    vRnd(i) = New Complex(vRoots(i))
                End If
            Next

            ' Split the roots into 4 types:
            ' 1) Real roots
            ' 2) Pure imaginary conjugate roots.
            ' 3) All other conjugate root pairs.
            ' 4) The rest (non-real roots without a conjugate)

            Dim vRealRoots(-1) As Complex, iR As Int64
            Dim vConjugate(-1) As Complex, iC As Int64
            Dim vPureImg(-1) As Complex, iP As Int64
            Dim vNotConjugate(-1) As Complex, iNC As Int64
            Dim dbRR(-1), dbPI(-1), dbC(-1), dbNC(-1) As Double

            ' Fetch real roots:
            For i = 0 To vRoots.Length - 1
                If Not vRoots(i) Is Nothing AndAlso _
                vRoots(i).pIm.IsZero Then
                    ' vRoots(i) is a real root (imaginary part=0)
                    ReDim Preserve vRealRoots(iR), dbRR(iR)
                    vRealRoots(iR) = vRoots(i)
                    dbRR(iR) = vRoots(i).pRe.ToDouble
                    iR += 1
                    vRoots(i) = Nothing
                End If
            Next
            ' Fetch conjugate pairs:
            For i = 0 To vRoots.Length - 1
                If vRoots(i) IsNot Nothing Then
                    For j = 0 To vRoots.Length - 1
                        ' Check for a conjugate:
                        If i <> j AndAlso vRoots(j) IsNot Nothing AndAlso _
                        (vRoots(i).pRe.ToDouble - vRoots(j).pRe.ToDouble = 0 OrElse _
                        vRnd(i).pRe.ToDouble - vRnd(j).pRe.ToDouble = 0) AndAlso _
                        (vRoots(i).pIm.ToDouble + vRoots(j).pIm.ToDouble = 0 OrElse _
                        vRnd(i).pIm.ToDouble + vRnd(j).pIm.ToDouble = 0) Then
                            If vRoots(i).pRe.IsZero Then
                                ReDim Preserve vPureImg(iP), dbPI(iP)
                                vPureImg(iP) = vRoots(i)
                                dbPI(iP) = Math.Abs(vRoots(i).pIm.ToDouble)
                                iP += 1
                            Else
                                ReDim Preserve vConjugate(iC), dbC(iC)
                                vConjugate(iC) = vRoots(i)
                                dbC(iC) = vRoots(i).pRe.ToDouble
                                iC += 1
                            End If

                            If vRoots(i).pRe.IsZero Then
                                ReDim Preserve vPureImg(iP), dbPI(iP)
                                vPureImg(iP) = vRoots(j)
                                dbPI(iP) = Math.Abs(vRoots(i).pIm.ToDouble)
                                iP += 1
                            Else
                                ReDim Preserve vConjugate(iC), dbC(iC)
                                vConjugate(iC) = vRoots(j)
                                dbC(iC) = vRoots(i).pRe.ToDouble
                                iC += 1
                            End If
                            vRoots(i) = Nothing
                            vRoots(j) = Nothing
                            Exit For
                        End If
                    Next
                End If
            Next

            ' Extrat all the rest:
            For i = 0 To vRoots.Length - 1
                If vRoots(i) IsNot Nothing Then
                    ReDim Preserve vNotConjugate(iNC), dbNC(iNC)
                    vNotConjugate(iNC) = vRoots(i)
                    dbNC(iNC) = vRoots(i).pRe.ToDouble
                    iNC += 1
                End If
            Next

            ' Sort the roots according to the values
            ' within the arrays dbRR, dbPI, dbC and dbNC:
            Array.Sort(dbRR, vRealRoots)
            Array.Sort(dbPI, vPureImg)
            For i = 0 To vPureImg.Length - 1 Step 2
                If vPureImg(i).pIm.ToDouble > 0 Then
                    ' Swap to, for example, place -i before +i:
                    Dim cjo As New Complex(vPureImg(i + 1))
                    vPureImg(i + 1) = vPureImg(i)
                    vPureImg(i) = cjo
                End If
            Next

            Array.Sort(dbC, vConjugate)
            For i = 0 To vConjugate.Length - 1 Step 2
                ' order equal real valued through
                ' imaginary value:
                Dim k As Int32 = i
                Dim n As Int32 = 2
                Do While k + 2 < vConjugate.Length AndAlso _
                Math.Round(vConjugate(k + 2).pRe.ToDouble, 3) = _
                Math.Round(vConjugate(i).pRe.ToDouble, 3)
                    k += 2
                    n += 2
                Loop
                If n > 2 Then
                    For j = i To i + n - 1
                        Dim im As Double = vConjugate(j).pIm.ToDouble
                        'im = Math.Abs(im) * 10 + Math.Sign(im)
                        dbC(j) = im
                    Next
                    Array.Sort(dbC, vConjugate, i, n)
                    Dim k1 As Int64 = 0
                    For j = i To i + n / 2 - 1
                        dbC(j) = k1
                        k1 += 2
                    Next
                    k1 = 1
                    For j = i + n - 1 To i + n / 2 Step -1
                        dbC(j) = k1
                        k1 += 2
                    Next
                    Array.Sort(dbC, vConjugate, i, n)
                    Array.Reverse(vConjugate, i, n)
                End If
            Next
            For i = 0 To vConjugate.Length - 1 Step 2
                If vConjugate(i).pIm.ToDouble > 0 Then
                    ' Swap to, for example, place -i before +i:
                    Dim cjo As New Complex(vConjugate(i + 1))
                    vConjugate(i + 1) = vConjugate(i)
                    vConjugate(i) = cjo
                End If
            Next

            Array.Sort(dbNC, vNotConjugate)

            vRetCjo = New Complex()() { _
                        vRealRoots, _
                        vPureImg, _
                        vConjugate, _
                        vNotConjugate}
            If bSortO Then
                Dim o2(o.Length - 1) As String
                Dim i2 As Int64
                For i = 0 To vRetCjo.Length - 1
                    For j = 0 To vRetCjo(i).Length - 1
                        Dim k As Int64
                        For k = 0 To vCjo.Length - 1
                            If (vRetCjo(i)(j) - vCjo(k)).IsZero Then
                                o2(i2) = o(k)
                                i2 += 1
                                If i2 >= o2.Length Then
                                    Exit For
                                End If
                            End If
                        Next
                        If i2 >= o2.Length Then
                            Exit For
                        End If
                    Next
                    If i2 >= o2.Length Then
                        Exit For
                    End If
                Next
                o = o2
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return vRetCjo
    End Function
    Public Shared Function sortRoots(ByVal roots() As Complex, _
                                     Optional ByRef o() As Object = Nothing) As Complex()
        Dim sdb(roots.Length - 1) As String
        Dim cjo(roots.Length - 1) As Complex
        Dim re(-1) As Double
        Dim cjoReOrig(-1) As Complex
        Dim cjo3(-1) As Complex
        Try
            Dim i, k As Int64
            For i = 0 To roots.Length - 1
                Dim c0 As Double = roots(i).pRe.ToDouble
                Dim c1 As Double = roots(i).pIm.ToDouble
                If c1 = 0 Then
                    ReDim Preserve re(re.Length), cjoReOrig(re.Length)
                    re(re.Length - 1) = c0
                    ' to avoid loosing fractional accuracy:
                    cjoReOrig(re.Length - 1) = New Complex(roots(i))
                Else
                    Dim alfa As Double = Math.Atan2(c1, Math.Abs(c0))
                    Dim aux As Double = 100000 - Math.Floor(Math.Abs(alfa) * 10000)
                    aux = aux * 10 + 2 + Math.Sign(c0)
                    sdb(k) = ""
                    If c1 Then
                        sdb(k) = "1"
                    End If
                    Dim db As Double = roots(i).opModulo
                    Dim ent As Double = Math.Abs(c0) ' Math.Floor(db)
                    Dim fra As Double = ent - c0 '  db - ent
                    'If c0 <= 0 Then fra = -ent
                    sdb(k) += aux.ToString(MathGlobal8.us) + "." + _
                        String.Format("{0:00000000000000.00000000000000}.{1:0000000000000.00000000000000}", ent, fra)
                    cjo(k) = New Complex(roots(i))
                    ReDim Preserve cjo3(cjo3.Length)
                    cjo3(cjo3.Length - 1) = New Complex(roots(i))
                    k += 1
                End If
            Next
            'If re.Length Then
            Dim cjo2(cjo.Length - 1) As Complex

            i = 0
            If re.Length Then
                Dim re2(re.Length - 1) As Double
                If o IsNot Nothing Then
                    Array.Copy(re, re2, re.Length)
                End If
                Array.Sort(re, cjoReOrig)
                If o IsNot Nothing Then
                    Array.Sort(re2, o)
                End If
                For i = 0 To re.Length - 1
                    cjo2(i) = cjoReOrig(i) ' New Complex(re(i))
                Next
            End If
            If cjo3.Length Then
                Dim sdb2(sdb.Length - 1) As String
                If o IsNot Nothing Then
                    Array.Copy(sdb, sdb2, sdb.Length)
                End If
                ReDim Preserve sdb(k - 1)
                Array.Sort(sdb, cjo3)
                If o IsNot Nothing Then
                    Array.Sort(sdb2, o)
                End If
                For j As Int64 = 0 To cjo3.Length - 1
                    cjo2(i) = cjo3(j)
                    i += 1
                Next
            End If
            cjo = cjo2
            'End If
        Catch ex As Exception
            Throw ex
        End Try
        Return cjo
    End Function
    Public Shared Function EvalFn(
                    ByVal fn As String,
                    ByVal cjoA As Complex,
                    Optional ByVal angle As exprSubType1 = exprSubType1.radian) As Complex
        Static vTrigonFns() As String =
            New String() {
            "sin", "cos", "tan",
            "sinh", "cosh", "tanh",
            "csc", "sec", "cot",
            "csch", "sech", "coth"
            }
        Dim rCjo As Complex = Nothing
        Try
            fn = LCase(fn)
            Dim bArgNotValid As Boolean = False
            If angle = exprSubType1.degree Then
                cjoA *= New Complex(MathGlobal8.degree2Radians)
            ElseIf angle = exprSubType1.gradian Then
                cjoA *= New Complex(MathGlobal8.centesimal2Rad)
            End If
repiteFn:
            Select Case Len(fn)
                Case 6
                    Select Case fn
                        Case "logtwo" : rCjo = Complex.opLn(cjoA) / Math.Log(2.0)
                        Case "logten" : rCjo = Complex.opLn(cjoA) / Math.Log(10.0)
                    End Select
                Case 5
                    Select Case fn
                        Case "acosh" : rCjo = Complex.opACosH(cjoA)
                        Case "acoth" : rCjo = Complex.opACotH(cjoA)
                        Case "acsch" : rCjo = Complex.opACscH(cjoA)
                        Case "asech" : rCjo = Complex.opASecH(cjoA)
                        Case "asinh" : rCjo = Complex.opASinH(cjoA)
                        Case "atanh" : rCjo = Complex.opAtanH(cjoA)
                        Case "floor" : rCjo = New Complex(Math.Floor(cjoA.pRe.ToDouble))
                            If cjoA.pIm.IsZero = False Then
                                bArgNotValid = True
                            End If
                        Case "round" : rCjo = New Complex(Math.Round(cjoA.pRe.ToDouble))
                            If cjoA.pIm.IsZero = False Then
                                bArgNotValid = True
                            End If
                    End Select
                Case 4
                    Select Case fn
                        Case "coth" : rCjo = Complex.opCotH(cjoA)
                        Case "csch" : rCjo = Complex.opCscH(cjoA)
                        Case "sech" : rCjo = Complex.opSecH(cjoA)
                        Case "acos" : rCjo = Complex.opACos(cjoA)
                        Case "acot" : rCjo = Complex.opAtan(Complex.one / cjoA)
                        Case "acsc" : rCjo = Complex.opASin(Complex.one / cjoA)
                        Case "asec" : rCjo = Complex.opASec(cjoA)
                        Case "asin" : rCjo = Complex.opASin(cjoA)
                        Case "atan" : rCjo = Complex.opAtan(cjoA)
                        Case "conj" : rCjo = Complex.opConjugate(cjoA)
                        Case "cosh" : rCjo = Complex.opCosH(cjoA)
                        Case "norm" : rCjo = New Complex(cjoA.opNorm, 0.0)
                        Case "sign" : rCjo = New Complex(Math.Sign(cjoA.pRe.ToDouble))
                            If cjoA.pIm.IsZero = False Then
                                bArgNotValid = True
                            End If
                        Case "sinh" : rCjo = Complex.opSinH(cjoA)
                        Case "sqrt" : rCjo = cjoA ^ Complex.oneHalf  ' 2013/08/09
                        Case "tanh" : rCjo = Complex.optanh(cjoA)
                    End Select
                Case 3
                    Select Case fn
                        Case "abs" : rCjo = cjoA.opAbs
                        Case "arg" : rCjo = Complex.opArg(cjoA)
                        Case "cos" : rCjo = Complex.opCos(cjoA)
                        Case "cot" : rCjo = Complex.opCot(cjoA)
                        Case "csc" : rCjo = Complex.opCsc(cjoA) ' csc=1/sin
                        Case "exp" : rCjo = Complex.opExp(cjoA)
                        Case "log" : fn = "ln" : GoTo repiteFn ' log()= ln()
                        Case "mod" : rCjo = New Complex(cjoA.opModulo, 0.0)
                        Case "sec" : rCjo = Complex.opSec(cjoA) ' sec
                        Case "sin" : rCjo = Complex.opSin(cjoA)
                        Case "sqr" : rCjo = cjoA ^ Complex.oneHalf  ' 2013/08/09
                        Case "tan" : rCjo = Complex.optan(cjoA)
                    End Select
                Case 2
                    Select Case fn
                        Case "ln" : rCjo = Complex.opLn(cjoA) ' ln()= log()
                        Case "im" : rCjo = New Complex(New Rational(0.0), cjoA.pIm)
                        Case "re" : rCjo = New Complex(cjoA.pRe, New Rational(0.0))
                    End Select
            End Select
            If bArgNotValid Then
                Throw New Exception(
                String.Format(msg8.num(29), fn))
            End If
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
        Return rCjo
    End Function
    Public Enum exprSubType1
        Fn = 1
        mtxFn = 2
        var = 3
        var2 = 4
        op = 6
        mtxOp = 7
        delimiter = 8
        equal = 9
        decBase = 10
        hexBase = 11
        octBase = 12
        binBase = 13
        radian = 16
        degree = 17
        gradian = 18
        logicalOp = 20
        at = 30
    End Enum
    Public Class ConjugateReComparer
        Inherits Comparer(Of Complex)
        Public Overrides Function Compare(x As Complex, y As Complex) As Integer
            Return CInt((x.pRe - y.pRe).ToDouble)
        End Function
    End Class
    Public Class ConjugateImComparer
        Inherits Comparer(Of Complex)
        Public Overrides Function Compare(x As Complex, y As Complex) As Integer
            If x Is y Then Return 0
            Return CInt((x.pIm - y.pIm).ToDouble)
        End Function
    End Class
End Class
