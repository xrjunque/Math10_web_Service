Public Class Token
    Public Shared us As System.Globalization.CultureInfo = Config84.us
    Friend tipo As tokenType84
    Friend subTipo As optor
    Friend pos As Int64
    Friend iToken As Int64
    Friend dblVal As ExprMatrix
    Friend eP As exprParser84
    Friend sTkn As String
    'Friend Poly As Polynomial84
    Friend Sub New(tipo As tokenType84, subTipo As Int64, eMtx As ExprMatrix, pos As Int64, iToken As Int64, strTkn As String)
        Me.tipo = tipo
        Me.subTipo = subTipo
        Me.dblVal = New ExprMatrix(eMtx)
        Me.pos = pos
        Me.iToken = iToken
        Me.sTkn = strTkn
    End Sub
    Friend Function Copy() As Token
        Dim cpy As New Token(tipo, subTipo, dblVal, pos, iToken, sTkn)
        'cpy.eP = eP
        'If tipo = tokenType84.Polynomial84 Then
        '    cpy.Poly = New Polynomial84(Poly)
        'End If
        Return cpy
    End Function
    Public Shared Operator -(ATkn As Token) As Token
        Return New Token(ATkn.tipo, ATkn.subTipo, -ATkn.dblVal, ATkn.pos, ATkn.iToken, ATkn.sTkn)
    End Operator
    Public Shared Operator -(ATkn As Token, BTkn As Token)
        Return op("-", ATkn, BTkn)
    End Operator
    Public Shared Function op(sOp As Char, ATkn As Token, BTkn As Token) As Token
        Dim CTkn As Token = ATkn.Copy
        Try
            Dim i As Int64 = CTkn.tipo + BTkn.tipo * 4
            'oprnd = 0 (num)
            Select Case sOp
                Case "+"
                    CTkn.dblVal += BTkn.dblVal
                Case "-"
                    CTkn.dblVal -= BTkn.dblVal
                Case "*"
                    CTkn.dblVal *= BTkn.dblVal
                Case "/"
                    CTkn.dblVal /= BTkn.dblVal
                Case "^"
                    CTkn.dblVal ^= BTkn.dblVal
                Case "m" ' mod
                    'CTkn.dblVal = CTkn.dblVal Mod BTkn.dblVal
                    Dim eM As New ExprMatrix(New Config, 1, 2)
                    eM.getExpr(0, 0) = New Expression(CTkn.dblVal.getCurExpr)
                    eM.getExpr(0, 1) = New Expression(CTkn.dblVal.getCurExpr)
                    CTkn.dblVal = exprParser.evalFn("mod", eM)
            End Select
            CTkn.sTkn = CTkn.dblVal.ToString
        Catch ex As Exception
            CTkn.eP.err = ex
        End Try
        Return CTkn
    End Function
    Public Shared Operator +(ATkn As Token, BTkn As Token) As Token
        Return op("+", ATkn, BTkn)
    End Operator
    Public Shared Operator *(ATkn As Token, BTkn As Token) As Token
        Return op("*", ATkn, BTkn)
    End Operator
    Public Shared Function opMultDetail(ATkn As Token, BTkn As Token) As String
        If ATkn.tipo <> tokenType84.Polynomial84 OrElse BTkn.tipo <> tokenType84.Polynomial84 Then
            Return ""
        End If
        Dim sRet As String = String.Empty
        Try
            Dim cfg As New Config
            Dim vTermsA() As Polynomial = ATkn.dblVal.getExpr(0, 0).getPolynomial.splitIntoTerms
            Dim vTermsB() As Polynomial = BTkn.dblVal.getExpr(0, 0).getPolynomial.splitIntoTerms

            For i As Int64 = 0 To vTermsA.Length - 1
                For j As Int64 = 0 To vTermsB.Length - 1
                    Dim scur As String =
                        vTermsA(i).toStringPoly(cfg) +
                        " * " +
                        vTermsB(j).toStringPoly(cfg)
                    If i + j AndAlso scur.Chars(0) <> "-" Then
                        sRet += "+ " + scur
                    Else
                        sRet += " " + scur
                    End If
                Next
            Next
        Catch ex As Exception
            Return ""
        End Try
        Return sRet
    End Function
    Public Shared Function opDivDetail(ByRef result As String,
                                       ATkn As Token,
                                       BTkn As Token) As Token
        If ATkn.tipo <> tokenType84.Polynomial84 OrElse BTkn.tipo <> tokenType84.Polynomial84 Then
            result = String.Empty
            Return ATkn / BTkn
        End If
        Dim C As Token = ATkn.Copy
        Try
            'ATkn.dblVal.cfg = ATkn.dblVal.cfg
            Dim bDetail As Boolean = ATkn.dblVal.cfg.bDetail
            ATkn.dblVal.cfg.bDetail = True
            ATkn.dblVal.cfg.oDetail = New Detall(Nothing)

            C.dblVal.getExpr(0, 0) = New Expression(
                ATkn.dblVal.getExpr(0, 0).getPolynomial / BTkn.dblVal.getExpr(0, 0).getPolynomial)
            result = ATkn.dblVal.cfg.oDetail.ToString(outputMessage.RichTextFormat)
            ATkn.dblVal.cfg.bDetail = bDetail
        Catch ex As Exception
            Throw ex
        End Try
        Return C
    End Function
    Public Shared Operator /(ATkn As Token, BTkn As Token)
        Return op("/", ATkn, BTkn)
    End Operator
    Public Shared Operator ^(ATkn As Token, BTkn As Token)
        Return op("^", ATkn, BTkn)
    End Operator
    Public Shared Operator Mod(ATkn As Token, BTkn As Token)
        Dim CTkn As Token = ATkn.Copy
        CTkn.dblVal = New ExprMatrix(CTkn.dblVal.toDouble Mod BTkn.dblVal.toDouble)
        Return CTkn
    End Operator
    Public Shared Operator And(ATkn As Token, BTkn As Token)
        Dim CTkn As Token = ATkn.Copy
        CTkn.dblVal = New ExprMatrix(CTkn.dblVal.toDouble And BTkn.dblVal.toDouble)
        Return CTkn
    End Operator
    Public Shared Operator Or(ATkn As Token, BTkn As Token)
        Dim CTkn As Token = ATkn.Copy
        CTkn.dblVal = New ExprMatrix(CTkn.dblVal.toDouble Or BTkn.dblVal.toDouble)
        Return CTkn
    End Operator
    Public Shared Operator Xor(ATkn As Token, BTkn As Token)
        Dim CTkn As Token = ATkn.Copy
        CTkn.dblVal = New ExprMatrix(CTkn.dblVal.toDouble Xor BTkn.dblVal.toDouble)
        Return CTkn
    End Operator
    Public Shared Operator Not(ATkn As Token)
        Dim CTkn As Token = ATkn.Copy
        CTkn.dblVal = New ExprMatrix(Not CTkn.dblVal.toDouble)
        Return CTkn
    End Operator
    Public Shared Function opFuntion(sFunction As String, opA As Token, opB As Token) As Token
        Dim ret As New Token(tokenType84.expression, 0, New ExprMatrix(0.0), 0, 0, "")
        Try
            Dim eM As ExprMatrix = Nothing
            If opB IsNot Nothing Then
                eM = New ExprMatrix(New Config, 1, 2)
                eM.getExpr(0, 0) = opA.dblVal.getExpr(0, 0)
                eM.getExpr(0, 1) = opB.dblVal.getExpr(0, 0)
            Else
                eM = opA.dblVal
            End If
            Dim eR As ExprMatrix = Nothing
            If LCase(sFunction) = "mod" Then
                eR = exprParser.evalFn(sFunction, opA.dblVal, moduloOpA:=opB.dblVal)
            Else
                eR = exprParser.evalFn(sFunction, eM)

            End If
            ret.tipo = tokenType84.oprnd
            ret.dblVal = eR
            ret.sTkn = eR.ToString()
        Catch ex As Exception
            Throw ex
        End Try
        Return ret
    End Function
    Public ReadOnly Property toDouble As Double
        Get
            Return dblVal.toDouble
        End Get
    End Property
    Public Overloads Function ToString(cfg As Config) As String
        Try
            If cfg.bRounding Then
                Dim eM As New ExprMatrix(dblVal)
                If eM.Rows = 1 AndAlso eM.Cols = 1 AndAlso eM.getExpr(0, 0).IsPolynomial Then
                    Dim pa As Polynomial = eM.getExpr(0, 0).getPolynomial
                    For i As Int64 = 0 To pa.cf.Length - 1
                        Dim re As Double = pa.cf(i).pRe.ToDouble
                        Dim im As Double = pa.cf(i).pIm.ToDouble
                        re = Math.Round(re, 10)
                        im = Math.Round(im, 10)
                        pa.cf(i) = New Complex(re, im)
                    Next
                    Return pa.ToString()
                End If
            End If
        Catch ex As Exception

        End Try
        Return dblVal.ToString
    End Function
    Public Overrides Function ToString() As String
        Return dblVal.ToString()
    End Function
    Friend Function ToStringStackTkn(cfg As Config,
                        Optional vVars() As String = Nothing,
                        Optional vValues() As ExprMatrix = Nothing,
                        Optional bShowVarValues As Boolean = True) As String
        Try
            Dim us As New Globalization.CultureInfo("en-US")
            Dim ni As Globalization.NumberFormatInfo = us.NumberFormat
            If tipo = tokenType84.oprnd Then
                Return dblVal.ToStringExprMtx(cfg) ' exprMatrix
            ElseIf tipo = tokenType84.optor Then
                Select Case subTipo
                    Case optor.and : Return "AND"
                    Case optor.or : Return "OR"
                    Case optor.nor : Return "NOR"
                    Case optor.xor : Return "XOR"
                    Case optor.not : Return "NOT"
                    Case optor.nand : Return "NAND"
                End Select
                Return Chr(subTipo)
            ElseIf tipo = tokenType84.chgSgn Then
                Return "- (unary)" ' change sign
            ElseIf tipo = tokenType84.var Then
                Dim i As Int64 = -1
                If vVars IsNot Nothing Then
                    i = Array.IndexOf(vVars, sTkn)
                    If i > -1 AndAlso vValues IsNot Nothing AndAlso
                    i <= vValues.Length AndAlso bShowVarValues Then
                        Return sTkn + " = " + vValues(i).ToString()
                    End If
                End If
                Return sTkn  ' var
            ElseIf tipo = tokenType84.fn Then
                Return sTkn  ' function
            ElseIf tipo = tokenType84.optor Then
                Return Chr(subTipo) ' operator
            Else 'If tipo = tokenType84.other Then
                Return sTkn
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return ""
    End Function
End Class
Friend Enum tokenType84
    oprnd = 0
    var
    fn
    Polynomial84
    expression
    optor
    chgSgn
    matrix
    other
    imaginary
End Enum
