Imports System.Text.RegularExpressions
Imports System.Text
Imports System.Drawing
Imports System.Windows.Forms
Imports tr = System.Windows.Forms.TextRenderer

Public Class m8ToRPN
    Public vVar() As String
    Public vVal() As ExprMatrix
    Public input() As Match
    Public Sub SetStack(vTknStack() As Token, ep As exprParser84)
        Try
            ep.rpn1.eP = ep
            ep.rpn1.oStack = vTknStack
            ep.rpn1.iSt = vTknStack.Length
        Catch ex As Exception

        End Try
    End Sub
    Public Function M8ToRPN(mp As matrixParser) As Token()
        Dim vVar(-1) As String
        Dim vVal(-1) As ExprMatrix
        Try
            input = mp.ret.cfg.cur.vMc
            If mp.vars IsNot Nothing Then
                For i As Int64 = 0 To mp.vars.getNamesList.Length - 1
                    ReDim Preserve vVar(i), vVal(i)
                    vVar(i) = mp.vars.getVarNameByID(i)
                    vVal(i) = mp.vars.getValueByID(i)
                Next
            End If
            Me.vVar = vVar
            Me.vVal = vVal
        Catch ex As Exception
            Throw ex
        End Try
        Return M8ToRPN()
    End Function
    Public Function M8ToRPN() As Token()
        Dim vToken(-1) As Token
        Try
            'Dim vAll = New String() {MathGlobal8.sFn,
            '            MathGlobal8.sLgOp,
            '            MathGlobal8.sMtxOp,
            '            MathGlobal8.sOp,
            '            MathGlobal8.sRow,
            '            "(?<resto>.+)"
            '                  }
            'Dim sAll As String = "(?<all>\G" + Join(vAll, "|") + ")"
            'Dim result As Token = Nothing
            Dim i1 As Int64 = 0
            Dim bIsVectorFn As Boolean = False
            Dim bIsPolynom As Boolean = False
            Dim sbMtx As New StringBuilder(50)
            Dim inputMtx(input.Length - 1) As Match, iM As Int64 = 0
            Dim sFn As String = ""
            For i = 0 To input.Length - 1
                If input(i).Groups("custFn").Success Then
                    Return Nothing
                ElseIf input(i).Groups("col").Success Then
                    inputMtx(iM) = input(i) : iM += 1
                ElseIf input(i).Groups("row").Success Then
                    inputMtx(iM) = input(i) : iM += 1
                ElseIf input(i).Groups("mtxOp").Success Then
                    Dim m As Match = Regex.Match(input(i).ToString,
                              MathGlobal8.sOp)
                    inputMtx(iM) = m : iM += 1
                ElseIf input(i).Groups("lp").Success AndAlso
                Not bIsPolynom Then
                    Dim lp As Int64 = 1
                    Dim rp As Int64 = 0
                    i1 = i + 1
                    Dim bVectorOrMatrix As Boolean = False
                    Dim sMtx As New StringBuilder(50)
                    Do While i1 < input.Length
                        If input(i1).Groups("rp").Success Then
                            rp += 1
                            If rp = lp Then
                                Exit Do
                            End If
                        ElseIf input(i1).Groups("lp").Success Then
                            lp += 1
                        ElseIf input(i1).Groups("col").Success Then
                            bVectorOrMatrix = True
                        ElseIf input(i1).Groups("row").Success Then
                            bVectorOrMatrix = True
                        End If
                        sMtx.Append(input(i1).ToString)
                        i1 += 1
                    Loop
                    If bVectorOrMatrix Then
                        If bIsVectorFn Then
                            Dim ve1() As String = Split(sMtx.ToString, vbCrLf)
                            inputMtx(iM) = Regex.Match(ve1(0) + "]", ".*")
                            iM += 1
                            inputMtx(iM) = Regex.Match("[" + ve1(1), ".*")
                            iM += 1
                        ElseIf bIsPolynom Then
                            Dim ve1() As String = Split(sMtx.ToString, ",")
                            ve1(0) = "[" + ve1(0) + "]"
                            ve1(1) = "[" + ve1(1) + "]"
                            inputMtx(iM) = Regex.Match(ve1(0), ".*")
                            iM += 1
                            inputMtx(iM) = Regex.Match(ve1(1), ".*")
                            iM += 1
                            sFn = ""
                            bIsPolynom = False
                        Else
                            sMtx.Insert(0, "[")
                            sMtx.Append("]")
                            Dim e1 As String = sMtx.ToString
                            e1 = Replace(e1, vbCrLf, "]|[")
                            inputMtx(iM) = Regex.Match(e1, ".*")
                            iM += 1
                            bIsVectorFn = False
                        End If
                        i = i1
                    Else
                        inputMtx(iM) = input(i) : iM += 1
                    End If
                ElseIf input(i).Groups("fn").Success Then
                    sFn = LCase(input(i).ToString)
                    Dim pos As Int64 = Array.IndexOf(MathGlobal8.vFn, sFn)
                    Dim tipo As MathGlobal8.FnType = MathGlobal8.vFnType(pos)
                    bIsVectorFn = (tipo = MathGlobal8.FnType.vector)
                    bIsPolynom = (tipo = MathGlobal8.FnType.polyn)
                    inputMtx(iM) = input(i) : iM += 1
                Else
                    inputMtx(iM) = input(i) : iM += 1
                End If
            Next
            ReDim Preserve inputMtx(iM - 1)
            input = inputMtx

            Dim rpn() As RPNStack = InfixToPostfix()
            vToken = RPNStackToToken(rpn)

        Catch ex As Exception

        End Try
        Return vToken
    End Function

    Private Function RPNStackToToken(vRPN() As RPNStack) As Token()
        Dim vToken(-1) As Token
        Try
            Dim i As Int64
            ReDim vToken(vRPN.Length - 1)
            For i = 0 To vRPN.Length - 1
                Dim t As Token = Nothing
                With vRPN(i)
                    If .sOptor Is Nothing OrElse .sOptor.Length = 0 Then
                        t = New Token(tokenType84.oprnd, 0, .opnd, 0, 0, .opnd.ToString)
                    ElseIf InStr("-+*/^!", .sOptor) Then
                        t = New Token(tokenType84.optor, 0, New ExprMatrix(0.0), 0, 0, .sOptor)
                        Select Case .sOptor
                            Case "-" : t.subTipo = optor.substract
                            Case "+" : t.subTipo = optor.add
                            Case "*" : t.subTipo = optor.multiply
                            Case "/" : t.subTipo = optor.divide
                            Case "^" : t.subTipo = optor.power
                            Case "!" : t.subTipo = optor.factorial
                        End Select
                    ElseIf .sOptor = "f" Then
                        t = New Token(tokenType84.fn, 0, New ExprMatrix(0.0), 0, 0, .sFn)
                    ElseIf .sOptor = "s" Then
                        t = New Token(tokenType84.chgSgn, 0, New ExprMatrix(0.0), 0, 0, .sFn)
                    ElseIf .sOptor = "var" Then
                        t = New Token(tokenType84.var, 0,
                           New ExprMatrix(Expression.ParseExpression(.sFn)), 0, 0, .sFn)
                    ElseIf .sOptor = "(" Then
                        t = New Token(tokenType84.other, -40, New ExprMatrix(0.0), 0, 0, "(")
                    ElseIf .sOptor = ")" Then
                        t = New Token(tokenType84.other, -41, New ExprMatrix(0.0), 0, 0, ")")
                    ElseIf .sOptor = "i" OrElse .sOptor = "j" Then
                        t = New Token(tokenType84.imaginary, 0, New ExprMatrix(0.0), 0, 0, .sOptor)
                    Else
                        t = New Token(tokenType84.optor, 0, New ExprMatrix(0.0), 0, 0, UCase(.sOptor))
                        Select Case UCase(.sOptor)
                            Case "AND" : t.subTipo = optor.and
                            Case "OR" : t.subTipo = optor.or
                            Case "XOR" : t.subTipo = optor.xor
                            Case "NOR" : t.subTipo = optor.nor
                            Case "NOT" : t.subTipo = optor.not
                            Case "NAND" : t.subTipo = optor.nand
                        End Select
                    End If
                End With
                t.iToken = vRPN(i).iToken
                vToken(i) = t
            Next
        Catch ex As Exception

        End Try
        Return vToken
    End Function
    Public Function InfixToPostfix(vMc() As Match) As RPNStack()
        Me.input = vMc
        Return InfixToPostfix()
    End Function
    Private Function InfixToPostfix() As RPNStack()
        Dim output(-1) As RPNStack, iO As Int64 = 0
        Dim vStkOptors(-1) As RPNStack, ivS As Int64 = 0
        Static sPrecedence As String = "-+*/^!sfx"
        Static iPrecedence As String = "112234567"
        Static sAssociatio As String = "LLLLRLRRL"
        Try
            'Dim input() As Match = mp.ret.cfg.cur.vMc
            Dim i As Int64
            For i = 0 To input.Length - 1
                Dim dbl As Double
                If MathGlobal8.TryParseDbl(input(i), dbl) OrElse
                InStr(input(i).ToString, "&") OrElse
                input(i).ToString.Chars(0) = "[" OrElse
                input(i).Groups("expr").Success OrElse
                input(i).Groups("cnt").Success Then
                    Dim mp1 As New matrixParser
                    Dim e1 As String = Replace(input(i).ToString, "],[", "][")
                    e1 = Replace(e1, "]|[", "|")
                    e1 = Replace(e1, "][", "|")
                    mp1.parse(e1)
                    ReDim Preserve output(iO)
                    If mp1.ret.diffType = retMtx.diffResponse.resto Then
                        output(iO) = New RPNStack With {
                        .opnd = New ExprMatrix(mp1.ret.exprMtx)
                    }
                    ElseIf mp1.retCjo IsNot Nothing Then
                        Dim eMtx As New ExprMatrix(New Config, mp1.retCjo.Length, 1)
                        For j As Int32 = 0 To mp1.retCjo.Length - 1
                            eMtx.getExpr(j, 0) = New Expression(mp1.retCjo(j))
                        Next
                        output(iO) = New RPNStack With {
                        .opnd = eMtx}
                    End If
                    output(iO).iToken = i
                    iO += 1
                ElseIf input(i).Groups("var2").Success OrElse
                 input(i).Groups("var").Success OrElse
                 input(i).Groups("img").Success Then
                    ReDim Preserve output(iO)
                    output(iO) = New RPNStack With {
                        .sFn = input(i).ToString
                    }
                    If input(i).Groups("img").Success Then
                        output(iO).sOptor = input(i).ToString
                    Else
                        output(iO).sOptor = "var"
                    End If
                    output(iO).iToken = i
                    iO += 1
                ElseIf input(i).Groups("op").Success OrElse input(iO).Groups("lgOp").Success OrElse
                input(i).Groups("fn").Success Then
                    ' While ((there Is an operator at the top of the operator stack with
                    ' greater precedence) Or (the operator at the top of the operator stack has
                    ' equal precedence And
                    ' the operator Is left associative)) And
                    ' (the operator at the top of the stack Is Not a left bracket):
                    '	        pop operators from the Operator stack, onto the output queue.
                    Dim sOptorInput As String = input(i).ToString
                    If sOptorInput = "-" AndAlso (i = 0 OrElse
                             input(i - 1).Groups("lp").Success OrElse
                             input(i - 1).Groups("col").Success OrElse
                             input(i - 1).Groups("row").Success OrElse
                             input(i - 1).Groups("op").Success) Then
                        sOptorInput = "s" ' change sign operator
                    ElseIf input(i).Groups("fn").Success Then
                        sOptorInput = "f" ' function
                    End If
                    Do While ivS
                        Dim sOptorTop As String = vStkOptors(ivS - 1).sOptor
                        If Regex.IsMatch(sOptorTop, "and|or|xor|nand|nor|not") Then
                            sOptorTop = "x"
                        End If
                        If sOptorTop <> "(" Then
                            Dim precedenceTop As Int64 = InStr(sPrecedence, sOptorTop) - 1
                            Dim sAssocTop As Char = sAssociatio.Chars(precedenceTop)
                            precedenceTop = Int64.Parse(iPrecedence.Chars(precedenceTop))
                            Dim precedenceInput As Int64 = InStr(sPrecedence, sOptorInput) - 1
                            If precedenceInput > -1 Then
                                precedenceInput = Int64.Parse(iPrecedence.Chars(precedenceInput))
                                If precedenceTop > precedenceInput OrElse
                            (precedenceTop = precedenceInput AndAlso sAssocTop <> "R") Then
                                    ReDim Preserve output(iO)
                                    If sOptorTop = "x" Then
                                        output(iO) = New RPNStack With {
                                    .sOptor = vStkOptors(ivS - 1).sOptor}
                                    Else
                                        output(iO) = New RPNStack With {
                                    .sOptor = sOptorTop}
                                    End If
                                    output(iO).sFn = vStkOptors(ivS - 1).sFn
                                    output(iO).iToken = vStkOptors(ivS - 1).iToken
                                    iO += 1
                                    ivS -= 1
                                Else
                                    Exit Do
                                End If
                            Else
                                Exit Do
                            End If
                        Else
                            Exit Do
                        End If
                    Loop
                    ' push the read operator onto the operator stack:
                    ReDim Preserve vStkOptors(ivS)
                    vStkOptors(ivS) = New RPNStack With {
                        .sOptor = sOptorInput
                    }
                    vStkOptors(ivS).sFn = IIf(sOptorInput <> "f", "", input(i).ToString)
                    vStkOptors(ivS).iToken = i
                    ivS += 1
                ElseIf input(i).Groups("lp").Success Then ' LP
                    ' push it onto the operator stack:
                    ReDim Preserve vStkOptors(ivS)
                    vStkOptors(ivS) = New RPNStack With {
                        .sOptor = "("
                    }
                    vStkOptors(ivS).iToken = i
                    ivS += 1
                    ' copy "(" to output:
                    ReDim Preserve output(iO)
                    output(iO) = New RPNStack With {
                                    .sOptor = "("}
                    output(iO).sFn = ""
                    output(iO).iToken = i
                    iO += 1
                ElseIf input(i).Groups("rp").Success Then ' RP
                    ' copy ")" to output:
                    ReDim Preserve output(iO)
                    output(iO) = New RPNStack With {
                                    .sOptor = ")"}
                    output(iO).sFn = ""
                    output(iO).iToken = i
                    iO += 1
                    Do
                        'pop operators from the operator stack onto the output queue.
                        'pop the left bracket from the stack.
                        If ivS = 0 Then
                            '/* if the stack runs out without finding a left bracket, then there are
                            'mismatched parentheses. */
                            Throw New Exception("Parentheses not matching")
                        End If
                        Dim sOptorTop As String = vStkOptors(ivS - 1).sOptor
                        If sOptorTop = "(" Then
                            Exit Do
                        End If
                        ReDim Preserve output(iO)
                        output(iO) = New RPNStack With {
                                    .sOptor = sOptorTop}
                        output(iO).sFn = vStkOptors(ivS - 1).sFn
                        output(iO).iToken = vStkOptors(ivS - 1).iToken
                        iO += 1
                        ivS -= 1
                    Loop
                    ' pop the left bracket from the stack
                    ivS -= 1
                    ReDim Preserve vStkOptors(ivS)
                ElseIf (input(i).Groups("col").Success) AndAlso
                ivS Then
                    ' empty the operator stack
                    Do
                        Dim sOptorTop As String = vStkOptors(ivS - 1).sOptor
                        If sOptorTop = "(" Then
                            Exit Do
                        End If
                        ReDim Preserve output(iO)
                        output(iO) = New RPNStack With {
                                    .sOptor = sOptorTop}
                        output(iO).sFn = vStkOptors(ivS - 1).sFn
                        output(iO).iToken = vStkOptors(ivS - 1).iToken
                        iO += 1
                        ivS -= 1
                    Loop
                    ReDim Preserve vStkOptors(ivS)
                    ' do not pop the left bracket from the stack
                End If
            Next
            ' while there are still operator tokens on the stack:
            While ivS
                Dim sOptorTop As String = vStkOptors(ivS - 1).sOptor
                If InStr("()", sOptorTop) Then
                    Throw New Exception("Parentheses not matching")
                End If
                ' pop the operator onto the output queue.
                ReDim Preserve output(iO)
                output(iO) = New RPNStack With {
                                    .sOptor = sOptorTop}
                output(iO).sFn = vStkOptors(ivS - 1).sFn
                output(iO).iToken = vStkOptors(ivS - 1).iToken
                iO += 1
                ivS -= 1
                ReDim Preserve vStkOptors(ivS)
            End While
        Catch ex As Exception

        End Try
        Return output
    End Function

    'Public Function toImage() As Bitmap
    '    Dim RetImg As New Bitmap(1, 1)
    '    Try
    '        Dim i As Int32 = 0
    '        Do While input(i).ToString.Length = 0
    '            i += 1
    '        Loop
    '        RetImg = MatchTobitmap(input(i))
    '        For i = i + 1 To input.Length - 1

    '        Next
    '    Catch ex As Exception

    '    End Try
    '    Return RetImg
    'End Function
    'Private Function MatchTobitmap(m As Match) As Bitmap
    '    Dim bmp As Bitmap = Nothing
    '    Try
    '        Dim fnt = New Font("Courier New", 12.0, FontStyle.Regular)
    '        Dim br As Brush = Brushes.Black
    '        Dim g As Graphics = Nothing
    '        Dim fondo As Color = Color.White
    '        Dim e1 As String = m.ToString
    '        Dim s As Size = tr.MeasureText(e1, fnt)
    '        bmp = New Bitmap(s.Width, s.Height)
    '        g = Graphics.FromImage(bmp)
    '        g.Clear(fondo)
    '        g.DrawString(e1, fnt, br, New Point(0, 0))
    '    Catch ex As Exception

    '    End Try
    '    Return bmp
    'End Function

End Class
Public Class RPNStack
    Public opnd As ExprMatrix
    Public sOptor, sFn As String
    Public iToken As Int64
    Public Overrides Function ToString() As String
        Dim e1 As String = sOptor
        If opnd IsNot Nothing Then
            e1 += " " + opnd.ToString
        End If
        e1 += " " + sFn
        Return e1
    End Function
End Class
Public Class m8ToRPN_B
    Public Function InfixToPostfix(sInput As String,
                                    Optional cfg As Config = Nothing) As RPNStack()
        Dim output(-1) As RPNStack, iO As Int64 = 0
        Dim vStkOptors(-1) As RPNStack, ivS As Int64 = 0
        Static sPrecedence As String = "-+*/^!sfx" ' s=change sign, f=function, x=logical operator
        Static iPrecedence As String = "112234567"
        Static sAssociatio As String = "LLLLRLRRL"
        Try
            If cfg Is Nothing Then
                cfg = New Config
            End If
            'Dim mg As New MathGlobal8
            'mg.Initialize(cfg)
            Dim mg As MathGlobal8 = cfg.mathGlobal

            ' supress leading and trailing white spaces:
            sInput = Regex.Replace(sInput, "(^(\+|\s)+)|((" +
                     MathGlobal8.sCol + "|" + MathGlobal8.sRow + "|\s+)$)", "")

            ' Insert *operator ? :
            sInput = Regex.Replace(sInput, MathGlobal8.sNum +
                                   "(?<v>" + mg.sVar2 + "|" + mg.sVar + "|" + MathGlobal8.sLP + ")",
                                   "${num}*${v}")
            If cfg.sImg = cfg.sImg = "i" Then
                sInput = Regex.Replace(sInput, "(?<![_a-zA-Z0-9]+)(?<img>i)" + MathGlobal8.sNum, "${img}*${num}")
            Else
                sInput = Regex.Replace(sInput, "(?<![_a-zA-Z0-9]+)(?<img>j)" + MathGlobal8.sNum, "${img}*${num}")
            End If

            ' Insert ^operator ? :
            sInput = Regex.Replace(sInput, "(?<v>" + mg.sVar + "|" + MathGlobal8.sRP + ")" + MathGlobal8.sNum,
                                   "${v}^${num}")
            Dim i As Int32

            ' Any subtitution ?
            Dim mc As MatchCollection = Regex.Matches(sInput, MathGlobal8.sSubstitute)
            If mc.Count Then
                Dim sb As New StringBuilder(sInput)
                Dim vi1(-1), vln(-1), vk(-1) As Int32, iv1 As Int32 = 0
                For i = 0 To mc.Count - 1
                    ReDim Preserve vi1(iv1), vln(iv1), vk(iv1)
                    vi1(iv1) = mc(i).Index
                    vln(iv1) = mc(i).Length
                    vk(iv1) = Array.IndexOf(MathGlobal8.vSubstitute, mc(i).ToString)
                    iv1 += 1
                Next
                For i = 0 To iv1 - 1
                    sb.Remove(vi1(i), vln(i))
                    sb.Insert(vi1(i), MathGlobal8.vSubstituteBy(vk(i)))
                Next
                sInput = sb.ToString
            End If
            mc = Regex.Matches(sInput, mg.sAll2)
            Dim input(mc.Count - 1) As Match
            mc.CopyTo(input, 0)
            For i = 0 To input.Length - 1
                Dim dbl As Double
                If MathGlobal8.TryParseDbl(input(i), dbl) OrElse
                InStr(input(i).ToString, "&") OrElse
                input(i).ToString.Chars(0) = "[" Then
                    ReDim Preserve output(iO)
                    Dim e1 As String = Replace(input(i).ToString, "],[", "][")
                    e1 = Replace(e1, "]|[", "|")
                    e1 = Replace(e1, "][", "|")
                    output(iO) = New RPNStack With {
                        .opnd = e1
                    }
                    output(iO).iToken = i
                    iO += 1
                ElseIf (input(i).Groups("var2").Success OrElse
                 input(i).Groups("var").Success OrElse
                 Regex.IsMatch(input(i).ToString, mg.sVar2)) AndAlso
                 Not input(i).Groups("fn").Success Then
                    ReDim Preserve output(iO)
                    output(iO) = New RPNStack With {
                        .sFn = input(i).ToString
                    }
                    output(iO).sOptor = "var"
                    output(iO).iToken = i
                    iO += 1
                ElseIf input(i).Groups("op").Success OrElse input(iO).Groups("lgOp").Success OrElse
                input(i).Groups("fn").Success Then
                    ' While ((there Is an operator at the top of the operator stack with
                    ' greater precedence) Or (the operator at the top of the operator stack has
                    ' equal precedence And
                    ' the operator Is left associative)) And
                    ' (the operator at the top of the stack Is Not a left bracket):
                    '	        pop operators from the Operator stack, onto the output queue.
                    Dim sOptorInput As String = input(i).ToString
                    If sOptorInput = "-" AndAlso (i = 0 OrElse
                             input(i - 1).Groups("lp").Success OrElse
                             input(i - 1).Groups("col").Success OrElse
                             input(i - 1).Groups("row").Success OrElse
                             input(i - 1).Groups("op").Success) Then
                        sOptorInput = "s" ' change sign operator
                    ElseIf input(i).Groups("fn").Success Then
                        sOptorInput = "f" ' function
                    End If
                    Do While ivS
                        Dim sOptorTop As String = vStkOptors(ivS - 1).sOptor
                        If Regex.IsMatch(sOptorTop, "and|or|xor|nand|nor|not") Then
                            sOptorTop = "x"
                        End If
                        If sOptorTop <> "(" Then
                            Dim precedenceTop As Int64 = InStr(sPrecedence, sOptorTop) - 1
                            Dim sAssocTop As Char = sAssociatio.Chars(precedenceTop)
                            precedenceTop = Int64.Parse(iPrecedence.Chars(precedenceTop))
                            Dim precedenceInput As Int64 = InStr(sPrecedence, sOptorInput) - 1
                            If precedenceInput > -1 Then
                                precedenceInput = Int64.Parse(iPrecedence.Chars(precedenceInput))
                                If precedenceTop > precedenceInput OrElse
                            (precedenceTop = precedenceInput AndAlso sAssocTop <> "R") Then
                                    ReDim Preserve output(iO)
                                    If sOptorTop = "x" Then
                                        output(iO) = New RPNStack With {
                                    .sOptor = vStkOptors(ivS - 1).sOptor}
                                    Else
                                        output(iO) = New RPNStack With {
                                    .sOptor = sOptorTop}
                                    End If
                                    output(iO).sFn = vStkOptors(ivS - 1).sFn
                                    output(iO).iToken = vStkOptors(ivS - 1).iToken
                                    iO += 1
                                    ivS -= 1
                                Else
                                    Exit Do
                                End If
                            Else
                                Exit Do
                            End If
                        Else
                            Exit Do
                        End If
                    Loop
                    ' push the read operator onto the operator stack:
                    ReDim Preserve vStkOptors(ivS)
                    vStkOptors(ivS) = New RPNStack With {
                        .sOptor = sOptorInput
                    }
                    vStkOptors(ivS).sFn = IIf(sOptorInput <> "f", "", input(i).ToString)
                    vStkOptors(ivS).iToken = i
                    ivS += 1
                ElseIf input(i).Groups("lp").Success Then ' LP
                    ' push it onto the operator stack:
                    ReDim Preserve vStkOptors(ivS)
                    vStkOptors(ivS) = New RPNStack With {
                        .sOptor = "("
                    }
                    vStkOptors(ivS).iToken = i
                    ivS += 1
                    ' copy "(" to output:
                    ReDim Preserve output(iO)
                    output(iO) = New RPNStack With {
                                    .sOptor = "("}
                    output(iO).sFn = ""
                    output(iO).iToken = i
                    iO += 1
                ElseIf input(i).Groups("rp").Success Then ' RP
                    ' copy ")" to output:
                    ReDim Preserve output(iO)
                    output(iO) = New RPNStack With {
                                    .sOptor = ")"}
                    output(iO).sFn = ""
                    output(iO).iToken = i
                    iO += 1
                    Do
                        'pop operators from the operator stack onto the output queue.
                        'pop the left bracket from the stack.
                        If ivS = 0 Then
                            '/* if the stack runs out without finding a left bracket, then there are
                            'mismatched parentheses. */
                            Throw New Exception("Parentheses not matching")
                        End If
                        Dim sOptorTop As String = vStkOptors(ivS - 1).sOptor
                        If sOptorTop = "(" Then
                            Exit Do
                        End If
                        ReDim Preserve output(iO)
                        output(iO) = New RPNStack With {
                                    .sOptor = sOptorTop}
                        output(iO).sFn = vStkOptors(ivS - 1).sFn
                        output(iO).iToken = vStkOptors(ivS - 1).iToken
                        iO += 1
                        ivS -= 1
                    Loop
                    ' pop the left bracket from the stack
                    ivS -= 1
                    ReDim Preserve vStkOptors(ivS)
                ElseIf input(i).Groups("col").Success OrElse
                input(i).Groups("row").Success Then
                    ' empty the operator stack
                    Do While ivS
                        Dim sOptorTop As String = vStkOptors(ivS - 1).sOptor
                        If sOptorTop = "(" Then
                            Exit Do
                        End If
                        ReDim Preserve output(iO)
                        output(iO) = New RPNStack With {
                                    .sOptor = sOptorTop}
                        output(iO).sFn = vStkOptors(ivS - 1).sFn
                        output(iO).iToken = vStkOptors(ivS - 1).iToken
                        iO += 1
                        ivS -= 1
                    Loop
                    ReDim Preserve vStkOptors(ivS)
                    ' do not pop the left bracket from the stack
                    ReDim Preserve output(iO)
                    If input(i).Groups("col").Success Then
                        output(iO) = New RPNStack With {
                                    .sOptor = "col"}
                        output(iO).sFn = ","
                    Else
                        output(iO) = New RPNStack With {
                                    .sOptor = "row"}
                        output(iO).sFn = "|"
                    End If
                    output(iO).iToken = i
                    iO += 1
                ElseIf input(i).Groups("img").Success Then
                    ReDim Preserve output(iO)
                    output(iO) = New RPNStack With {
                        .opnd = input(i).ToString
                    }
                    output(iO).iToken = i
                    iO += 1
                End If
            Next
            ' while there are still operator tokens on the stack:
            While ivS
                Dim sOptorTop As String = vStkOptors(ivS - 1).sOptor
                If InStr("()", sOptorTop) Then
                    Throw New Exception("Parentheses not matching")
                End If
                ' pop the operator onto the output queue.
                ReDim Preserve output(iO)
                output(iO) = New RPNStack With {
                                    .sOptor = sOptorTop}
                output(iO).sFn = vStkOptors(ivS - 1).sFn
                output(iO).iToken = vStkOptors(ivS - 1).iToken
                iO += 1
                ivS -= 1
                ReDim Preserve vStkOptors(ivS)
            End While
        Catch ex As Exception

        End Try
        Return output

    End Function
    Public Class RPNStack
        Public opnd As String
        Public sOptor, sFn As String
        Public iToken As Int64
        Public Overrides Function ToString() As String
            Dim e1 As String = sOptor
            If opnd IsNot Nothing Then
                e1 += " " + opnd.ToString
            End If
            e1 += " " + sFn
            Return e1
        End Function
    End Class

End Class