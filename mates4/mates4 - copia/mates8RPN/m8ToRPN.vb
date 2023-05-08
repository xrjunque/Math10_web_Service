
Public Class m8ToRPN
    Public Sub SetStack(vTknStack() As Token, ep As exprParser84)
        Try
            ep.rpn1.eP = ep
            ep.rpn1.oStack = vTknStack
            ep.rpn1.iSt = vTknStack.Length
        Catch ex As Exception

        End Try
    End Sub
    Public Function m8ToRPN(mp As matrixParser) As Token()
        Dim vToken(-1) As Token
        Try
            Dim rpn() As RPNStack = InfixToPostfix(mp)
            vToken = RPNStackToToken(rpn)
        Catch ex As Exception

        End Try
        Return vToken
    End Function
    Function RPNStackToToken(vRPN() As RPNStack) As Token()
        Dim vToken(-1) As Token
        Try
            Dim i As Int32
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
                           New ExprMatrix(Expression.parseExpression(.sFn)), 0, 0, .sFn)
                    ElseIf .sOptor = "(" Then
                        t = New Token(tokenType84.other, -40, New ExprMatrix(0.0), 0, 0, "(")
                    ElseIf .sOptor = ")" Then
                        t = New Token(tokenType84.other, -41, New ExprMatrix(0.0), 0, 0, ")")
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
    Function InfixToPostfix(mp As matrixParser) As RPNStack()
        Dim output(-1) As RPNStack, iO As Int32 = 0
        Dim vStkOptors(-1) As RPNStack, ivS As Int32 = 0
        Static sPrecedence As String = "-+*/^!sfx"
        Static iPrecedence As String = "112234567"
        Static sAssociatio As String = "LLLLRLRRL"
        Try
            Dim input() As Match = mp.ret.cfg.cur.vMc
            Dim i As Int32
            For i = 0 To input.Length - 1
                Dim dbl As Double
                If MathGlobal8.TryParseDbl(input(i), dbl) OrElse
                InStr(input(i).ToString, "&") Then
                    If InStr(input(i).ToString, "&") Then
                        Dim mp1 As New matrixParser
                        mp1.parse(input(i).ToString)
                        dbl = mp1.ret.exprMtx.toDouble
                    End If
                    ReDim Preserve output(iO)
                    output(iO) = New RPNStack With {
                        .opnd = New ExprMatrix(dbl)
                    }
                    output(iO).iToken = i
                    iO += 1
                ElseIf input(i).Groups("var2").Success OrElse
                 input(i).Groups("var").Success Then
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
                            Dim precedenceTop As Int32 = InStr(sPrecedence, sOptorTop) - 1
                            Dim sAssocTop As Char = sAssociatio.Chars(precedenceTop)
                            precedenceTop = Int32.Parse(iPrecedence.Chars(precedenceTop))
                            Dim precedenceInput As Int32 = InStr(sPrecedence, sOptorInput) - 1
                            If precedenceInput > -1 Then
                                precedenceInput = Int32.Parse(iPrecedence.Chars(precedenceInput))
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
                ElseIf (input(i).Groups("col").Success OrElse
                input(i).Groups("row").Success) AndAlso
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

End Class
Public Class RPNStack
    Public opnd As ExprMatrix
    Public sOptor, sFn As String
    Public iToken As Int32
    Public Overrides Function ToString() As String
        Dim e1 As String = sOptor
        If opnd IsNot Nothing Then
            e1 += " " + opnd.ToString
        End If
        e1 += " " + sFn
        Return e1
    End Function
End Class
