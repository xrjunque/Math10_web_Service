Imports System.Text.RegularExpressions
Imports System.Text

<Serializable()>
Public Class exprParser84
    Public Sub New()
        Me.cfg = New Config84
    End Sub

    Public Shared us As System.Globalization.CultureInfo = Config84.us
    Friend iRe As Int32
    Private iToken, lenToken As Int32
    Private opndCurPos As Int32
    Private optoriTkn, chgSgniTkn As Int32
    Private optorPos, chgSgnPos As Int32
    Private LP, RP As Int32
    Private IsFunction As Boolean = False
    Private nOpnd, curOpnd As Int32, curOptor As optor
    Private oldType As tknGnralType
    Private bInsChar As Boolean
    Friend bVarsOf1Letter As Boolean
    Private bValidate As Boolean
    Friend sbExpr As StringBuilder
    Private sLastTkn As String
    Private vLogOp() As String = Config84.vLogOp
    Friend cfg As Config84
    Private reVar2 As New Regex(Config84.sVar2, RegexOptions.Compiled)
    Private reDec As New Regex(Config84.sNum, RegexOptions.Compiled)
    Private reHex As New Regex(Config84.sHex, RegexOptions.Compiled)
    Private reOct As New Regex(Config84.sOct, RegexOptions.Compiled)
    Private rebin As New Regex(Config84.sBin, RegexOptions.Compiled)
    Private err1 As Exception
    Public rpn1 As RPN_Stack
    Private curBase As numBase = numBase.decimal


    Public Function Eval(ByRef Result As Token, Optional vNames() As String = Nothing,
                          Optional vValues() As ExprMatrix = Nothing,
                                Optional ByRef cplxRoots() As Complex84 = Nothing) As Boolean
        Return rpn1.Eval(Result, vNames, vValues, cplxRoots)
    End Function
    Public Function Parse(strToParse As String,
                          Optional bValidate As Boolean = True) As Boolean
        Try
            Me.bValidate = bValidate
            If bValidate Then
                ' supress leading and trailing white spaces:
                strToParse = Regex.Replace(strToParse, "(^(\+|\s)+)|((" +
                         Config84.sCol + "|" + Config84.sRow + "|\s+)$)", "")
            End If
            If strToParse.Length = 0 Then
                err = New msg884(Me, 1) ' empty string
                Return False
            End If
            If Regex.IsMatch(strToParse, "\[|\{") Then
                strToParse = Regex.Replace(strToParse, "\[|\{", "(")
                strToParse = Regex.Replace(strToParse, "\]|\}", ")")
            End If
            If rpn1.oStack IsNot Nothing AndAlso
            rpn1.oStack.Length >= strToParse.Length * 3 / 2 Then
                rpn1.Clear()
            Else
                rpn1 = New RPN_Stack(Me, strToParse.Length * 3 / 2)
            End If

            iRe = 0 : nOpnd = 0 : LP = 0 : RP = 0 : iToken = -1
            sLastTkn = "" : oldType = 0
            err = Nothing
            sbExpr = New StringBuilder(strToParse)
            nextExpr()



            If err IsNot Nothing Then
                Return False
            End If
            If bValidate AndAlso LP > RP AndAlso InStr(strToParse, ",") = 0 Then
                err = New msg884(Me, 10) ' missing matching RP
                Return False
            End If
            ReDim Preserve rpn1.oStack(rpn1.iSt - 1)
        Catch ex As Exception
            err = ex
            Return False
        End Try
        Return True
    End Function

    Public Function ToStringStack(ByRef sResult As String, Optional vNames() As String = Nothing,
                          Optional vValues() As ExprMatrix = Nothing,
                          Optional bGroup As Boolean = False,
                          Optional bShowVarValues As Boolean = True) As Boolean
        Return rpn1.ToStringStack(sResult, vNames, vValues, bGroup, bShowVarValues)
    End Function
    Public Function ToStringDetail(ByRef result As Token,
                                   ByRef sResult As String,
                        Optional vNames() As String = Nothing,
                        Optional vValues() As ExprMatrix = Nothing,
                        Optional format As outputMessage = outputMessage.plainText,
                        Optional vTkn() As Token = Nothing) As Boolean
        Return rpn1.ToStringDetail(result, sResult, vNames, vValues, format, vTkn)
    End Function

    Private Sub nextExpr()          ' - +
        Try
            nextTerm()
            Do While curOptor = optor.add OrElse curOptor = optor.substract
                Dim oStk As New Token(tokenType84.optor, curOptor,
                        New ExprMatrix(0.0), optorPos, optoriTkn, Chr(curOptor))
                nextTerm()
                rpn1.Add(oStk) ' Add the operator to the stack
            Loop
        Catch ex As Exception
            err = ex
        End Try
    End Sub
    Private Sub nextTerm()  '       * /
        Try
            nextPow()
            Do While curOptor = optor.multiply OrElse curOptor = optor.divide
                Dim oStk As New Token(tokenType84.optor, curOptor,
                        New ExprMatrix(0.0), optorPos, optoriTkn, Chr(curOptor))
                nextPow()
                rpn1.Add(oStk) ' Add the operator to the stack
            Loop
        Catch ex As Exception
            err = ex
        End Try
    End Sub
    Private Sub nextPow() '    ^ !
        Dim sgn As Int32
        Try

            nextToken(sgn)

            Do While curOptor = optor.power OrElse curOptor = optor.modulo
                Dim oStk() As Token =
                     {New Token(tokenType84.optor, curOptor,
                        New ExprMatrix(0.0), optorPos, optoriTkn, Chr(curOptor))
                     }
                If curOptor = optor.power Then ' ^
                    Dim iv As Int32 = 0
                    Dim vSgn(0), vChgSgnCurPos(0), vChgiTkn(0) As Int32
                    vChgiTkn(iv) = chgSgniTkn
                    vChgSgnCurPos(iv) = chgSgnPos
                    nextToken(vSgn(iv))
                    Do While curOptor = optor.power
                        iv += 1
                        If iv >= vSgn.Length Then
                            ReDim Preserve vSgn(iv), vChgSgnCurPos(iv),
                                 vChgiTkn(iv), oStk(iv)
                        End If
                        oStk(iv) = New Token(tokenType84.optor, 94,
                            New ExprMatrix(0.0), opndCurPos, optoriTkn, "^")
                        vChgiTkn(iv) = chgSgniTkn
                        vChgSgnCurPos(iv) = chgSgnPos
                        nextToken(vSgn(iv))
                    Loop
                    For i As Int32 = iv To 0 Step -1
                        If vSgn(i) = -1 Then
                            ' Add change sign to the stack:
                            rpn1.Add(New Token(
                                     tokenType84.chgSgn, 0, New ExprMatrix(0.0),
                                     vChgSgnCurPos(i), vChgiTkn(i), "^"))
                        End If
                        ' Add operator "^" power to the stack:
                        rpn1.Add(oStk(i)) ' operator
                    Next
                Else
                    ' %
                    nextToken(sgn)
                    rpn1.Add(oStk(0)) ' Add operator "%" power to the stack:
                    sgn = 1
                End If
                nOpnd -= 1
            Loop
            If sgn = -1 Then
                rpn1.Add(New Token(
                         tokenType84.chgSgn, 0, New ExprMatrix(0.0), chgSgnPos,
                         chgSgniTkn, "-"))
            End If
        Catch ex As Exception
            err = ex
        End Try
    End Sub


    Private Sub nextToken(ByRef sgn As Int32)
        Dim c As Int32
        Dim bNotUnary As Boolean
        Try
            sgn = 1
            Do
                If iRe < sbExpr.Length Then
                    iToken += 1
retry:
                    c = AscW(sbExpr.Chars(iRe))
                    If c = 32 Then
                        sbExpr = sbExpr.Remove(iRe, 1) : GoTo retry
                    ElseIf c = 45 OrElse c = 43 OrElse c = 42 OrElse c = 47 _
                    OrElse c = 94 OrElse c = 37 OrElse c = 33 Then  '      O P E R A T O R
                        ' -  +  *  /  ^  %  !  .  :  ÷
                        '45 43 42 47 94 37 33 46 58 247
                        If bValidate AndAlso
                        Not Validate(tknGnralType.optor, Chr(c)) Then
                            Exit Do
                        End If
                        If c <> 33 Then
                            ' Operator will be added to the
                            ' stack in its way ("up") back to the callers
                            If bNotUnary OrElse
                             (c <> 45 AndAlso c <> 43) Then
                                If c = 58 Then c = 47 ' convert ":" into "/"
                                bNotUnary = False
                                curOptor = c
                                optoriTkn = iToken
                                optorPos = iRe
                                iRe += 1
                                Exit Try
                            ElseIf c = 45 Then
                                ' unary "-", c=45: change sign
                                sgn *= -1
                                If sgn = -1 Then
                                    chgSgniTkn = iToken
                                    chgSgnPos = iRe
                                End If
                            Else
                                ' unary "+", c=43: do nothing
                            End If
                            iRe += 1
                        Else
                            ' !
                            ' Add "!" operator to the stack:
                            rpn1.Add(New Token(
                                     tokenType84.optor, 33, New ExprMatrix(0.0),
                                     iRe, iToken, "!")) ' operator
                            iRe += 1
                        End If
                    ElseIf curBase = numBase.decimal AndAlso
                    ((48 <= c AndAlso c <= 57) OrElse c = 46) Then '       N  U  M  B  E  R
                        Dim mNum As Match = reDec.Match(sbExpr.ToString, iRe)
                        If mNum.Success Then
                            If bValidate AndAlso
                            Not Validate(tknGnralType.num, mNum.ToString) Then
                                If bInsChar Then
                                    GoTo retry
                                End If
                                Exit Do
                            End If
                            If bInsChar Then
                                c = bInsChar : GoTo retry
                            End If
                            Dim dbl As Double
                            If Config84.TryParseDbl(mNum.ToString, dbl) Then
                                ' Add the number (operand) to the stack:
                                rpn1.Add(New Token(tokenType84.oprnd,
                                                0, New ExprMatrix(dbl), iRe,
                                                iToken, mNum.ToString)) ' numeric

                            Else
                                err = New msg884(Me, 6, New String() {mNum.ToString})
                            End If
                            iRe += mNum.Length
                        Else
                            err = New msg884(Me, 6, New String() _
                                         {sbExpr.ToString.Substring(iRe)})
                            Exit Do
                        End If
                    ElseIf curBase <> numBase.decimal AndAlso
                    ((curBase = numBase.hexadecimal AndAlso
                            reHex.IsMatch(sbExpr.ToString, iRe)) OrElse
                    (curBase = numBase.octal AndAlso
                            reOct.IsMatch(sbExpr.ToString, iRe)) OrElse
                    (curBase = numBase.binary AndAlso
                            rebin.IsMatch(sbExpr.ToString, iRe))) Then
                        Dim mNum As Match = Nothing

                        Select Case curBase '                         BASE <> DECIMAL ( HEXA, OCTAL, BINARY )

                            Case numBase.hexadecimal : mNum = reHex.Match(sbExpr.ToString, iRe)
                            Case numBase.octal : mNum = reOct.Match(sbExpr.ToString, iRe)
                            Case numBase.binary : mNum = rebin.Match(sbExpr.ToString, iRe)
                        End Select
                        If bValidate AndAlso
                        Not Validate(tknGnralType.num, mNum.ToString) Then
                            If bInsChar Then
                                GoTo retry
                            End If
                            Exit Do
                        End If
                        If bInsChar Then
                            c = bInsChar : GoTo retry
                        End If
                        Dim dbl As Double
                        If tryParseHexDecOctBinBaseNum(mNum.ToString, dbl) Then
                            rpn1.Add(New Token(tokenType84.oprnd,
                                            0, New ExprMatrix(dbl), iRe,
                                            iToken, mNum.ToString)) ' numeric
                            iRe += mNum.Length
                        Else
                            err = New msg884(Me, 6, New String() _
                                         {sbExpr.ToString.Substring(iRe)})
                            Exit Do
                        End If
                    Else

                        ' Is a function?
                        Dim iRe2 As Int32 = iRe
                        Do
                            Dim c1 As Char = sbExpr.ToString.Chars(iRe2)
                            If ("a" <= c1 AndAlso c1 <= "z") OrElse
                            ("A" <= c1 AndAlso c1 <= "Z") Then
                                iRe2 += 1
                            Else
                                Exit Do
                            End If
                        Loop While iRe2 < sbExpr.Length
                        If iRe < iRe2 Then
                            Dim sFnOrVar As String = LCase(sbExpr.ToString.Substring(iRe, iRe2 - iRe))
                            Dim iFn As Int32 = Array.IndexOf(Config84.vFn, sFnOrVar)

                            If iFn > -1 Then

                                ' Is a function declared in Config84.vFn()

                                ' Is there a valid exponent?
                                ' For example, cos2(0.3) --> cos(0.3)^2=cos(0.3)*cos(0.3)
                                Dim exp As Int32 = 1
                                Dim iRe3 As Int32 = InStr(iRe2, sbExpr.ToString, "(")
                                If iRe2 < iRe3 AndAlso
                                Int32.TryParse(sbExpr.ToString.Substring(iRe2, iRe3 - iRe2 - 1), exp) Then
                                    If exp < 0 OrElse exp > 9 Then ' only up to 9th power
                                        err = New msg884(Me, 2, New String() {sFnOrVar, exp.ToString})
                                        Exit Do
                                    End If
                                End If

                                Dim iFnToken As Int32 = iToken     '            F  U  N  C  T  I  O  N
                                Dim iReFn As Int32 = iRe
                                If sFnOrVar = "mod" Then
                                    sbExpr.Remove(iRe, 3)
                                    sbExpr.Insert(iRe, "%")
                                    GoTo retry
                                End If
                                If bValidate AndAlso
                                Not Validate(tknGnralType.fn, sFnOrVar) Then
                                    If bInsChar Then
                                        GoTo retry
                                    End If
                                    Exit Do
                                End If

                                iRe = iRe2
                                If iRe3 > 0 Then iRe = iRe3 - 1
                                Dim sgn0 As Int32 = 1
                                ' Get the function's argument and
                                ' add to the stack:
                                Do
                                    IsFunction = True
                                    nextExpr() 'nextToken sgn0, True)
                                Loop While sbExpr.Chars(iRe - 1) = ","
                                IsFunction = False

                                ' NOTE 1.
                                ' Execution will return back here because of setting
                                ' optional parameter in nextToken() to TRUE
                                ' (see NOTE 2 below) 
                                ' If there is any operator behind the function,
                                ' for ex. sin(pi)*2, execution will continue as usual
                                ' getting token "*" and, 'up' back in nextTerm(), call
                                ' nextPow() adding, in the course of the call, token "2"
                                ' to the stack and, 'up' back in nextTerm() add the "*" operator
                                ' to the stack, as expected.

                                If sgn0 = -1 Then
                                    ' Add change argument's sign to the stack:
                                    ' as in for example sin(-pi/2))
                                    rpn1.Add(New Token(
                                        tokenType84.chgSgn, 0, New ExprMatrix(0.0), iRe, iToken, "-"))
                                End If
                                ' Add the function to the stack:
                                rpn1.Add(New Token(
                                         tokenType84.fn, -1, New ExprMatrix(0.0), iReFn,
                                         iFnToken, sFnOrVar)) '  function
                                If exp <> 0 Then
                                    ' Add the exponent to the stack:
                                    rpn1.Add(New Token(tokenType84.oprnd,
                                                    0, New ExprMatrix(exp), iRe,
                                                    iToken + 2, exp.ToString))
                                    ' Add power operator to the stack:
                                    rpn1.Add(New Token(
                                        tokenType84.optor, 94,
                                        New ExprMatrix(0.0), opndCurPos, iToken + 1, "^"))
                                    iToken += 2
                                End If
                            ElseIf Array.IndexOf(vLogOp, sFnOrVar) > -1 Then
                                Dim indexLogOp As optor = Array.IndexOf(vLogOp, sFnOrVar) + 1
                                If bValidate AndAlso
                                Not Validate(tknGnralType.andOrXorNandNor, sFnOrVar) Then
                                    If bInsChar Then
                                        GoTo retry
                                    End If
                                    Exit Do
                                End If
                                Dim iLogiToken As Int32 = iToken        '    L O G I C A L   O P E R A T O R
                                Dim iLogRe As Int32 = iRe
                                iRe += sFnOrVar.Length
                                ' Get next operand:
                                'Dim sgn0 As Int32
                                IsFunction = True
                                nextExpr()
                                IsFunction = False
                                'If sgn0 = -1 Then
                                '    ' Add change argument's sign to the stack:
                                '    rpn1.Add(New Token(
                                '        tokenType84.chgSgn, 0, New ExprMatrix(0.0), iRe, iToken, "-"))
                                'End If
                                ' Add the logical operator to the stack:
                                rpn1.Add(New Token(
                                         tokenType84.optor, indexLogOp, New ExprMatrix(0.0), iLogRe,
                                          iLogiToken, sFnOrVar)) '  logical operator

                            Else
                                Dim posConst As Int32 = Array.IndexOf(Config84.vConst, sFnOrVar)
                                If posConst >= 0 Then
                                    If bValidate AndAlso
                                    Not Validate(tknGnralType.Cnt, sFnOrVar) Then    '    C O N S T A N T
                                        If bInsChar Then
                                            GoTo retry
                                        End If
                                        Exit Do
                                    End If
                                    opndCurPos = iRe
                                    Select Case sFnOrVar
                                        Case "pi"
                                            ' Add "pi" to the stack:
                                            rpn1.Add(New Token(tokenType84.oprnd,
                                                0, New ExprMatrix(Math.PI), iRe,
                                                iToken, sFnOrVar)) ' constant
                                            ' Add "e" to the stack:
                                        Case "e"
                                            rpn1.Add(New Token(tokenType84.oprnd,
                                                0, New ExprMatrix(Math.E), iRe,
                                                iToken, sFnOrVar)) ' constant
                                    End Select
                                    iRe = iRe2
                                Else
                                    opndCurPos = iRe '                             V A R I A B L E
                                    If bValidate AndAlso
                                    Not Validate(tknGnralType.variable, sFnOrVar) Then
                                        If bInsChar Then
                                            GoTo retry
                                        End If
                                        Exit Do
                                    End If
                                    ' Add the variable to the stack:
                                    rpn1.Add(New Token(
                                             tokenType84.var, -1, New ExprMatrix(0.0), opndCurPos,
                                             iToken, sbExpr.ToString.Substring(iRe, iRe2 - iRe))) '  variable
                                    iRe = iRe2
                                End If
                            End If
                        ElseIf c = 91 OrElse c = 40 OrElse c = 123 Then '       LP

                            If bValidate AndAlso
                            Not Validate(tknGnralType.LPsqrDx, "(") Then
                                If bInsChar Then
                                    LP -= 1
                                    GoTo retry
                                End If
                                Exit Do
                            End If
                            rpn1.Add(New Token(tokenType84.other,
                                                   -40, New ExprMatrix(0.0), iRe, iToken, "(")) ' "("

                            Dim cP As Int32 = iRe
                            iRe += 1
                            nextExpr()


                            If IsFunction Then

                                ' NOTE 2.

                                ' bHasFn=true indicates that has arrived here to
                                ' get the argument of a function (for instance cos(0.2+0.1) )
                                ' so execution should return back to the caller (see NOTE 1 above)

                                Exit Try
                            ElseIf iRe >= sbExpr.Length Then
                                Exit Do
                            End If
                        ElseIf c = 93 OrElse c = 41 OrElse c = 125 Then '       RP
                            If bValidate AndAlso
                            Not Validate(tknGnralType.RP, ")") Then
                                If bInsChar Then
                                    GoTo retry
                                End If
                                Exit Do
                            End If

                            rpn1.Add(New Token(tokenType84.other,
                                                  -41, New ExprMatrix(0.0), iRe, iToken, ")")) ' ")"
                            iRe += 1
                            curOptor = -1
                            Exit Try
                        ElseIf c = 960 Then

                            If bValidate AndAlso
                            Not Validate(tknGnralType.Cnt, "pi") Then '                      (PI)
                                If bInsChar Then
                                    GoTo retry
                                End If
                                Exit Do
                            End If
                            opndCurPos = iRe
                            'sbExpr.Remove(iRe, 1)
                            Dim ln1 As Int32 = sbExpr.Length
                            Dim sPi As String = Math.PI.ToString(us)
                            'sbExpr.Insert(iRe, sPi)
                            Dim ln2 As Int32 = sbExpr.Length
                            rpn1.Add(New Token(
                                     tokenType84.oprnd,
                                     0, New ExprMatrix(Math.PI), iRe, iToken, sPi)) ' П͏͏constant
                            iRe += 1

                        ElseIf c = 38 Then ' 38="&"
                            iRe2 = iRe + 1
                            Do While iRe2 < sbExpr.Length AndAlso
                            sbExpr.ToString.Chars(iRe2) = " "
                                iRe2 += 1
                            Loop
                            Select Case LCase(sbExpr.ToString.Substring(iRe2, 1))
                                Case "h" : curBase = numBase.hexadecimal
                                Case "d" : curBase = numBase.decimal
                                Case "o" : curBase = numBase.octal
                                Case "b" : curBase = numBase.binary
                                Case Else
                                    err = New msg884(Me, 6, New String() {sbExpr.ToString.Substring(iRe, iRe2 - iRe + 1)})
                                    Exit Do
                            End Select
                            iRe = iRe2 + 1
                        ElseIf c = 39 Then ' 39="'"
                            Exit Do ' end of tokens
                        ElseIf c = 44 Then ' 44=","
                            Do
                                If bValidate AndAlso
                                Not Validate(tknGnralType.coma, ")") Then
                                    If bInsChar Then
                                        GoTo retry
                                    End If
                                    Exit Do
                                End If
                                IsFunction = True
                                'rpn1.Add(New Token(tokenType84.matrix,
                                '0, 0.0, iRe, iToken, ",")) ' ")"
                                iRe += 1
                                curOptor = -1
                                nextExpr()
                                IsFunction = False
                            Loop While iRe < sbExpr.Length AndAlso
                                       sbExpr.Chars(iRe) = ","
                            Exit Try
                        ElseIf c = 95 Then ' "_"
                            Dim m As Match = reVar2.Match(sbExpr.ToString, iRe)
                            opndCurPos = iRe '                             V A R I A B L E ("_" start)

                            If bValidate AndAlso
                            Not Validate(tknGnralType.variable, m.ToString) Then
                                If bInsChar Then
                                    GoTo retry
                                End If
                                Exit Do
                            End If
                            rpn1.Add(New Token(
                                     tokenType84.var, -1, New ExprMatrix(0.0), opndCurPos,
                                     iToken, m.ToString)) '  variable
                            iRe += m.Length
                        ElseIf c = 58 OrElse c = 247 Then '  ":"=58, "÷"=247 OPERATORS
                            sbExpr.Chars(iRe) = "/" ' convert ":" or "÷" into "/"
                            GoTo retry
                        ElseIf c = 151 OrElse c = 8211 Then ' "–"
                            sbExpr.Chars(iRe) = "-"
                            GoTo retry
                        Else
                            Dim pos As Int32 = Array.IndexOf(New Int32() {
                           151, 8211, 58, 247, 188, 189, 190,
                                &H2212, &HD7, &H3C0, 12310, 12311, 8542, 6,
                                178, 179,
                                8313, 8312, 8311, 8310, 8309, 8308,
                                8722
                                }, c)
                            If pos >= 0 Then
                                Dim vSubstituteBy() As String = {
                                "-", "-", "/", "/", "(1/4)", "(1/2)", "(3/4)",
                                 "-", "x", "PI", "(", ")", "(7/8)", "-",
                                 "^2", "^3",
                                 "^9", "^8", "^7", "^6", "^5", "^4",
                                "-"
                                                    }
                                sbExpr.Remove(iRe, 1)
                                sbExpr.Insert(iRe, vSubstituteBy(pos))
                                GoTo retry
                            Else
                                Try
                                    Dim vsErr() As String = {Chr(c)}
                                    err = New msg884(Me, 6, vsErr)
                                Catch ex As Exception
                                    err = New msg884(Me, 0)
                                End Try
                                Exit Do
                            End If

                        End If

                    End If

                    bNotUnary = True
                End If
            Loop While iRe < sbExpr.Length
            If bValidate AndAlso err Is Nothing Then
                Validate(tknGnralType.EOTokens, Chr(c))
            End If
            curOptor = -4 ' End Of Tokens

        Catch ex As Exception
            err = ex
        End Try
    End Sub
    Friend Function EvalFn(sFn As String, arg As Double, Optional args() As Expression = Nothing) As Double
        Dim Result As Double
        Dim bOk As Boolean = True
        Try
            Select Case sFn.Length
                Case 6
                    Select Case LCase(sFn)
                        Case "logten" : Result = Math.Log10(arg)
                        Case "logtwo" : Result = Math.Log(arg) / Math.Log(2.0)
                        Case Else : bOk = False
                    End Select
                Case 5
                    Select Case LCase(sFn)
                        Case "acosh" : Result = Math.Log(arg + Math.Sqrt(arg * arg - 1))
                        Case "acoth" : Result = 0.5 * Math.Log((1 + arg) / (arg - 1))
                        Case "acsch"
                            If arg < 0 Then
                                Result = Math.Log((1 - Math.Sqrt(1 + arg * arg)) / arg)
                            Else
                                Result = Math.Log((1 + Math.Sqrt(1 + arg * arg)) / arg)
                            End If
                        Case "asech"
                            If arg < -1 Then
                                Result = Math.Log((1 - Math.Sqrt(1 - arg * arg)) / arg)
                            ElseIf arg > 0 Then
                                Result = Math.Log((1 + Math.Sqrt(1 - arg * arg)) / arg)
                            End If
                        Case "asinh" : Result = arg

                        Case "atanh" : Result = 0.5 * Math.Log((1 + arg) / (1 - arg))
                        Case "floor" : Result = Math.Floor(arg)
                            'Case "roots"
                        Case "round" : Result = Math.Round(arg)
                        Case Else : bOk = False
                    End Select
                Case 4
                    Select Case LCase(sFn)
                        Case "acos" : Result = Math.Acos(arg)
                        Case "acot" : Result = Math.Atan(1.0 / arg)
                        Case "acsc" : Result = Math.Asin(1.0 / arg)
                            'Case "asec"
                        Case "asin" : Result = Math.Asin(arg)
                        Case "atan" : Result = Math.Atan(arg)
                            'Case "atan2"
                            'Case "conj"
                            'Case "cosh"
                            'Case "coth"
                            'Case "csch"
                            'Case "norm"
                            'Case "sech"
                        Case "sign" : Result = Math.Sign(arg)
                            'Case "sinh"
                        Case "sqrt" : Result = Math.Sqrt(arg)
                            'Case "tanh"
                        Case Else : bOk = False
                    End Select
                Case 3
                    Select Case LCase(sFn)
                        Case "abs" : Result = Math.Abs(arg)
                        Case "arg" : Result = arg
                        Case "cos" : Result = Math.Cos(arg)
                        Case "cot" : Result = Math.Cos(arg) / Math.Sin(arg)
                        Case "csc" : Result = 1.0 / Math.Sin(arg)
                        Case "exp" : Result = Math.Exp(arg)
                        Case "gcd"
                            Dim eMArgs As New ExprMatrix(New Config, 1, 2)
                            eMArgs.getExpr(0, 0) = New Expression(args(0))
                            eMArgs.getExpr(0, 1) = New Expression(args(1))
                            Dim eM As ExprMatrix = exprParser.evalFn(sFn, eMArgs)
                            Result = eM.getCurExpr.toDouble
                        Case "lcm"
                            Dim eMArgs As New ExprMatrix(New Config, 1, 2)
                            eMArgs.getExpr(0, 0) = New Expression(args(0))
                            eMArgs.getExpr(0, 1) = New Expression(args(1))
                            Dim eM As ExprMatrix = exprParser.evalFn(sFn, eMArgs)
                            Result = eM.getCurExpr.toDouble
                        Case "log" : Result = Math.Log(arg)
                            'Case "mod"
                        Case "sec" : Result = 1.0 / Math.Cos(arg)
                        Case "sin" : Result = Math.Sin(arg)
                        Case "sqr" : Result = Math.Sqrt(arg)
                        Case "tan" : Result = Math.Tan(arg)
                        Case "neg" : Result = -arg
                        Case Else : bOk = False
                    End Select
                Case 2
                    Select Case LCase(sFn)
                        'Case "im"
                        Case "ln" : Result = Math.Log(arg)
                            'Case "re"
                        Case Else : bOk = False
                    End Select
                Case 1
                    If sFn = "√" Then
                        Result = Math.Sqrt(arg)
                    End If
            End Select
        Catch ex As Exception
            err = ex
        End Try
        If bOk = False Then
            err = New msg884(Me, 6, New String() {sFn})
        End If
        Return Result
    End Function
    Public ReadOnly Property retErr As msg884
        Get
            If err Is Nothing OrElse
            err.Message Is Nothing Then
                Return Nothing
            End If
            If TypeOf (err) Is msg884 Then
                Return err
            End If
            Dim m8 As New msg884(Me, err.Message)
            Return m8
        End Get
    End Property
    Private Function Validate(
                    curGType As tknGnralType,
                    sCur As String) As Boolean
        If iRe >= sbExpr.Length Then
            If iToken = 0 Then Return True
            curGType = 20
        End If
        If iToken = 0 Then
            sLastTkn = sCur
            oldType = curGType
            Return validateFirstToken(curGType, sCur)
        End If
        If oldType = tknGnralType.EOTokens Then
            Return True
        End If
        Dim vsErr() As String = {sLastTkn, sCur}
        Try


            '    1           2        3   21       4         5         6         7         8     
            'LP, √, Dx       RP     num, cnt     fn       col       row        =       variable 

            '    9           10        11        12      13 mtxOp    14         15        16     
            '-,+,*,/,%,^,!  modulo  integralRe integral  -,+,*,/,^  and,or,xor  not     binary    

            '   16           16        16         17        17        17        20        25        50
            ' octal       decimal  hexadecimal  radians   degrees   gradians  EOTokens  subtitute  unknown

            If oldType = 16 OrElse oldType = 17 Then
                Exit Try
            ElseIf oldType = 25 Then
                ' TODO
                Exit Try
            ElseIf oldType = 50 Then
                ReDim Preserve vsErr(0)
                err = New msg884(Me, 6, vsErr) ' unknown token
                GoTo notValid
            End If
            bInsChar = False
            Select Case curGType
                Case 1 ' (, √, Dx
                    LP += 1
                    Select Case oldType
                        Case 2, 3, 8, 11, 21 ' ), num, cnt, var, dx
                            ' *  ^
                            ' 42 94
                            sbExpr.Insert(iRe, "*")
                        Case Else
                            Exit Try
                    End Select
                Case 2 ' )
                    RP += 1
                    If RP > LP Then
                        err = New msg884(Me, 9) ' missing (
                    End If
                    Select Case oldType
                        Case 2, 3, 6, 8, 11, 21
                            Exit Try
                        Case Else
                            err = New msg884(Me, 2, vsErr) ' token sequence is not valid
                            GoTo notValid
                    End Select
                Case 3, 21 ' num, cnt
                    Select Case oldType
                        Case 2, 8 ' old= {")","var"}  cur={num,constant}
                            sbExpr.Insert(iRe, "^")
                        Case 3, 21 ' old= {num,constant}  cur={num,const}
                            sbExpr.Insert(iRe, "*")
                        Case 9
                            If sLastTkn = "!" Then
                                sbExpr.Insert(iRe, "*")
                            Else
                                Exit Try
                            End If
                        Case Else
                            Exit Try
                    End Select
                Case 4 ' fn
                    Select Case oldType
                        Case 2, 3, 8, 21
                            ' *  ^
                            ' 42 94
                            sbExpr.Insert(iRe, "*")
                        Case 4
                            err = New msg884(Me, 2, vsErr) ' token sequence is not valid
                            GoTo notValid
                        Case Else
                            Exit Try
                    End Select
                Case 5 ' col
                    Select Case oldType
                        Case 2, 3, 8, 11, 13, 21 : Exit Try
                        Case Else
                            err = New msg884(Me, 2, vsErr) ' token sequence is not valid
                            GoTo notValid
                    End Select
                Case 6 ' row
                    Select Case oldType
                        Case 1, 2, 3, 8, 11, 13, 21 : Exit Try
                        Case Else
                            err = New msg884(Me, 2, vsErr) ' token sequence is not valid
                            GoTo notValid
                    End Select
                Case 7 ' =
                    Select Case oldType
                        Case 2, 3, 8, 11, 21 : Exit Try
                        Case Else
                            err = New msg884(Me, 2, vsErr) ' token sequence is not valid
                            GoTo notValid
                    End Select
                Case 8 ' var
                    Select Case oldType
                        Case 9 ' optor
                            If sLastTkn = "!" Then
                                sbExpr.Insert(iRe, "*")
                            End If
                        Case tknGnralType.RP, 21
                            sbExpr.Insert(iRe, "*")
                        Case 4
                            err = New msg884(Me, 2, vsErr) ' token sequence is not valid
                            GoTo notValid
                        Case 3
                            ' *  ^
                            ' 42 94
                            sbExpr.Insert(iRe, "*")
                        Case 11
                            ' *  ^
                            ' 42 94
                            sbExpr.Insert(iRe, "*")
                        Case Else : Exit Try
                    End Select
                Case 9 ' -,+,*,/,^,!
                    Select Case oldType
                        Case 1, 2, 3, 4, 8, 11, 21 : Exit Try
                        Case 10, 13
                            err = New msg884(Me, 2, vsErr) ' token sequence is not valid
                            GoTo notValid
                        Case 9
                            If sCur = "-" AndAlso
                            InStr("*/^", sLastTkn) Then
                                Exit Try
                            ElseIf sLastTkn <> "!" Then
                                err = New msg884(Me, 2, vsErr) ' token sequence is not valid
                                GoTo notValid
                            End If
                            Exit Try
                        Case Else
                            If sLastTkn <> "-" AndAlso sLastTkn <> "+" Then
                                err = New msg884(Me, 2, vsErr) ' token sequence is not valid
                                GoTo notValid
                            End If
                            Exit Try
                    End Select
                Case 10 ' mod
                    Select Case oldType
                        Case 2, 3, 8, 11, 21 : Exit Try
                        Case Else
                            err = New msg884(Me, 2, vsErr) ' token sequence is not valid
                            GoTo notValid
                    End Select
                Case 11 ' dx
                    Select Case oldType
                        Case 2, 3, 8, 11, 12, 21 : Exit Try
                        Case Else
                            err = New msg884(Me, 2, vsErr) ' token sequence is not valid
                            GoTo notValid
                    End Select
                Case 12 ' ∫ integral
                    Select Case oldType
                        Case 4, 10
                            err = New msg884(Me, 2, vsErr) ' token sequence is not valid
                            GoTo notValid
                        Case 2, 8, 11
                            ' *  ^
                            ' 42 94
                            sbExpr.Insert(iRe, "*")
                        Case Else : Exit Try
                    End Select
                Case 13 ' matrix operator (-,+,*,/,^)
                    Select Case oldType
                        Case 1, 2, 3, 6, 8, 11, 21 : Exit Try
                        Case 9
                            If sLastTkn <> "!" Then
                                err = New msg884(Me, 2, vsErr) ' token sequence is not valid
                                GoTo notValid
                            End If
                        Case 13
                            err = New msg884(Me, 2, vsErr) ' token sequence is not valid
                            GoTo notValid
                        Case Else
                            err = New msg884(Me, 2, vsErr) ' token sequence is not valid
                            GoTo notValid
                    End Select
                Case 14 ' tknGnralType.andOrXorNandNor 
                    If sCur = "not" Then
                        Select Case oldType
                            Case 4
                                err = New msg884(Me, 2, vsErr) ' token sequence is not valid
                                GoTo notValid
                            Case 3, 21, 8
                                sbExpr.Insert(iRe, "*")
                                Return True

                            Case Else : Exit Try

                        End Select
                    Else
                        Select Case oldType
                            Case 2, 3, 8, 11 : Exit Try
                            Case Else
                                err = New msg884(Me, 2, vsErr) ' token sequence is not valid
                                GoTo notValid
                        End Select
                    End If

                Case 15 ' NOT
                    Select Case oldType
                        Case 4
                            err = New msg884(Me, 2, vsErr) ' token sequence is not valid
                            GoTo notValid
                        Case 3, 8, 21
                            ' *  ^
                            ' 42 94
                            sbExpr.Insert(iRe, "*")
                            Return True
                        Case Else : Exit Try
                    End Select
                Case 20 ' EOTokens
                    Select Case oldType
                        Case 2, 3, 6, 8, 11, 13, 21 : Exit Try
                        Case 9, 13
                            If sCur <> "!" Then
                                ReDim Preserve vsErr(0)
                                err = New msg884(Me, 3, vsErr) ' ending token is not valid
                                GoTo notValid
                            Else
                                Exit Try
                            End If
                        Case Else
                            ReDim Preserve vsErr(0)
                            err = New msg884(Me, 3, vsErr) ' ending token is not valid
                            GoTo notValid
                    End Select
                Case tknGnralType.coma '
                    Select Case sLastTkn
                        Case "("
                            err = New msg884(Me, 2, vsErr) ' token sequence is not valid
                            GoTo notValid
                        Case Else
                            If oldType = tknGnralType.fn Then
                                err = New msg884(Me, 2, vsErr) ' token sequence is not valid
                                GoTo notValid
                            End If
                            Exit Try
                    End Select
                Case 50 ' unknown token
                    ReDim Preserve vsErr(0)
                    err = New msg884(Me, 6, vsErr) ' unknown token
                    GoTo notValid
            End Select
            If err Is Nothing Then
                sLastTkn = sbExpr.Chars(iRe)
                oldType = curGType
                bInsChar = True
                Return False
            End If
        Catch ex As Exception
            err = ex
        End Try
        sLastTkn = sCur
        oldType = curGType
        Return True
notValid:
        iRe = sbExpr.Length
        Return False
    End Function
    Private Function validateFirstToken(curGType As tknGnralType, sCur As String) As Boolean
        Try

            '    1           2       3    21     4         5         6         7         8     
            'LP, √, Dx       RP     num, cnt     fn       col       row        =       variable 

            '    9           10        11        12      13 mtxOp    14         15        16     
            '-,+,*,/,%,^,!  modulo  integralRe integral  -,+,*,/,^  and,or,xor  not     binary    

            '   16           16        16         17        17        17        20        25        50
            ' octal       decimal  hexadecimal  radians   degrees   gradians  EOTokens  subtitute  unknown

            Dim vsErr() As String = {sCur}
            If iRe + Len(sCur) >= sbExpr.Length Then
                ' Is first and last token
                Select Case curGType
                    Case 3, 8, 16, 21 ' num, cnt, var.
                        Return True
                    Case Else
                        err = New msg884(Me, 4, vsErr) ' err in start token
                        Return False
                End Select
            Else
                Select Case curGType
                    Case 1, 3, 4, 8, 12, 15, 16, 17, 21, 25
                        If sCur = "(" Then LP += 1
                        Return True
                    Case 9, 13
                        If sCur = "-" OrElse sCur = "+" Then
                            Return True
                        Else
                            err = New msg884(Me, 4, vsErr) ' err in start token
                            Return False
                        End If
                    Case Else
                        err = New msg884(Me, 4, vsErr) ' err in start token
                        Return False
                End Select
            End If
        Catch ex As Exception
            err = ex
        End Try
        Return False
    End Function
    Private Function tryParseHexDecOctBinBaseNum(str As String, ByRef result As Decimal) As Boolean
        Dim Ent As Long = 0
        Dim Fra As Double = 0.0
        Dim sPattern As String = ""
        Dim base As Int64
        Dim ns As Globalization.NumberStyles
        Try
            Select Case curBase
                Case numBase.hexadecimal
                    sPattern = "[.0-9abcdef]"
                    base = 16
                    ns = Globalization.NumberStyles.HexNumber
                    'Case numbase.decimal
                    '    sPattern = "[.0-9]"
                    '    base = 10
                    '    ns = Globalization.NumberStyles.Integer
                Case numBase.octal
                    sPattern = "[.0-7]"
                    base = 8
                Case numBase.binary
                    sPattern = "[.01]"
                    base = 2
            End Select
            Dim i As Int32 = iRe
            Dim posDot As Int32 = InStr(str, ".")
            If posDot < str.Length AndAlso
            InStr(posDot + 1, str, ".") Then
                err = New msg884(Me, 6, New String() _
                    {sbExpr.ToString.Substring(iRe)})
                Exit Try
            End If
            If posDot = 0 Then
                posDot = Len(str)
            Else
                posDot -= 1
            End If
            Dim curEnt As Long = 0L
            For j As Int32 = 0 To Len(str) - 1
                If j <> posDot Then
                    If Not Long.TryParse(str.Chars(j), curEnt) Then
                        ' si llega aqui es porque str.chars(j) = {a,b,..,f,A,B,... ,F}
                        ' asc("A")=65 ==> asc("A")-55=10, asc("B")-55=11,... asc("F")-55=15
                        curEnt = Asc(UCase(str.Chars(j))) - 55
                    End If
                End If
                If j < posDot Then
                    Ent = Ent * base + curEnt
                ElseIf j > posDot Then
                    Fra = Fra + curEnt / base ^ (j - posDot)
                End If
            Next
            i += 1
            result = Ent + Fra
        Catch ex As Exception
            err = New msg884(Me, ex.Message)
            Return False
        End Try
        Return True
    End Function
    Public ReadOnly Property currentBase As numBase
        Get
            Return Me.curBase
        End Get
    End Property
    Friend Property err As Exception
        Get
            Return err1
        End Get
        Set(value As Exception)
            If err1 Is Nothing AndAlso
            err1 IsNot value Then
                err1 = New msg884(Me, value.Message)
                curOptor = -4
            End If
        End Set
    End Property
End Class
Friend Enum optor
    ' -  +  *  /  ^  %  !  .  :
    '45 43 42 47 94 37 33 46 58
    [and] = 1
    [or]
    [not]
    [xor]
    [nor]
    [nand]
    substract = 45
    add = 43
    multiply = 42
    divide = 47
    power = 94
    modulo = 37
    factorial = 33

End Enum
Friend Enum tknGnralType
    '    1           2         3         4         5         6         7         8     
    'LP, √, Dx       RP     num, cnt     fn       col       row        =       variable 

    '    9           10        11        12      13 mtxOp    14        15        16     
    '-,+,*,/,%,^,!  modulo  integralRe integral  -,+,*,/,^  and,or,xor  not     binary    

    '   16           16        16         17        17        17        20        25        50
    ' octal       decimal  hexadecimal  radians   degrees   gradians  EOTokens  subtitute  unknown
    LPsqrDx = 1
    RP = 2
    num = 3
    fn = 4
    col = 5
    row = 6
    equal = 7
    variable = 8
    optor = 9
    modulo = 10
    integrRespTo = 11
    integral = 12
    mtxOptor = 13
    andOrXorNandNor = 14
    [not] = 15
    numericBase = 16
    angleBase = 17
    EOTokens = 20
    Cnt = 21
    substitute = 25
    coma = 30
    Unknown = 50
End Enum
Public Enum numBase
    hexadecimal
    [decimal]
    octal
    binary
End Enum

