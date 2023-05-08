Imports System.Text.RegularExpressions
Imports System.Text

<Serializable()>
Public Class exprParser84
    Public Sub New()
        Me.cfg = New Config84
    End Sub

    Public Shared us As System.Globalization.CultureInfo = Config84.us
    Friend iRe As Int64
    Private iToken, lenToken As Int64
    Private opndCurPos As Int64
    Private optoriTkn, chgSgniTkn As Int64
    Private optorPos, chgSgnPos As Int64
    Private LP, RP As Int64
    Private IsFunction As Boolean = False
    Private nOpnd, curOpnd As Int64, curOptor As optor
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



    Public Function ToStringStack(ByRef sResult As String, cfg As Config, Optional vNames() As String = Nothing,
                          Optional vValues() As ExprMatrix = Nothing,
                          Optional bGroup As Boolean = False,
                          Optional bShowVarValues As Boolean = True) As Boolean
        Return rpn1.ToStringStack(sResult, cfg, vNames, vValues, bGroup, bShowVarValues)
    End Function
    Public Function ToStringDetail(ByRef result As Token,
                                   ByRef sResult As String,
                        Optional vNames() As String = Nothing,
                        Optional vValues() As ExprMatrix = Nothing,
                        Optional format As outputMessage = outputMessage.plainText,
                        Optional vTkn() As Token = Nothing) As Boolean
        If rpn1.eP Is Nothing Then
            rpn1.eP = Me
        End If
        Return rpn1.ToStringDetail(result, sResult, vNames, vValues, format, vTkn)
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
                            sbExpr.Insert(CInt(iRe), "*")
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
                            sbExpr.Insert(CInt(iRe), "^")
                        Case 3, 21 ' old= {num,constant}  cur={num,const}
                            sbExpr.Insert(CInt(iRe), "*")
                        Case 9
                            If sLastTkn = "!" Then
                                sbExpr.Insert(CInt(iRe), "*")
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
                            sbExpr.Insert(CInt(iRe), "*")
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
                                sbExpr.Insert(CInt(iRe), "*")
                            End If
                        Case tknGnralType.RP, 21
                            sbExpr.Insert(CInt(iRe), "*")
                        Case 4
                            err = New msg884(Me, 2, vsErr) ' token sequence is not valid
                            GoTo notValid
                        Case 3
                            ' *  ^
                            ' 42 94
                            sbExpr.Insert(CInt(iRe), "*")
                        Case 11
                            ' *  ^
                            ' 42 94
                            sbExpr.Insert(CInt(iRe), "*")
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
                            sbExpr.Insert(CInt(iRe), "*")
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
                                sbExpr.Insert(CInt(iRe), "*")
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
                            sbExpr.Insert(CInt(iRe), "*")
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
            Dim i As Int64 = iRe
            Dim posDot As Int64 = InStr(str, ".")
            If posDot < str.Length AndAlso
            InStr(CInt(posDot + 1), str, ".") Then
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
            For j As Int64 = 0 To Len(str) - 1
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

