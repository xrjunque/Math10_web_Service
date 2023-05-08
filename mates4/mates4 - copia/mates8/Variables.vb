Imports System.Text
Imports System.Text.RegularExpressions

<Serializable()> _
Public Class VarsAndFns
    Dim IDG As Int32 = 0
    ' multiCharNames are variables names not present in sVar 
    ' or sVar2 patterns in MathGlobal8 class:
    Dim nomVars2Append(-1) As String
    Dim names(-1) As String
    Dim fns(-1) As customFn
    Dim exprMtx(-1) As ExprMatrix
    Dim bCaseSensitive As Boolean
    Dim bHasValue As Boolean = True
    Public bVerifyName As Boolean = True
    Public cfg As Config
    Public cur(-1) As currentMatch ' value's currentMatchObject
    Public Sub New(ByRef cfg As Config)
        bCaseSensitive = cfg.bCaseSensitive
        Me.cfg = cfg
    End Sub
    Public Sub Clear()
        ReDim nomVars2Append(-1), names(-1), fns(-1), exprMtx(-1), cur(-1)
        bCaseSensitive = False
        bHasValue = True
        IDG = 0
    End Sub
    Public Sub New(ByVal vars As VarsAndFns)
        With vars
            Me.IDG = .IDG
            ReDim Me.nomVars2Append(.nomVars2Append.Length - 1), _
                Me.names(.names.Length - 1), _
                Me.fns(.fns.Length - 1), _
                Me.exprMtx(.exprMtx.Length - 1)
            If Me.nomVars2Append.Length Then
                Array.Copy(.nomVars2Append, Me.nomVars2Append, .nomVars2Append.Length)
            End If
            If Me.names.Length Then
                Array.Copy(.names, Me.names, .names.Length)
            End If
            If Me.fns.Length Then
                Array.Copy(.fns, Me.fns, .fns.Length)
            End If
            If Me.exprMtx.Length Then
                Array.Copy(.exprMtx, Me.exprMtx, .exprMtx.Length)
            End If
            Me.bCaseSensitive = .bCaseSensitive
            Me.bHasValue = .bHasValue
            If vars.cfg IsNot Nothing Then
                Me.cfg = vars.cfg
            End If
        End With
    End Sub
    Public Shared Function tryParseVariables( _
                ByVal sVars As String, _
                ByRef oVars As VarsAndFns, ByRef msg As String, _
                Optional maxLen As Int64 = -1) As Boolean
        Dim bRet As Boolean = False
        Dim bDoGCD As Boolean = Polynomial.bDoGCD
        Try
            Polynomial.bDoGCD = False
            Dim cfg As Config = oVars.cfg
            If oVars Is Nothing Then
                oVars = New VarsAndFns(cfg)
            End If
            Dim i As Int64
            If cfg.bIgnoreSpaces Then
                sVars = Regex.Replace(sVars, "[ \t]+", "")
            End If
            sVars = Regex.Replace(sVars,
                        MathGlobal8.sComment, "")
            sVars = Replace(sVars, vbCrLf, vbLf)
            sVars = Replace(sVars, vbLf, "|")
            sVars = Replace(sVars, vbCr, "|")
            sVars = Replace(sVars, "@", "|")
            sVars = Replace(sVars, "|", vbCrLf)

            ' 1) Add all variables to oVars:
            Dim e1() As String = Split(sVars, vbCrLf)
            Dim e1b(-1) As String, ie As Int64 = -1
            For i = 0 To e1.Length - 1
                Dim e3 As String = Trim(e1(i))
                If Len(e3) Then
                    If InStr(e3, "=") Then
                        ie += 1
                        ReDim Preserve e1b(ie)
                    Else
                        ' definition extends
                        ' across more than 1 line
                        If ie = -1 Then
                            msg = String.Format(msg8.num(52), e3)
                            Throw New Exception(msg)
                        End If
                    End If
                    e1b(ie) += e3 + vbCrLf
                End If
            Next
            For i = 0 To e1b.Length - 1
                If Trim(e1b(i)).Length Then
                    ' supress leading and trailing white spaces:
                    e1b(i) = Regex.Replace(e1b(i), "(^\s+)|((" +
                             MathGlobal8.sCol + "|" + MathGlobal8.sRow + "|\s+)$)", "")
                    Dim e2() As String = Split(e1b(i), "=")
                    If e2.Length > 2 Then
                        For i1 As Int64 = 2 To e2.Length - 1
                            e2(1) += "=" + e2(i1)
                        Next
                        ReDim Preserve e2(1)
                    ElseIf e2.Length < 2 Then
                        msg = String.Format(
                         msg8.num(18), e1b(i))
                        Exit Try
                    End If
                    ' supress leading and trailing white spaces:
                    e2(1) = Regex.Replace(e2(1), "(^\s+)|((" +
                             MathGlobal8.sCol + "|" + MathGlobal8.sRow + "|\s+)$)", "")
                    Dim bHasLP As Boolean = False
                    If Len(e2(0)) > 1 AndAlso e2(0).Chars(0) = "-" Then
                        e2(0) = Mid(e2(0), 2)
                        e2(1) = "-(" + e2(1) + ")"
                    End If
                    Dim name As String = Trim(e2(0))
                    Dim oVars2 As New VarsAndFns(oVars) ' (cfg)
                    Dim value As New matrixParser()
                    value.cfg = cfg
                    value.bCaseS = cfg.bCaseSensitive
                    e2(1) = Trim(e2(1))
                    If Len(e2(1)) = 0 Then
                        msg = String.Format(
                           msg8.num(52), e1b(i))
                        Exit Try
                    End If
                    'value.parse(Trim(e2(1)), "", oVars2)

                    value.parse(Replace(Trim(e2(1)), vbCrLf, "|"), "", oVars2)
                    If value.errMsg.Length Then
                        msg = value.errMsg
                        Return False
                    End If
                    If Regex.IsMatch(name, MathGlobal8.sLP) Then
                        bHasLP = True
                        Dim fn As customFn = Nothing
                        If customFn.tryParseCustomFn(
                        cfg, e1b(i), fn, oVars2, msg) Then
                            fn.exprMtx = value.ret.exprMtx
                            If oVars.AddVar(fn.name, fn.exprMtx, msg) Then
                                ReDim Preserve oVars.fns(oVars.getNamesList.Length - 1)
                                ReDim Preserve oVars.cur(oVars.getNamesList.Length - 1)
                                oVars.fns(oVars.fns.Length - 1) = fn
                                oVars.cur(oVars.cur.Length - 1) = fn.cur
                                ' fn added, goto next:
                                GoTo sig
                            End If
                            Exit Try
                        End If
                    End If

                    Dim eMtx As ExprMatrix = value.ret.exprMtx
                    Dim mpName As New matrixParser()
                    mpName.cfg = cfg
                    mpName.cfg.bDetail = False
                    mpName.bCaseS = cfg.bCaseSensitive
                    Dim oVarsLeftSide As New VarsAndFns(cfg)
                    If Not Regex.IsMatch(name, MathGlobal8.sLP) Then
                        mpName.parse(name, "", oVarsLeftSide)
                    Else
                        bHasLP = True
                    End If
                    Dim bAdded As Boolean = False
                    If mpName.getParser Is Nothing Then
                        mpName.parser = New exprParser(mpName, "", "", cfg)
                    End If
                    If Not bHasLP AndAlso mpName.getParser.cur.iCur > 1 Then
                        Dim cur As currentMatch =
                            mpName.getParser.cur
                        cur.reStartCurMatch()
                        Dim nVars As Int64 = 0
                        Dim bTryAdd As Boolean = True
                        Do While Not cur.bEnd
                            If cur.subT1 = currentMatch.exprSubType1.op Then
                                If cur.subT2 = currentMatch.exprSubType2.mult Then
                                Else
                                    bTryAdd = False
                                    Exit Do
                                End If
                            ElseIf cur.tipo = currentMatch.exprType.var Then
                                If nVars Then
                                    bTryAdd = False
                                    If name <> cur.str Then Exit Do
                                Else
                                    name = cur.str
                                    nVars += 1
                                    bTryAdd = True
                                End If
                            ElseIf cur.tipo = currentMatch.exprType.num Then
                                Dim db As Double
                                MathGlobal8.TryParseDbl(cur.str, db)
                                eMtx /= New ExprMatrix(db)
                            End If
                            cur.doNext()
                        Loop
                        If bTryAdd Then
                            bAdded = oVars.AddVar(name, eMtx, msg)
                        End If
                    Else
                        bAdded = oVars.AddVar(name, eMtx, msg)
                    End If
                    If bAdded Then
                        Dim varID As Int64 =
                            oVars.getVarIDByName(name)
                        If varID >= oVars.cur.Length Then
                            ReDim Preserve oVars.cur(varID)
                            If value.getParser.cur IsNot Nothing Then
                                oVars.cur(varID) =
                                   value.getParser.cur.Clone
                            End If
                        End If
                    Else
                        cfg.oDetail.Clear()
                    End If
                End If

                'End If
sig:
            Next


            Dim vVarAll() As String = oVars.getNamesList
            For i = 0 To oVars.exprMtx.Length - 1
                If oVars.exprMtx(i) Is Nothing Then
                ElseIf i < oVars.fns.Length AndAlso oVars.fns(i) IsNot Nothing Then
                    ' is a custom function
                ElseIf oVars.exprMtx(i).Cols + oVars.exprMtx(i).Rows > 2 Then
                    '  expression matrix
                Else
                    ' replace referenced variables by its values,
                    ' for example if x=0.7 and y=x-0.5 then, here 
                    ' y will become y=0.2:
                    Dim cfg1 As New Config(cfg)
                    cfg1.bRounding = False
                    If True Then
                        Dim curExpr As Expression = oVars.exprMtx(i).getExpr(0, 0)
                        Dim oVars2 As New VarsAndFns(cfg1)
                        For j = 0 To i - 1
                            If oVars.exprMtx(j) Is Nothing Then
                            ElseIf i < oVars.fns.Length AndAlso oVars.fns(j) IsNot Nothing Then
                            ElseIf oVars.exprMtx(j).Cols + oVars.exprMtx(j).Rows > 2 Then
                            Else
                                oVars2.AddVar(vVarAll(j), oVars.exprMtx(j).getExpr(0, 0))
                            End If
                        Next
                        curExpr = curExpr.evalExprToExpr(oVars2)
                        Dim vCurAll(-1) As String
                        Dim vCurAll2(-1) As String
                        curExpr.getAllVars(vCurAll2)
                        Do
                            vCurAll = vCurAll2
                            curExpr = curExpr.evalExprToExpr(oVars2)
                            curExpr.getAllVars(vCurAll2)
                        Loop While vCurAll2.Length <> vCurAll.Length
                        vCurAll = vCurAll2
                        oVars.exprMtx(i).getExpr(0, 0) = curExpr
                    End If
                    If True Then
                        Dim curExpr As Expression = oVars.exprMtx(i).getExpr(0, 0)
                        Dim oVars2 As New VarsAndFns(cfg1)
                        For j = i + 1 To oVars.exprMtx.Length - 1
                            If oVars.exprMtx(j) Is Nothing Then
                            ElseIf i < oVars.fns.Length AndAlso oVars.fns(j) IsNot Nothing Then
                            ElseIf oVars.exprMtx(j).Cols + oVars.exprMtx(j).Rows > 2 Then
                            Else
                                oVars2.AddVar(vVarAll(j), oVars.exprMtx(j).getExpr(0, 0))
                            End If
                        Next
                        curExpr = curExpr.evalExprToExpr(oVars2)
                        Dim vCurAll(-1) As String
                        Dim vCurAll2(-1) As String
                        curExpr.getAllVars(vCurAll2)
                        Do
                            vCurAll = vCurAll2
                            curExpr = curExpr.evalExprToExpr(oVars2)
                            curExpr.getAllVars(vCurAll2)
                        Loop While vCurAll2.Length <> vCurAll.Length
                        vCurAll = vCurAll2
                        oVars.exprMtx(i).getExpr(0, 0) = curExpr
                    End If
                End If
            Next
            bRet = True
        Catch ex As Exception
            msg = ex.Message
        Finally
            Polynomial.bDoGCD = bDoGCD
        End Try
        Return bRet
    End Function
    Public Shared Sub parseVariables(ByVal cfg As Config, _
                                    ByVal sVars As String, _
                                          ByRef vars As VarsAndFns)
        Try
            If vars Is Nothing Then
                vars = New VarsAndFns(cfg)
            Else
                vars.cfg = cfg
            End If
            If sVars = "" Then Exit Sub
            Dim msg As String = ""
            If Not tryParseVariables(sVars, vars, msg) Then
                Throw New Exception(msg)
            End If
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Public Sub AddVars(ByVal vars2 As VarsAndFns)
        Try
            If vars2 Is Nothing Then
                Exit Sub
            End If
            Dim i As Int64
            Dim msg As String = ""
            For i = 0 To vars2.names.Length - 1
                Me.AddVar(vars2.names(i), vars2.exprMtx(i), msg)
            Next
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Function AddVar(ByVal name As String, ByVal expr As Expression) As Boolean
        Dim msg As String = ""
        If expr Is Nothing Then
            Return AddVar(name, Nothing, msg)
        Else
            Return AddVar(name, New ExprMatrix(expr), msg)
        End If
    End Function
    Function AddVar(ByVal name As String, ByVal expr As ExprMatrix, ByRef msg As String) As Boolean
        Try
            If Me.IDG < names.Length Then
                Me.IDG = names.Length
            End If
            name = Trim(name)
            If Len(name) = 0 Then
                msg = _
                String.Format(msg8.num(15), name)
                Return False
            End If
            'If Not bCaseSensitive Then
            '    'name = LCase(name)
            'End If
            Dim nameToValidate As String
            If Microsoft.VisualBasic.Right(name, 1) = "(" Then
                nameToValidate = Mid(name, 1, Len(name) - 1)
            Else
                nameToValidate = name
            End If
            If Len(name) > 1 Then
                cfg.mathGlobal.nomVarsToAppend = nameToValidate
            Else
                cfg.mathGlobal.nomVarsToAppend = ""
            End If
            Dim mc As MatchCollection = _
            Regex.Matches(nameToValidate, Replace( _
                    cfg.mathGlobal.sVar + "|" + cfg.mathGlobal.sVar2, "{}", "")) '  cfg.mathGlobal.sAll2)
            Dim nVars As Int64 = 0
            Dim oldIDG As Int64 = IDG
            Dim lenVars2App As Int64 = nomVars2Append.Length
            If bVerifyName Then
                For i As Int64 = 0 To mc.Count - 1
                    If mc(i).ToString.Length Then
                        If mc(i).Groups("var2").Success OrElse _
                            mc(i).Groups("var").Success Then
                        Else
                            ' name isn't valid
                            msg = _
                            String.Format(msg8.num(15), nameToValidate)
                            Return False
                        End If
                    End If
                    'If Len(name) > 1 AndAlso name.Chars(0) <> "_" Then
                    ' variable name has 2 or more characters and does not
                    ' start by _ 
                    If Regex.IsMatch(name, "\(\d+\)") Then
                        ' is a initial value, not just a variable or a function: don't add to VarsList
                    Else
                        AddNom2VarsListToAppend(name)
                    End If
                    'End If
                    nVars += 1
                    If nVars > 1 Then Exit For
                Next
            Else
                AddNom2VarsListToAppend(name)
                nVars += 1
            End If
            cfg.mathGlobal.nomVarsToAppend = ""
            If nVars <> 1 Then
                IDG = oldIDG
                ReDim Preserve nomVars2Append(lenVars2App - 1)
                msg = _
                String.Format(msg8.num(15), name)
                Return False
            End If
            Dim curID As Int64 = Array.IndexOf(names, name, 0, IDG)
            If curID = -1 Then
                ReDim Preserve names(IDG), Me.exprMtx(IDG), Me.fns(IDG)
                names(IDG) = name
                If expr IsNot Nothing Then
                    Me.exprMtx(IDG) = New ExprMatrix(expr)
                End If
                IDG += 1
                curID = 0
            Else
                ' variable already exists
                If expr IsNot Nothing Then
                    Me.exprMtx(curID) = expr
                End If
            End If
            If curID > -1 AndAlso Me.exprMtx(curID) IsNot Nothing Then
                Dim sVar As String = ""
                sVar = getVarsStrListToAppend()
                If Len(sVar) > 1 Then
                    cfg.mathGlobal.nomVarsToAppend = sVar
                Else
                    cfg.mathGlobal.nomVarsToAppend = ""
                End If
            End If
            If expr IsNot Nothing Then
                Try
                    If expr.IsPolynomial Then
                    ElseIf True Then
                        Dim m8 As New matrixParser
                        m8.cfg = cfg
                        m8.bCaseS = cfg.bCaseSensitive
                        m8.parse(expr.ToStringExprMtx(cfg), "", Nothing, cfg)
                        If m8.errMsg.Length Then
                            msg = m8.errMsg
                            Return False
                        End If
                        If curID >= Me.cur.Length Then
                            ReDim Preserve Me.cur(curID)
                        End If
                        Me.cur(curID) = m8.getParser.cur
                        For i As Int64 = 0 To expr.Rows - 1
                            For j As Int64 = 0 To expr.Cols - 1
                                If m8.ret.exprMtx.getExpr(i, j).IsPolynomial Then
                                    m8.ret.exprMtx.getExpr(i, j) = New Expression( _
                                        m8.ret.exprMtx.getExpr(i, j).getPolynomial)
                                Else
                                    m8.ret.exprMtx.getExpr(i, j) = m8.ret.exprMtx.getExpr(i, j)
                                End If
                            Next
                        Next
                    End If
                Catch ex2 As Exception
                Finally
                End Try
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return True
    End Function

    Public Function replaceVarsReferences(ByVal name As String) As Boolean
        Try
            Dim expr2 As ExprMatrix = getValueByName(name)
            Dim curID As Int64 = getVarIDByName(name)
            If expr2 Is Nothing Then
                Return False
            End If
            'Dim mc As MatchCollection = _
            '    Regex.Matches(expr2.ToString, _
            '            Replace( _
            '            cfg.mathGlobal.sAll2, "@", ""))
            Dim bMod As Boolean = False
            Dim sb As New StringBuilder(50)
            If curID >= Me.cur.Length Then
                Return False
            End If

            If Me.cur(curID) Is Nothing Then
                Return False
            End If
            Dim cur As currentMatch = Me.cur(curID).Clone
            cur.reStartCurMatch()
            Dim sVarsToExplore As String = ""
            Do While Not cur.bEnd
                'For i As Int64 = 0 To mc.Count - 1
                If cur.str = name Then
                    Return True
                End If
                Dim pos As Int64 = Array.IndexOf( _
                names, cur.str)
                If pos > -1 AndAlso pos < Me.cur.Length AndAlso _
                exprMtx(pos) IsNot Nothing Then
                    If InStr(cur.str, "(") > 1 Then
                        ' is a custom function as, f.ex., "P(" ... ")"
                        sb.Append("(")
                    End If
                    'replace var, inside another var,
                    'by its value:
                    sb.Append("(") ' wrap with ( ) because
                    ' otherwise in, for ex., D=x^2+y^2 when x=(y+4)
                    ' would become D=y+4^2+y^2 (erronously) instead
                    ' of D = (y+4)^2 +y^2 ...=y^2+8y*16+y^2
                    'sb.Append(exprMtx(pos).ToStringExprMtx(New Config))
                    Me.cur(pos).reStartCurMatch()
                    sb.Append(Me.cur(pos).toStrCurMatch("", False, False))
                    sb.Append(")")
                    bMod = True ' var to modify
                Else
                    sb.Append(cur.m)
                End If
                '
                cur.doNext()
            Loop
            If bMod Then
                ' update var if there have been changes:
                Dim eP As New matrixParser
                eP.bCaseS = cfg.bCaseSensitive
                Dim msg As String = ""
                'Dim cfg As Config = Nothing
                If eP.tryParse(sb.ToString, "", eP, msg, Nothing, cfg) Then
                    exprMtx(curID) = eP.ret.exprMtx
                End If
            End If
            'If Len(sVarsToExplore) Then
            '    Dim vExplore() As String = Split(sVarsToExplore, "|")
            '    For i As Int64 = 0 To vExplore.Length - 1
            '        If IsNested (
            '    Next
            'End If
        Catch ex As Exception
            Throw ex
        End Try
        Return False
    End Function

    Public Function IsNestedOLD(ByVal name As String) As Boolean
        Try
            Dim expr2 As ExprMatrix = getValueByName(name)
            Dim curID As Int64 = getVarIDByName(name)
            If expr2 Is Nothing Then
                Return False
            End If
            Dim mc As MatchCollection = _
                Regex.Matches(expr2.ToString, _
                        Replace( _
                        cfg.mathGlobal.sAll2, "@", ""))
            Dim bMod As Boolean = False
            Dim sb As New StringBuilder(50)
            For i As Int64 = 0 To mc.Count - 1
                If mc(i).ToString = name Then
                    Return True
                End If
                If Array.IndexOf(Me.names, mc(i).ToString) > -1 Then
                    If replaceVarsReferences(mc(i).ToString) Then
                        Return True
                    End If
                End If
                Dim pos As Int64 = Array.IndexOf( _
                names, mc(i).ToString)
                If pos > -1 Then
                    If exprMtx(pos) IsNot Nothing Then
                        ' replace var, inside another var,
                        ' by its value:
                        sb.Append("(") ' wrap with ( ) because
                        ' otherwise in, for ex., D=x^2+y^2 when x=(y+4)
                        ' would become D=y+4^2+y^2 (erronously) instead
                        ' of D = (y+4)^2 +y^2 ...=y^2+8y*16+y^2
                        Dim cfg1 As New Config(cfg)
                        cfg1.bRounding = False
                        sb.Append(exprMtx(pos).ToStringExprMtx(cfg1))
                        sb.Append(")")
                        bMod = True ' var to modify
                    Else
                        sb.Append(mc(i))
                    End If
                Else
                    sb.Append(mc(i))
                End If
            Next
            If bMod Then
                ' update var if there have been changes:
                Dim eP As New matrixParser
                Dim msg As String = ""
                'Dim cfg As Config = Nothing
                eP.bCaseS = cfg.bCaseSensitive
                If eP.tryParse(sb.ToString, "", eP, msg, Nothing, cfg) Then
                    exprMtx(curID) = eP.ret.exprMtx
                End If
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return False
    End Function

    Public Shared Function IsNested(cfg As Config, ByVal name As String, ByVal oVars As VarsAndFns) As Boolean
        Try
            Dim bMod As Boolean = False
            Dim sb As New StringBuilder(50)
            Dim ID As Int64 = oVars.getVarIDByName(name)
            Dim cur As currentMatch = oVars.cur(ID)
            cur.reStartCurMatch()
            Do While Not cur.bEnd
                If cur.str = name Then
                    Return True
                End If
                cur.doNext()
            Loop
        Catch ex As Exception
            Throw ex
        End Try
        Return False
    End Function
    Public Shared Function IsNestedOLD(cfg As Config, ByVal name As String, ByVal strExpr As String) As Boolean
        Try
            Dim mc As MatchCollection = _
                Regex.Matches(strExpr, _
                        Replace( _
                        cfg.mathGlobal.sAll2, "@", ""))
            Dim bMod As Boolean = False
            Dim sb As New StringBuilder(50)
            For i As Int64 = 0 To mc.Count - 1
                If mc(i).ToString = name Then
                    Return True
                End If
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return False
    End Function
    Public Function length() As Int64
        Return Me.names.Length
    End Function
    Public Function sortAs(ByVal names() As String) As Boolean
        Try
            Dim i As Int64
            Dim nom(names.Length - 1) As String
            Dim vExpr(names.Length - 1) As ExprMatrix
            Dim vcFn(names.Length - 1) As customFn
            For i = 0 To Me.names.Length - 1
                Dim pos As Int64 = Array.IndexOf(names, Me.names(i))
                If pos = -1 Then
                    Return False
                End If
                nom(pos) = Me.names(i)
                vExpr(pos) = Me.exprMtx(i)
                vcFn(pos) = Me.fns(i)
            Next
            Me.names = nom
            Me.exprMtx = vExpr
            Me.fns = vcFn
        Catch ex As Exception
            Throw ex
        End Try
        Return True
    End Function
    Public Function getVarsStrListToAppend() As String
        Dim e1 As String = ""
        If nomVars2Append.Length Then
            Dim e2(nomVars2Append.Length - 1) As String
            'Dim e3(nomVars2Append.Length - 1) As String
            Dim i, i2(e2.Length - 1) As Int64
            Dim fn As customFn = Nothing
            For i = 0 To e2.Length - 1
                If IsCustomFn(nomVars2Append(i), fn) Then
                    Dim name As String = Mid(fn.name, 1, Len(fn.name) - 1)
                    e2(i) = name + "\(" '+ ")"
                    'e3(i) = String.Format("{0:00}{1}", 100 - Len(name), name)
                    i2(i) = 999 - Len(name) '= 1
                Else
                    e2(i) = nomVars2Append(i)
                    'e3(i) = String.Format("{0:00}{1}", 100 - Len(e2(i)), e2(i))
                    i2(i) = -Len(e2(i)) ' =  0
                End If
            Next
            Array.Sort(i2, e2)
            Dim j As Int64 = 0
            For i = 0 To i2.Length - 1
                If Len(e1) Then e1 += "|"
                If i2(i) < 0 Then ' = 0 Then
                    e1 += e2(i)
                Else
                    If j = 0 Then
                        e1 += "(?<custFn>" + e2(i)
                        j = 1
                    Else
                        e1 += e2(i)
                    End If
                End If
            Next
            If Len(e1) Then
                If j Then
                    e1 += ")"
                End If
            End If
            'Array.Reverse(e2)
            'e1 = Join(e2, "|")
        End If
        Return e1
    End Function
    Public Sub AddVarDoNotValidate(ByVal nomVar As String, expr As Expression)
        Try
            Dim pos As Int64 = _
                 Array.IndexOf(names, nomVar, 0, names.Length)
            If pos = -1 Then
                pos = names.Length
                ReDim Preserve names(pos), Me.exprMtx(pos)
                names(pos) = nomVar
            End If
            If expr Is Nothing Then
                Me.exprMtx(pos) = Nothing
            Else
                Me.exprMtx(pos) = New ExprMatrix(expr)
            End If
        Catch ex As Exception

        End Try
    End Sub
    Public Sub AddNom2VarsListToAppend(ByVal nomVar As String)
        Try
            Dim pos As Int64 = _
                 Array.IndexOf(nomVars2Append, nomVar, 0, nomVars2Append.Length)
            If pos = -1 Then
                ReDim Preserve nomVars2Append(nomVars2Append.Length)
                nomVars2Append(nomVars2Append.Length - 1) = nomVar
            End If
        Catch ex As Exception

        End Try
    End Sub
    Public Sub RemoveVarByName(ByVal name As String)
        Try
            Dim ID As Int64 = getVarIDByName(name)
            names(ID) = ""
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Public Sub RemoveVarByID(ByVal ID As Int64)
        Try
            names(ID) = ""
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Public Function tryGetVarNameByID(ByVal ID As Int64, ByRef result As String, _
                                     Optional ByRef errMsg As String = "") As Boolean
        Try
            result = ""
            If ID < names.Length Then
                result = names(ID)
                If result <> "" Then
                    Return True
                End If
            End If
            ' variable not found
            errMsg = _
            String.Format(msg8.num(16), "ID=" + ID.ToString)
        Catch ex As Exception
            Throw ex
        End Try
        Return False
    End Function
    Public Function getVarNameByID(ByVal ID As Int64) As String
        Dim msg As String = ""
        Dim name As String = ""
        If Not tryGetVarNameByID(ID, name, msg) Then
            Throw New Exception(msg)
        End If
        Return name
    End Function
    Public ReadOnly Property getNamesList() As String()
        Get
            Return names
        End Get
    End Property
    Public Function tryfindVarWithValue(ByVal name As String, _
                                   ByRef ID As Int64) As Boolean
        Try
            name = Trim(name)
            If name <> "" Then
                If Not bCaseSensitive Then
                    'name = LCase(name)
                End If
                ID = Array.IndexOf(names, name)

                If ID > -1 AndAlso getValueByID(ID) IsNot Nothing Then
                    Return True
                End If
            End If
        Catch ex As Exception
            Throw ex
        End Try
        ' variable not found
        Return False
    End Function
    Public Function getVarIDByName(ByVal name As String, _
                                   Optional ByVal bThrowExcep As Boolean = True) As Int64
        Try
            name = Trim(name)
            If name <> "" Then
                If Not bCaseSensitive Then
                    'name = LCase(name)
                End If
                Dim pos As Int64 = Array.IndexOf(names, name)
                If pos > -1 Then
                    Return pos
                End If
            End If
            If bThrowExcep Then
                ' variable not found
                Throw New Exception( _
                String.Format(msg8.num(16), name))
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return -1
    End Function
    Public Function tryGetVarIDByName(ByVal name As String, ByRef resultID As Int64) As Boolean
        Try
            name = Trim(name)
            If name <> "" Then
                resultID = Array.IndexOf(names, name)
                If resultID > -1 Then
                    Return True
                End If
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return False
    End Function
    Public Function IsCustomFn(ByVal name As String, _
                            ByRef fn As customFn) As Boolean
        Try
            name = Trim(name)
            If name <> "" Then
                'If Not bCaseSensitive Then
                '    'name = LCase(name)
                'End If
                Dim pos As Int64 = Array.IndexOf(names, name)
                If pos > -1 Then
                    If pos >= Me.fns.Length OrElse _
                    Me.fns(pos) Is Nothing Then
                        Return False
                    End If
                    fn = Me.fns(pos)
                    Return True
                End If
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return False
    End Function
    Public Function getValueByName(ByVal name As String, _
                                   Optional ByVal bThrowExcep As Boolean = True) As ExprMatrix
        Try
            name = Trim(name)
            If name <> "" Then
                If Not bCaseSensitive Then
                    'name = LCase(name)
                End If
                Dim pos As Int64 = Array.IndexOf(names, name)
                If pos > -1 Then
                    If exprMtx(pos) Is Nothing Then
                        Return Nothing
                    End If
                    Return New ExprMatrix(exprMtx(pos))
                End If
            End If
            If bThrowExcep Then
                ' variable not found
                Throw New Exception( _
                String.Format(msg8.num(16), name))
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return Nothing
    End Function
    Public Function getValueByID(ByVal ID As Int64) As ExprMatrix
        Try
            If ID > -1 AndAlso ID < exprMtx.Length _
            AndAlso exprMtx(ID) IsNot Nothing Then
                Return New ExprMatrix(exprMtx(ID))
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return Nothing
    End Function
    Public Sub setValue(ByVal varID As Int64, ByVal exprValue As ExprMatrix)
        Try
            exprMtx(varID) = exprValue
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Public Function supressInternalVars(expr As Expression) As Expression
        Dim i As Int64
        If getNamesList.Length > 1 Then
            For i = getNamesList.Length - 1 To 1 Step -1
                For j As Int64 = 0 To getNamesList.Length - 1
                    Dim eM As ExprMatrix = getValueByID(i)
                    If eM IsNot Nothing Then
                        Dim expr1 As Expression = New Expression(eM.getCurExpr)
                        expr1 = expr1.evalExprToExpr(Me)
                        setValue(i, New ExprMatrix(expr1))
                    End If
                Next
            Next
        End If
        Return expr.evalExprToExpr(Me)
    End Function
    Public Overrides Function ToString() As String
        Dim e1 As String = String.Empty
        Try
            For i As Int64 = 0 To Me.getNamesList.Length - 1
                e1 += Me.getVarNameByID(i)
                Dim emtx As ExprMatrix = Me.getValueByID(i)
                If emtx IsNot Nothing Then
                    Dim e2 As String = " = " + emtx.ToString
                    e2 = Replace(e2, vbCrLf, " | ")
                    e1 += e2
                End If
                e1 += vbCrLf
            Next
        Catch ex As Exception

        End Try
        Return e1
    End Function
End Class
