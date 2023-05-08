Imports System.Drawing
Imports System.IO

Public Class ConvertAlg2RPN_RPL
    Inherits System.Web.UI.Page

    Dim nID As Int32
    Dim vNames() As String
    Dim vValues() As ExprMatrix
    Dim bDoEval As Boolean = True
    Dim cfg As New Config
    Dim mp As matrixParser
    Dim ToToken As New m8ToRPN
    Dim vTkn() As Token

    Public Shared sFn As String
    Public Shared sGreek As String =
    "ΑαΒβΓγΔδΕεΖζΗηΘθΙιΚκΛλΜμΝνΞξΟοΠπΡρΣσΤτΥυΦφΧχΨψΩω" + "∞"
    Dim vFn() As String = {
                "rank",
                "trn", "cross", "dot", "det", "egvl", "egv",
                "acosh", "acoth", "acsch", "asech", "asinh", "atanh",
                "coth", "csch", "sech", "sign", "gcd", "lcm",
                "acos", "acot", "acsc", "asec", "asin", "atan", "conj", "cosh", "neg", "sinh", "sqrt", "tanh",
                "abs", "arg", "cos", "cot", "csc", "exp", "log", "mod", "sec", "sin", "tan", "sqr",
                "im", "ln", "re", "√"}
    Dim sNum As String = "(?<num>((\d{1,3}((\,\d{3})+)(\.[0-9]+)?)|[\.\d]+)([eE][-+]?[0-9]+)?)"
    Public Shared sCnt As String '= "(?<cnt>(?i)(" + sCnts + "){1}(?-i))"
    Public Shared sLP As String = "(?<lp>\(|\{|\[){1}"
    Public Shared sRP As String = "(?<rp>\)|\}|\]){1}"
    Public Shared sCol As String = "(?<col>(\;|\t|\,){1})"
    Public Shared sRow As String = "(?<row>(\r\n|\r|\n|\||\]\[){1,})"
    Public Shared sSpace As String = "(?<space>[ ]+)"
    Public Shared vOp() As String = {"+", "-", "*", "/", "%", "^", "!"}
    Public Shared vLgOp() As String = {"and", "or", "xor", "not", "nand", "nor"}
    Public Shared sLgOp As String = "(?<lgOp>(?i)" + Join(vLgOp, "|") + "(?-i))"
    Public Shared sMtxOp As String = "(((\r|\n|\|)+||(?<=\]))(?<mtxOp>\" + Join(vOp, "|\") + ")((\r|\n|\|)+|(?=\[)))"
    Public Shared vCntsCaseNonSen() As String = {"pi"}
    Public Shared vCntsCaseSen() As String = {"e"}
    Shared sSqrt As String = "(?<fn>(?<sqr>√))"
    Public Shared sOp As String = "(?<op>\" + Join(vOp, "|\") + "{1})"
    Shared sCnts As String = Join(vCntsCaseSen, "|") +
        "|(?i)" + Join(vCntsCaseNonSen, "|") + "(?-i)"
    Public sVar As String = "(?<var>_([0-9a-zA-Z_" + sGreek + "])*{})"
    Public Shared sEqual As String = "(?<equal>\=)"
    Public sVar2 As String
    Dim vAll() As String
    Dim sAll As String

    Private Sub Page_PreRender(sender As Object, e As EventArgs) Handles Me.PreRender
        Try
            C_sessionWeb.contador(Page, Request, Response)
        Catch ex As Exception
        End Try
        Try
            nID = C_sessionWeb.getID(Page)
            C_sessionWeb.sProvieneDe(nID) = Request.Url.AbsoluteUri
        Catch ex As Exception
        End Try
        Try
            Dim o = Request.Params("eDiv")
            If o IsNot Nothing Then
                UpdatePanel(CStr(o))
                'ScriptManager.RegisterStartupScript(Me, Me.GetType(), "hwa", "Update1();", True)
                Exit Try
            End If
            eDiv.InnerHtml = Color(hiddeneDiv.Value)
            If Not Page.IsPostBack Then
                'Dim cr As String = vbCrLf
                'tbVars.Text = "x = &h1E&d" + cr +
                '    "_y12 = 4" + cr +
                '    "z = 0" + cr +
                '    "A = 1" + cr
                'tbInput.Text = "1,600/((A-(A+A/_y12)^x)/(A-(A+cos(z)/_y12))) // comment"
                populateFns()
            Else
                'populateFns()
                'Dim e1 = Request.Form("__EVENTTARGET")
                'Dim e2 = Request.Form("__EVENTARGUMENT")
                'If e2 IsNot Nothing AndAlso Len(e2) Then
                '    Me.Link1_Click(e2)
                'End If
            End If
            'ScriptManager.RegisterStartupScript(Me, Me.GetType(), "hwa", "Update1();", True)

        Catch ex As Exception

        End Try
    End Sub

    Private Sub BtnConvert_Click(sender As Object, e As EventArgs) Handles BtnConvert.Click
        Try
            Calculate()
            ' Session.Abandon()
            ' Response.Cookies.Add(New HttpCookie("ASP.NET_SessionId", ""))
        Catch ex As Exception

        End Try
    End Sub
    Private Sub Calculate()
        Dim eP = New exprParser84
        Try
            If Not bDoEval Then Exit Sub
            Dim us As New Globalization.CultureInfo("en-US")
            Dim ni As Globalization.NumberFormatInfo = us.NumberFormat
            lblOutput.Text = ""
            lblMessage.Text = ""
            lblDetail.Text = ""
            'divInput.InnerHtml = HiddenField1.Value

            If chkStack.Checked Then
                'chkShowComments.Checked = False
            End If
            Dim t1 As Int64 = Now.Ticks
            eP.cfg.outputFormat = outputMessage.HTML
            'Dim sInput As String = HiddenField1.Value
            Dim sInput As String = hiddeneDiv.Value '            eDiv.InnerText
            sInput = SupressHTMLtags(sInput)
            sInput = Replace(sInput, "√", "sqr")
            Dim vVar() As String = Split(sInput, "@")
            If vVar.Length > 1 Then
                Dim sVar As String = ""
                For i As Int32 = 1 To vVar.Length - 1
                    sVar += vVar(i) + vbCrLf
                Next
                tbVars.Text += vbCrLf + sVar
            End If
            sInput = vVar(0)
            If InStr(sInput, "@") Then
                lblMessage.Text = "n/a"
                Response.End()
                Exit Sub
            End If

            Dim vInput() As String = Split(sInput, "//")
            sInput = vInput(0)


            getVarsValues()
            'If vNames.Length Then
            '    chkPurge.Checked = False
            '    chkPurge.Enabled = False
            '    chkAssign.Checked = False
            '    chkAssign.Enabled = False
            'Else
            '    chkPurge.Enabled = True
            '    chkAssign.Enabled = True
            'End If
            Dim sM = "  " ' margen: dos espacios
            Dim sLog As String = ""
            For i As Int32 = 0 To vInput.Length - 1
                sLog += vInput(i)
                If i < vValues.Length Then
                    sLog += " =" + vValues(i).ToString
                End If
                sLog += vbCrLf
            Next

            logFile(Nothing, vInput(0), sLog)
            'If Regex.IsMatch(vInput(0), "[\@\=]") Then
            '    Dim msg As String = "n/a: " + vInput(0)
            '    msg = Replace(msg, "=", "<span style=""color:red"">=</span>")
            '    msg = Replace(msg, "@", "<span style=""color:red"">@</span>")
            '    lblMessage.Text = msg
            '    lblStack.Text = "<br /><br /><br /><br />"
            '    Exit Try
            'End If
            mp = New matrixParser
            Dim oVars As New VarsAndFns(cfg)
            For i = 0 To vNames.Length - 1
                oVars.AddVar(vNames(i), New Expression(vValues(i)))
            Next

            cfg.bDetail = True
            cfg.bCaseSensitive = True
            cfg.bAltMtxOutput = False
            sInput = Replace(sInput, " ", "")
            If sInput Is Nothing OrElse sInput.Length = 0 Then
                Exit Sub
            End If
            sInput = Replace(sInput, "]]", "]")
            sInput = Replace(sInput, "[[", "[")
            sInput = Regex.Replace(sInput, "\](\,?)\[", vbCrLf)
            mp.parse(sInput, "", oVars, cfg) ' mates8.3 parse
            'If chkRPL.Checked = False Then
            '    chkListProcessing.Checked = False
            '    chkListProcessing.Enabled = False
            'Else
            '    chkListProcessing.Enabled = True
            'End If
            'If oVars.getNamesList.Length = 0 Then
            '    chkPurge.Checked = False
            '    chkAssign.Checked = False
            '    chkPurge.Enabled = False
            '    chkAssign.Enabled = False
            '    chkListProcessing.Checked = False
            '    chkListProcessing.Enabled = False
            'ElseIf vNames.Length = 0 Then
            '    chkListProcessing.Checked = False
            '    chkListProcessing.Enabled = False
            '    chkPurge.Enabled = True
            '    chkAssign.Checked = False
            '    chkAssign.Enabled = False
            'Else
            '    chkListProcessing.Enabled = True
            '    chkPurge.Enabled = True
            '    chkAssign.Enabled = True
            'End If
            'If chkAssign.Checked Then
            '    chkPurge.Checked = False
            'ElseIf chkPurge.Checked Then
            '    chkAssign.Checked = False
            'End If

            '			         |casilla variables		|
            '			         |vacía	|con variables	|							
            'INPUT	sin variables|	a	|   b			|
            '	con variables	 |  c	|   d								
            '											
            '											
            '	RPL	stack	list processing	assignments	purge	evaluate	comments
            'a		off	         off	        off	     off		
            'b	casilla variables desactivada						
            'c			       off	         off			
            'd							

            chkStack.Enabled = True
            tbVars.Enabled = True
            chkListProcessing.Enabled = True
            chkAssign.Enabled = True
            chkPurge.Enabled = True
            If oVars.getNamesList.Length = 0 Then
                If vNames.Length = 0 Then
                    ' a: casilla vars. off / stack off / list processing off / assignments off / purge off
                    chkStack.Checked = False
                    chkStack.Enabled = False
                    chkListProcessing.Checked = False
                    chkListProcessing.Enabled = False
                    chkAssign.Checked = False
                    chkAssign.Enabled = False
                    chkPurge.Checked = False
                    chkPurge.Enabled = False
                Else
                    ' b casilla variables desactivada
                    tbVars.Text = ""
                    tbVars.Enabled = False
                End If
            Else
                If vNames.Length = 0 Then
                    ' c list processing off / assignments off
                    chkListProcessing.Checked = False
                    chkListProcessing.Enabled = False
                    chkAssign.Checked = False
                    chkAssign.Enabled = False
                End If
            End If





            If mp.errMsg.Length Then
                lblMessage.Text = "n/a"
                lblOutput.Text = ""
                Exit Try
            End If
            vTkn = ToToken.M8ToRPN(mp)
            ToToken.SetStack(vTkn, eP)




            lblStack.Text = ""
            Dim result As Token = Nothing
            Dim cplxRoots() As Complex = Nothing
            If chkEval.Checked Then
                If chkRounding.Checked Then
                    cfg.bRounding = True
                Else
                    cfg.bRounding = False
                End If
                cfg.bAltMtxOutput = True
                If cplxRoots Is Nothing Then
                    Dim sBr As String = "<br />"
                    Dim iniBlue As String = "<span style=""color:blue"">"
                    Dim sStop As String = "</span>"
                    Dim sOut As String = iniBlue + "Question:" + sStop + sBr
                    Dim sVar As String = ""
                    For i = 0 To vNames.Length - 1
                        If i < vValues.Length Then
                            sVar += vNames(i) + "=" + vValues(i).ToStringExprMtx(cfg)
                            If i + 1 < vNames.Length Then
                                sVar += ", "
                            End If
                        End If
                    Next
                    If Len(sVar) Then
                        sVar = " | {" + sVar + "}"
                        sOut += sInput + sVar + sBr
                    Else
                        sOut += sInput + sBr
                    End If
                    sOut += iniBlue + "Answer:" + sStop + sBr
                    Dim e3 As String = mp.ret.toStringRetVal(cfg) ' result.ToString(cfg)
                    e3 = Replace(e3, vbCrLf, "<br />")
                    sOut += e3
                    lblOutput.Text = sOut
                Else
                    lblOutput.Text = ""
                    For i = 0 To cplxRoots.Length - 1
                        lblOutput.Text += cplxRoots(i).toString + vbCrLf
                    Next
                End If
                cfg.bAltMtxOutput = False
            End If


            Dim e1 As String = ""
            Dim bShowVarValues As Boolean =
                IIf(chkShowComments.Checked AndAlso chkStack.Checked, True, False)
            cfg.bAltMtxOutput = True
            eP.ToStringStack(e1, cfg, vNames, vValues, Not chkStack.Checked, bShowVarValues)
            'e1 = Me.ConvertToRPN(sInput)
            cfg.bAltMtxOutput = False
            Dim sAssign As String = ""
            Dim sListProcessingA As String = ""
            Dim sListProcessingB As String = ""
            Dim sPurge As String = ""
            If chkAssign.Checked Then
                For i As Int32 = 0 To vNames.Length - 1
                    If Len(Trim(vNames(i))) Then
                        If i < vValues.Length Then
                            Dim sName As String = "<span style='color:brown'>" + Replace(vNames(i), "_", "") + "</span>"
                            Dim sApostrofe As String = "<span style='color:grey'>'</span>"
                            If chkStack.Checked Then
                                If chkListProcessing.Checked Then
                                    sListProcessingA += vValues(i).ToString
                                    sListProcessingB += "'" + Replace(vNames(i), "_", "") + "'"
                                    If i < vNames.Length - 1 Then
                                        sListProcessingA += ","
                                        sListProcessingB += ","
                                    End If
                                Else
                                    sAssign += sM + vValues(i).ToString
                                    sAssign += vbLf + "  " + sApostrofe + sName + sApostrofe + vbLf + "  STO" + vbLf
                                    If i < vNames.Length - 1 Then
                                        sAssign += vbLf
                                    End If
                                End If
                            Else
                                If chkListProcessing.Checked Then
                                    sListProcessingA += vValues(i).ToString
                                    sListProcessingB += "'" + Replace(vNames(i), "_", "") + "'"
                                    If i < vNames.Length - 1 Then
                                        sListProcessingA += ","
                                        sListProcessingB += ","
                                    End If
                                Else
                                    sAssign += " " + sApostrofe + sName + sApostrofe + " STO" + vbLf
                                End If
                            End If
                        End If
                    End If
                Next
            End If
            If chkAssign.Checked = False Then
                sAssign = ""
            Else
                If chkListProcessing.Checked Then
                    If chkStack.Checked Then
                        sAssign = "{" + sListProcessingA + "}" + vbLf
                        sAssign += "{" + sListProcessingB + "}" + vbLf + " STO" + vbLf
                    Else
                        sAssign = "{" + sListProcessingA + "} "
                        sAssign += "{" + sListProcessingB + "} STO" + vbLf
                    End If
                End If
            End If
            If chkPurge.Checked Then
                Dim sClear As String = ""
                If chkShowComments.Checked Then
                    sClear = "  @ Clear variables" + vbLf
                End If
                Dim oVar As VarsAndFns = mp.vars
                For i As Int32 = 0 To oVar.getNamesList.Length - 1
                    If Len(Trim(oVar.getNamesList(i))) Then
                        Dim sName As String = "<span style='color:brown'>" + Replace(oVar.getNamesList(i), "_", "") + "</span>"
                        Dim sApostrofe As String = "<span style='color:grey'>'</span>"
                        If chkStack.Checked = False Then
                            If chkListProcessing.Checked Then
                                sPurge += " " + sApostrofe + sName + sApostrofe
                                If i < oVar.getNamesList.Length - 1 Then
                                    sPurge += ","
                                End If
                            Else
                                sPurge += " " + sApostrofe + sName + sApostrofe + " PURGE" + vbLf
                            End If
                        Else
                            If chkListProcessing.Checked Then
                                sPurge += " " + sApostrofe + sName + sApostrofe
                                If i < oVar.getNamesList.Length - 1 Then
                                    sPurge += ","
                                End If
                            Else
                                sPurge += " " + sApostrofe + sName + sApostrofe + vbLf + " PURGE" + vbLf
                            End If
                        End If
                    End If
                Next
                If chkListProcessing.Checked Then
                    If chkStack.Checked = False Then
                        sPurge = "{" + sPurge + "} PURGE" + vbLf
                    Else
                        sPurge = "{" + sPurge + "}" + vbLf + " PURGE" + vbLf
                    End If
                End If
                sPurge = sClear + sPurge
            End If
            e1 = Replace(e1, "_", "")
            Dim ve1() As String = Split(e1, vbCrLf)
            For i As Int32 = 0 To ve1.Length - 1
                If InStr(ve1(i), "=") Then
                    If bShowVarValues Then
                        ' mostrar los comentarios:
                        ve1(i) = Replace(ve1(i), "=", "@ =")
                    Else
                        ' suprimir el comentario: (ya viene suprimido de antes
                        ' por eso se comenta aqui)
                        've1(i) = Regex.Replace(ve1(i), " \= [\-a-zA-Z0-9]+ ", " ")
                        've1(i) = Regex.Replace(ve1(i), " \=(\W|\w)+", "")
                    End If
                End If
            Next
            If ve1.Length Then
                Do While ve1.Length AndAlso Trim(ve1(ve1.Length - 1)) = ""
                    ReDim Preserve ve1(ve1.Length - 2)
                Loop
                If vInput.Length > 1 AndAlso ve1.Length AndAlso chkShowComments.Checked Then
                    ve1(ve1.Length - 1) += "  @" + vInput(1)
                End If

                If chkRPL.Checked Then
                    lblStack.Text = "« "
                    If chkShowComments.Checked Then
                        lblStack.Text += "@ BEGIN_PROGRAM" + vbLf
                    Else
                        lblStack.Text += vbLf
                    End If
                Else
                    'If chkShowComments.Checked Then
                    '    lblStack.Text += "@ BEGIN_PROGRAM" + vbLf
                    'End If
                End If
                If chkPurge.Checked Then
                    lblStack.Text += vbLf + sPurge
                End If
                If chkShowComments.Checked AndAlso chkAssign.Checked AndAlso sAssign.Length Then
                    lblStack.Text += vbLf + "  @ ASSIGNMENTS" + vbLf + sAssign + vbLf
                Else
                    If chkShowComments.Checked AndAlso chkListProcessing.Checked AndAlso sAssign.Length Then
                        lblStack.Text += vbLf + "  @ LIST PROCESSING" + vbLf + sAssign + vbLf
                    Else
                        lblStack.Text += sAssign + vbLf
                    End If
                End If
                If chkShowComments.Checked Then
                    lblStack.Text += "  @ RPN of " + vInput(0) + vbLf
                End If
                If chkStack.Checked = False Then
                    ' Agrupar (A*B)+(C/D):
                    ' @ RPN of (A*B)+(C/D)
                    ' A B *
                    ' C D /
                    ' +
                    'Dim ve2(-1) As String, i2 As Int32 = 0
                    'For i = ve1.Length - 1 To 0 Step -1

                    'Next
                End If

                ' Colorear:
                'Dim re As New Regex(cfg.mathGlobal.sAll2)
                For i = 0 To ve1.Length - 1
                    'Dim mc As MatchCollection = re.Matches(ve1(i))
                    'Dim sb As New StringBuilder(50)
                    'For Each m As Match In mc
                    '    If m.Groups("num").Success Then
                    '        sb.Append("<span style='color:orange;'>" + m.ToString + "</span>")
                    '    ElseIf m.Groups("op").Success Then
                    '        sb.Append("<span style='color:#8B0000;'>" + m.ToString + "</span>") ' dark red
                    '    ElseIf m.Groups("fn").Success Then
                    '        sb.Append("<span style='font-weight:bold;'>" + m.ToString + "</span>")
                    '    ElseIf m.Groups("var2").Success OrElse m.Groups("var").Success Then
                    '        sb.Append("<span style='color:#1B1BB1;'>" + m.ToString + "</span>") ' dark blue
                    '    Else
                    '        sb.Append(m.ToString)
                    '    End If
                    'Next
                    've1(i) = sb.ToString
                    ve1(i) = Color(ve1(i))
                Next
                lblStack.Text += Join(ve1, vbLf + sM) + vbLf
                If chkRPL.Checked Then
                    lblStack.Text += vbLf + "» "
                    If chkShowComments.Checked Then
                        lblStack.Text += "@ END_PROGRAM" + vbLf
                    Else
                        lblStack.Text += vbLf
                    End If
                Else
                    'If chkShowComments.Checked Then
                    '    lblStack.Text += vbLf + "@ END_PROGRAM"
                    'End If
                End If
                If chkEval.Checked Then
                    If chkShowComments.Checked Then
                        lblStack.Text += vbLf + "  @ EVALUATION" + vbLf
                    End If
                    If chkStack.Checked Then
                        lblStack.Text += vbLf + "DUP" + vbLf + "EVAL" + vbLf
                    Else
                        lblStack.Text += "DUP EVAL" + vbLf
                    End If
                End If
                Do While lblStack.Text.Length AndAlso lblStack.Text.Chars(0) = vbLf
                    lblStack.Text = Mid(lblStack.Text, 2)
                Loop
                'lblStack.Text = Replace(lblStack.Text, vbLf + vbLf + vbLf, vbLf + vbLf)
                Dim nRows As Int32 = 1 + Regex.Matches(lblStack.Text, "\x0A").Count
                If nRows < 15 Then nRows = 15
                'lblStack.Rows = nRows
            End If
            Dim ve2() As String = Split(lblStack.Text, vbLf)
            For i = 0 To ve2.Length - 1
                Dim pos As Int32 = InStr(ve2(i), "@")
                If pos Then
                    ve2(i) = Left(ve2(i), pos - 1) + "<span style='color:green'>" + Mid(ve2(i), pos) + "</span>"
                Else
                    ve2(i) = Regex.Replace(ve2(i), "(?<color>STO|DUP|EVAL|PURGE)",
                                         "<span style='color:orange'>${color}</span>")
                End If
                ve2(i) = Regex.Replace(ve2(i), "(?<color>«|»)",
                                     "<span style='color:grey'>${color}</span>")
            Next
            lblStack.Text = Join(ve2, vbLf)
            If chkDetail.Checked Then
                detail(sInput)
            End If
            lblStack.Text = Replace(lblStack.Text, vbLf, "<br />")
            logFile(Nothing, lblStack.Text, "")
        Catch ex As Exception
            logFile(ex, "", "")
            lblMessage.Text = ex.Message
        Finally
            ' Was there any error?
            If eP.retErr IsNot Nothing Then
                lblMessage.Text = eP.retErr.ToString
            End If
        End Try
    End Sub
    Function findtbInputControl(Optional parent As Control = Nothing) As HtmlTextArea
        Dim ctr As HtmlTextArea = Nothing
        Dim curr = parent
        If curr Is Nothing Then curr = Page
        For Each c As Control In curr.Controls
            If c.Controls.Count Then
                findtbInputControl(c)
            End If
            If c.GetType Is GetType(HtmlTextArea) Then
                Return c
            End If
        Next
        Return ctr
    End Function
    Private Sub BtnClearAll_Click(sender As Object, e As EventArgs) Handles BtnClearAll.Click
        Try
            eDiv.InnerHtml = ""
            hiddeneDiv.Value = ""
            lblStack.Text = "<br /><br /><br /><br />"
            tbVars.Text = ""
            lblMessage.Text = ""
            lblOutput.Text = ""
            lblDetail.Text = ""
            hiddendiv.Value = ""
            imgStandard.ImageUrl = ""
            cbExamples.SelectedIndex = 0
            cbFns.SelectedIndex = 0
        Catch ex As Exception

        End Try
    End Sub
    Sub getVarsValues()
        Try
            Dim eP As New matrixParser
            Dim vRow() As String = Split(tbVars.Text, vbCrLf)
            Dim iv As Int32
            For iRow As Int32 = 0 To vRow.Length - 1
                Dim vR() As String = Split(Trim(vRow(iRow)), "=")
                If vR.Length = 2 Then
                    vR(0) = Trim(vR(0))
                    ReDim Preserve vNames(iv), vValues(iv)
                    vNames(iv) = vR(0)
                    eP.parse(vR(1))
                    If eP.errMsg.Length = 0 Then
                        vValues(iv) = New ExprMatrix(eP.ret.exprMtx)
                        iv += 1
                    End If
                End If
            Next
            iv -= 1
            ReDim Preserve vNames(iv), vValues(iv)
        Catch ex As Exception
            lblMessage.Text = ex.ToString
        End Try
    End Sub

    Sub populateFns()
        Try
            Dim i As Int32
            cbFns.Items.Clear()
            cbFns.Items.Add("Functions:")
            Dim vFn(MathGlobal8.vFn.Length - 1) As String
            Array.Copy(MathGlobal8.vFn, vFn, vFn.Length)
            'Array.Sort(vFn)
            Dim i1(vFn.Length - 1), i2(vFn.Length - 1) As String
            Dim i0(vFn.Length - 1) As Int32
            Dim j As Int32 = 0
            Dim vFn2(vFn.Length - 1) As String
            For i = 0 To vFn.Length - 1
                If Array.IndexOf(Config84.vFn, LCase(vFn(i))) > -1 Then
                    i0(j) = CType(MathGlobal8.vFnType(i), Int32)
                    i1(j) = Chr(MathGlobal8.vFnType(i) + 48) + vFn(i)
                    vFn2(j) = vFn(i)
                    j += 1
                End If
            Next
            ReDim Preserve i0(j), i1(j), i2(j), vFn2(j)
            Array.Copy(i1, i2, i1.Length)
            Array.Sort(i1, vFn2)
            Array.Sort(i2, i0)
            Dim oldT As Int32 = -1
            For i = 0 To vFn2.Length - 1
                If i0(i) <> oldT Then
                    Dim itm As New ListItem
                    If i Then
                        itm.Text = "        "
                        itm.Value = "---"
                        cbFns.Items.Add(itm)
                        itm = New ListItem
                    End If
                    Select Case CType(i0(i), MathGlobal8.FnType)
                        Case MathGlobal8.FnType.num : itm.Text = "Numeric"
                        Case MathGlobal8.FnType.complex : itm.Text = "Complex"
                        Case MathGlobal8.FnType.matrix : itm.Text = "Matrix"
                        Case MathGlobal8.FnType.polyn : itm.Text = "Polynomial"
                        Case MathGlobal8.FnType.trig : itm.Text = "Trigonometric"
                        Case MathGlobal8.FnType.vector : itm.Text = "Vectorial"
                        Case MathGlobal8.FnType.other : itm.Text = "Other"
                    End Select
                    Dim ln As Int32 = itm.Text.Length
                    itm.Value = "---"
                    cbFns.Items.Add(itm)
                    itm = New ListItem
                    itm.Text = StrDup(ln, "-")
                    itm.Value = "---"
                    cbFns.Items.Add(itm)
                    oldT = i0(i)
                End If
                Dim item As New ListItem()
                item.Text = UCase(vFn2(i))
                item.Value = UCase(vFn2(i))
                cbFns.Items.Add(item) ' load functions into the combobox
            Next
        Catch ex As Exception

        End Try
    End Sub
    Private Sub cbFns_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbFns.SelectedIndexChanged
        Try
            If cbFns.SelectedIndex > 0 Then
                'Dim sFile As String = readHistory()
                'Me.setSession(False, sFile)
                Dim e1 As String = cbFns.SelectedItem.Text
                If cbFns.SelectedValue.Chars(0) = "-" Then
                    Exit Sub
                End If
                If e1.Chars(0) = "-" Then
                    Exit Sub
                End If
                If e1 <> "pi" AndAlso e1 <> "e" Then
                    e1 += "()"
                End If
                'Dim tbInput As HtmlTextArea = FindControl("tbInput")
                'HiddenField1.Value = e1
                Dim e2 As String = hiddeneDiv.Value ' eDiv.InnerText
                e2 = SupressHTMLtags(e2) + e1
                eDiv.InnerHtml = Color(e2)
                hiddendiv.Value = SupressHTMLtags(hiddeneDiv.Value) '(eDiv.InnerHtml)
            End If
        Catch ex As Exception

        End Try
    End Sub
    Public Function SupressHTMLtags(e1 As String)
        Return Regex.Replace(e1, "\<[^\>]+\>", "")
    End Function
    Sub logFile(ByVal ex As Exception, ByVal e1 As String, ByVal e2 As String)
        If Not Request.IsLocal Then
            C_sessionWeb.logFile(ex, e1, e2, Me.Page, nID, "polycalc.txt")
        End If
    End Sub

    Private Sub cbExamples_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbExamples.SelectedIndexChanged
        Try
            With cbExamples
                Dim sValue As String = .SelectedValue
                If sValue = "--" Then
                    Exit Try
                End If
                Dim vVar() As String = Split(sValue, "@")
                Dim sVar As String = ""
                Dim i As Int32
                For i = 1 To vVar.Length - 1
                    sVar += vVar(i) + vbCrLf
                Next
                bDoEval = False
                chkEval.Checked = True
                bDoEval = True
                tbVars.Text = sVar
                hiddendiv.Value = vVar(0)
                hiddeneDiv.Value = vVar(0)
                eDiv.InnerHtml = Color(vVar(0))
                imgStandard.ImageUrl = ""
                Calculate()
            End With
        Catch ex As Exception

        End Try
    End Sub

    Sub detail(sInput As String)
        Dim eP As New exprParser84
        Try

            Dim sArry As String = String.Empty
            Dim tkn As Token = Nothing

            'eP.Parse(sInput)


            cfg.bDetail = chkDetail.Checked
            Config.reset(cfg)
            Dim sResult As String = ""

            mp.cfg.outputFormat = outputMsgFormat.HTML
            Dim result As Token = Nothing
            Dim bShowDivision As Boolean = False
            If vTkn Is Nothing OrElse
                Not eP.ToStringDetail(result, sResult, ToToken.vVar, ToToken.vVal, outputMessage.HTML, vTkn) Then
                sInput = SupressHTMLtags(sInput)
                If InStr(UCase(sInput), "MOD") Then
                    sInput = Replace(UCase(sInput), "MOD", "/")
                    mp.parse(sInput, cfg:=cfg)
                    bShowDivision = True
                    cfg.oDetail.ie = 0
                Else
                    sResult = cfg.cur.toStringDetailStack
                End If
            Else
                sResult = Replace(sResult, vbCrLf, "<br />")
            End If
            Dim e1 As String = cfg.oDetail.ToStringDivisionHTML("navy")
            If bShowDivision OrElse InStr("Division of", e1) Then
                sResult += e1
            End If

            sResult = Replace(sResult, "%", "MOD")
            lblDetail.Text = "<span style=""color:blue"">Evaluation Step by Step:</span><br />"
            If mp.ret.exprMtx.Rows + mp.ret.exprMtx.Cols > 2 Then
                Exit Sub
            End If
            lblDetail.Text += sResult
            logFile(Nothing, lblDetail.Text, "")
        Catch ex As Exception
            If eP.retErr IsNot Nothing Then
                lblMessage.Text = eP.retErr.Message
            Else
                lblMessage.Text = ex.Message
            End If
            logFile(ex, "error in step by step", "")
        Finally
        End Try

    End Sub

    Private Sub chkShowComments_CheckedChanged(sender As Object, e As EventArgs) Handles chkShowComments.CheckedChanged
        Calculate()
    End Sub

    Private Sub chkAssign_CheckedChanged(sender As Object, e As EventArgs) Handles chkAssign.CheckedChanged
        If chkAssign.Checked Then
            chkPurge.Checked = False
            'chkListProcessing.Checked = False
        End If
        Calculate()
    End Sub
    Private Sub chkEval_CheckedChanged(sender As Object, e As EventArgs) Handles chkEval.CheckedChanged
        Calculate()
    End Sub
    Private Sub chkRPL_CheckedChanged(sender As Object, e As EventArgs) Handles chkRPL.CheckedChanged
        Calculate()
    End Sub
    Private Sub chkStack_CheckedChanged(sender As Object, e As EventArgs) Handles chkStack.CheckedChanged
        Calculate()
    End Sub
    Private Sub chkDetail_CheckedChanged(sender As Object, e As EventArgs) Handles chkDetail.CheckedChanged
        Calculate()
    End Sub

    Private Sub chkRounding_CheckedChanged(sender As Object, e As EventArgs) Handles chkRounding.CheckedChanged
        Calculate()
    End Sub
    Private Sub chkPurge_CheckedChanged(sender As Object, e As EventArgs) Handles chkPurge.CheckedChanged
        If chkPurge.Checked Then
            chkAssign.Checked = False
        End If
        Calculate()
    End Sub
    Private Sub chkListProcessing_CheckedChanged(sender As Object, e As EventArgs) Handles chkListProcessing.CheckedChanged
        If chkRPL.Checked = False Then
            chkListProcessing.Checked = False
            chkListProcessing.Enabled = False
            Exit Sub
        End If
        'If chkListProcessing.Checked Then
        '    chkAssign.Checked = False
        'End If
        Calculate()
    End Sub



    Private Function ConvertToRPN(sInput As String) As String
        Dim eP = New exprParser84
        Dim mp As New matrixParser
        Dim cfg As New Config
        cfg.outputFormat = outputMsgFormat.plainText
        Dim oVars As New VarsAndFns(cfg)
        For i As Int32 = 0 To vNames.Length - 1
            oVars.AddVar(vNames(i), Nothing)
            If i < vValues.Length Then
                oVars.setValue(i, vValues(i))
            End If
        Next
        mp.parse(sInput, cfg:=cfg)
        If mp.errMsg.Length Then
            Return mp.errMsg
        End If
        Dim ToToken As New m8ToRPN
        ' vTkn returns as an infix (RPN) notation stack:
        Dim vTkn() As Token = ToToken.M8ToRPN(mp)
        ToToken.SetStack(vTkn, eP)

        Dim e1 As String = String.Empty
        eP.ToStringStack(e1, cfg)
        Return e1
    End Function

    Private Sub UpdatePanel(param As String)
        Try
            Response.Clear()
            Response.ClearContent()
            param = Regex.Replace(param, "^(\|)+|(\|)+$", "")
            Do While InStr(param, "||")
                param = Replace(param, "||", "|")
            Loop
            Do While InStr(param, "&nbsp;")
                param = Replace(param, "&nbsp;", "")
            Loop
            If param.Length = 0 Then
                Response.Write("")
                Exit Try
            End If
            Dim ve1() As String = Split(param, "|")
            Dim e1 As String = ""
            e1 = Color(Join(ve1))
            'e1 += "<span id='row'></span>"
            eDiv.InnerHtml = e1
            hiddeneDiv.Value = Join(ve1)
            Response.Write(e1)
            Response.End()
        Catch ex As Exception
        End Try
    End Sub

    Public Function sAll2() As String
        Return Replace(sAll, "{}", "|" + "")
    End Function
    Public Sub Initialize()
        'Optional ByVal sImaginary As String = "i")
        Try
            sFn = "(?<fn>(?i)" + Join(vFn, "|") + "(?-i))"
            sVar2 = "(?<var2>([a-zA-Z" + sGreek + "]){1}([a-zA-Z" + sGreek + "]){0,1})"

            Dim sImag As String

            sCnt = "(?<cnt>(?i)(?<![A-Za-z]+)(" + sCnts + "){1}(?![A-Za-z]+)(?-i))"
            Dim sImg As String = "i"
            sImag = "(?<img>(?<![A-Za-z]+)" + sImg + "(?![A-Za-z]+){1})"
            vAll = New String() {sEqual, sFn, sNum, sVar, sCnt, sImag, sVar2, sMtxOp, sOp, sLP, sRP,
                                 sSqrt, sCol, sRow, sSpace,
                                  "(?<eq>\=)", "(?<resto>.+)"}
            sAll = "(?<all>\G" + Join(vAll, "|") + ")"
        Catch ex As Exception
            Throw ex
        End Try
    End Sub

    Function Color(e1 As String) As String
        Dim sb As New StringBuilder(50)
        Static bInit = False
        Try
            If Not bInit Then
                Initialize()
                bInit = True
            End If
            Dim re As New Regex("(?<op>[-+*\/\^\!]+)|" + sNum +
                                "|" + sFn + "|" + sVar2 + "|" + sVar +
                                "|[\@\=\,\;\|\r\n]|(?<parenth>[\[\]\(\)\{\}])|(?<any>.+)", RegexOptions.IgnoreCase)
            Dim mc As MatchCollection = re.Matches(e1)
            sb.Append("<span style=""font-family:'Courier New', Courier, 'Lucida Sans Typewriter', 'Lucida Typewriter'"">")
            For Each m As Match In mc
                If m.ToString.Length Then
                    If m.Groups("num").Success Then
                        sb.Append("<span style='color:#5c6bc0;'>" + m.ToString + "</span>")
                    ElseIf m.Groups("op").Success Then
                        sb.Append("<span style='color:#ff1744;font-weight:bold;'>" + m.ToString + "</span>") ' dark red
                    ElseIf m.Groups("fn").Success Then
                        sb.Append("<span style='font-weight:bold;'>" + m.ToString + "</span>")
                    ElseIf m.Groups("var2").Success OrElse m.Groups("var").Success Then
                        sb.Append("<span style='color:#956733;'>" + m.ToString + "</span>") ' dark blue
                        'ElseIf m.ToString = "@" Then
                        'sb.Append("<span id=""row""></span>")
                    ElseIf instr("[]", m.ToString) Then
                        sb.Append("<span style='color:#c0c0c0;'>" + m.ToString + "</span>")
                    Else
                        sb.Append("<span style='color:black;'>" + m.ToString + "</span>") ' dark blue
                    End If
                End If
            Next
            sb.Append("</span>")
            'lblInput.Text = sb.ToString
        Catch ex As Exception

        End Try
        Return sb.ToString
    End Function

    Private Sub btnStandard_Click(sender As Object, e As EventArgs) Handles btnStandard.Click
        Calculate()
        Dim img As bitmap = Nothing
        If RPN_Stack.ToImage(img, vTkn, vNames, vValues) Then
            Dim ms As MemoryStream = New MemoryStream()
            img.Save(ms, Imaging.ImageFormat.Bmp)
            imgStandard.ImageUrl = "data:image/png;base64," +
                Convert.ToBase64String(ms.ToArray(), 0, ms.ToArray().Length)
        End If
    End Sub

End Class