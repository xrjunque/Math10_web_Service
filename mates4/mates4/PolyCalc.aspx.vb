Imports System.Threading
Imports System.IO
Imports System.Runtime.Serialization
Imports System.Runtime.Serialization.Formatters

Partial Public Class Polycalc
    Inherits System.Web.UI.Page
    'Implements ICallbackEventHandler


#Region "Dims"
    Dim rnd As New Random(1)
    Dim nID As Int32
    Dim n As Int32 = 1
    Dim bLoaded As Boolean
    Dim factor As Single = 1.0

    Dim iCurGo2, lstGo2 As Int32
    Dim vN(-1) As Int32
    Dim sTbVar(-1) As String
    Dim sTbQry(-1), vsResultGo2(-1), vOptions(-1) As String


    Dim xi, xf, yi, yf As Double
    Dim sfn, sf2, sf3, yAuto As String, xAxis, yAxis As Double



    Dim sToday As String = C_SessionWeb.sCurrVersion



    Dim mP As New matrixParser
    Dim cfg As Config = Config.cfg
    Dim spacesMatch As Match
    Public Delegate Sub parseDelegate(
                ByVal sExpression As String,
                ByVal sVars As String,
                ByRef vars As VarsAndFns,
                ByRef cfg As Config)
    Dim bDoCalculate As Boolean = True
    Dim sComments() As String
    Dim vNames() As String, vValues() As Double
#End Region


    Private Sub Page_PreRender(sender As Object, e As EventArgs) Handles Me.PreRender
        Dim sLitResponse As String = String.Empty
        Try
            btnHelp.PostBackUrl = "download.aspx?file=mates8users"
            btnHelp4.PostBackUrl = "download.aspx?file=mates8usuario"
            linkHelp2.PostBackUrl = "download.aspx?file=mates8usershtm"
            C_sessionWeb.contador(
                Page,
                Request,
                Response)
        Catch ex2 As Exception

        End Try
        Try
            nID = C_sessionWeb.getID(Page)
            C_sessionWeb.sProvieneDe(nID) = Request.Url.AbsoluteUri
            If rbI.Checked Then
                cfg.sImg = "i" ' set 'i' for imaginary symbol
            Else
                cfg.sImg = "j" ' set 'j' for imaginary symbol
            End If

            Dim bUploaded As Boolean = False
            If IsPostBack AndAlso FileUpload1.HasFile Then
                If Me.UploadBtn_Click(Nothing, Nothing) Then
                    bUploaded = True
                End If
            End If

            If Not IsPostBack AndAlso Not bUploaded Then
                Dim tb1 As String = Request.QueryString("tb1")
                Dim tb3 As String = Request.QueryString("tb3")
                Dim sRnd As String = Request.QueryString("rnd")
                Dim sFrac As String = Request.QueryString("frac")
                Dim sIgnoreSpaces As String = Request.QueryString("ignoreSpaces")
                If tb1 Is Nothing Then tb1 = ""
                If tb3 Is Nothing Then tb3 = ""
                If sRnd Is Nothing Then sRnd = ""
                If sFrac Is Nothing Then sFrac = ""
                If sIgnoreSpaces Is Nothing Then sIgnoreSpaces = ""
                If tb3.Length Then
                    Me.tbVarsWithName.Text = tb3
                End If
                If sRnd.Length Then
                    Boolean.TryParse(sRnd, chkRound.Checked)
                End If
                If sFrac.Length Then
                    Boolean.TryParse(sFrac, chkFractions.Checked)
                End If
                If sIgnoreSpaces.Length Then
                    Boolean.TryParse(sIgnoreSpaces, chkIgnoreSpaces.Checked)
                End If
                If tb1.Length Then
                    Me.tbQuery.Text = tb1
                    CALCULATE(False)
                    Exit Try
                End If
                'tbQuery.Text = "P(x) = 0"
                'tbVarsWithName.Text = "P(x) = x^3 -6*x^2 +11*x -6"
                'tbVarsWithName.Text = "Xc=50Ω|f=10e6Hz|Hz=1 // fundamental unit=1|Ω=1 // fundamental unit=1|1pF=1e-12F // 1pFarad = 1e-12 Farads|F=1 // fundamental unit=1"
                'tbVarsWithName.Text = Replace(tbVarsWithName.Text, "|", vbCrLf)
                'tbQuery.Text = "Xc=1/(2*pi*f*C(pF)) // get C in picoFarads"

                litResponse.Text = ""
                Me.Form.Attributes.Add("onkeydown",
                                     "var KeyID = (window.event) ? event.keyCode : e.keyCode;if(KeyID==119) document.forms[0].elements['" + Me.btnCalc.ClientID + "'].click();")

                'tbQuery.Attributes.Add("onfocus", _
                '                       "this.value='';" + tbVarsWithName.ID + ".value='';this.onfocus='';" + tbVarsWithName.ID + ".onfocus=''")
                'tbVarsWithName.Attributes.Add("onfocus", _
                '                       "this.value='';" + tbQuery.ID + ".value='';this.onfocus='';" + tbQuery.ID + ".onfocus=''")

            Else
                'tbQuery.Attributes.Add("onfocus", "'';")
                'tbVarsWithName.Attributes.Add("onfocus", "'';")
            End If
            'If chkVar1Char.Checked Then
            '    chkNumInVar.Checked = False
            'End If

        Catch ex As Exception

        End Try
        Try
            If Not IsPostBack Then
                populateFns()
            End If
            If True Then
                Dim e1 As String =
                            "function AddText(ctrl,str) " + vbCrLf +
                            "{" + vbCrLf +
                            " ctrl.value+=str;" + vbCrLf +
                            "}" + vbCrLf
                ClientScript.RegisterStartupScript(GetType(String), "addText", e1, True)
                e1 =
                "function clearText(ctrl) " + vbCrLf +
                "{" + vbCrLf +
                " ctrl.value='';" + vbCrLf +
                "}" + vbCrLf
                ClientScript.RegisterStartupScript(GetType(String), "clearText", e1, True)
                e1 =
                "function CE(ctrl) " + vbCrLf +
                "{" + vbCrLf +
                " if(ctrl.value.length==1) ctrl.value='';" + vbCrLf +
                " else{if(ctrl.value.length>1) ctrl.value=ctrl.value.substring(0,ctrl.value.length-1);}" + vbCrLf +
                "}" + vbCrLf
                ClientScript.RegisterClientScriptBlock(GetType(String), "CE", e1, True)

                Dim vCtr() As Control = {btnIntegral, btnLP, btnRP, btnOneHalf, btnCE,
                                        btnx, btnPow, btnDiv, btnMult, btnMinus,
                                        btnSqrt, btn7, btn8, btn9, btnPlus,
                                        btnHexa, btn4, btn5, btn6, btnEqual,
                                        btnOctal, btn1, btn2, btn3, btnEqualZero,
                                        btnBin, btn0, btnDot, btnExp, btnPi}
                For Each ctr In vCtr
                    CType(ctr, WebControl).Attributes.Add(
                        "onclick", "AddText(" + tbQuery.ClientID + ",'" +
                        CType(ctr, Button).Text + "');return false;")
                    CType(ctr, WebControl).Attributes.Add("style",
                        "font-family : Calibri; font-size: medium; width:35px; height:30px; text-align:center; vertical-align: middle;")
                    'Dim title As String = CType(ctr, WebControl).Attributes.Item("title")
                    'If title = "" Then
                    '    CType(ctr, WebControl).Attributes.Add("title", CType(ctr, Button).Text)
                    'End If
                Next

                btnCE.Attributes.Add("onclick", "CE(" + tbQuery.ClientID + ");return false;")
                btnClearQuery.Attributes.Add("onclick", "clearText(" + tbQuery.ClientID + ");return false;")
                btnClearVars.Attributes.Add("onclick", "clearText(" + tbVarsWithName.ClientID + ");return false;")
                btnClearBoth.Attributes.Add("onclick", "clearText(" + tbQuery.ClientID + ");clearText(" +
                                tbVarsWithName.ClientID + ");return false;")
                vCtr = New Control() {btnClearQuery, btnClearVars, btnClearBoth}
                For Each ctr In vCtr
                    CType(ctr, WebControl).Attributes.Add("style",
                        "font-family : Calibri; font-size: medium; height:30px; text-align:center; vertical-align: middle;")
                Next

                e1 =
                "function AddSelectedText(ctr1,ctr2) " + vbCrLf +
                "{" + vbCrLf +
                " var i=ctr1.selectedIndex;" + vbCrLf +
                " if(i>0) AddText(ctr2,ctr1.options[i].value+'(');" + vbCrLf +
                "}" + vbCrLf
                ClientScript.RegisterStartupScript(GetType(String), "AddSelectedText", e1, True)
                cbFns.Attributes.Add("onchange", "AddSelectedText(this," + tbQuery.ClientID +
                        ");return false;")


                'Dim cs As ClientScriptManager = Page.ClientScript
                'Dim cbReference As String = cs.GetCallbackEventReference( _
                '     "'" + Page.UniqueID + "'", "arg", "ReceiveServerData", "", _
                '    "ProcessCallBackError", False)
                'Dim callbackScript As String = "function CallTheServer(arg, context) {" + _
                '    cbReference + "; }"
                'cs.RegisterClientScriptBlock(Me.GetType(), "CallTheServer", _
                '    callbackScript, True)

                'e1 = _
                '"function LBHistVarsOnChange(ctr1,ctr2) " + vbCrLf + _
                '"{" + vbCrLf + _
                '" var i=ctr1.selectedIndex;" + vbCrLf + _
                '" CallTheServer('HISTVARS|'+i, null )" + vbCrLf + _
                '"}" + vbCrLf
                'ClientScript.RegisterStartupScript(GetType(String), "LBHistVarsOnChange", e1, True)
                'DropHistVars1.Attributes.Add("onchange", "LBHistVarsOnChange(this,null" + _
                '        ");return false;")

                'e1 = _
                '"function LBHistQueryOnChange(ctr1,ctr2) " + vbCrLf + _
                '"{" + vbCrLf + _
                '" var i=ctr1.selectedIndex;" + vbCrLf + _
                '" CallTheServer('HISTQUERY|'+i, null )" + vbCrLf + _
                '"}" + vbCrLf
                'ClientScript.RegisterStartupScript(GetType(String), "LBHistQueryOnChange", e1, True)
                'DropHistQuery1.Attributes.Add("onchange", "LBHistQueryOnChange(this,null" + _
                '        ");return false;")


                '                function chgVisibility(ctr1,ctr2) {
                ' if(ctr1 == 'Panel1') ctr1 = document.getElementById("<% = Panel1.ClientID %>");
                ' else ctr1 = document.getElementById("<% = Panel2.ClientID %>");
                ' if(ctr1.display=='hidden') {this.display='visible';}
                ' else {ctr1.display='hidden';}
                '}

                'e1 = _
                '"function chgVisibility(ctr1,ctr2) " + vbCrLf + _
                '"{" + vbCrLf + _
                '" CallTheServer('HISTVARS|'+i, null )" + vbCrLf + _
                '"}" + vbCrLf
                'ClientScript.RegisterStartupScript(GetType(String), "chgVisibility", e1, True)
                'btnVarsHist.Attributes.Add("onclick", "")
                'btnExprHist.Attributes.Add("onclick", "")

                'btnExprHist.Attributes.Add("onclick", "CallTheServer('Panel|1',null);" + _
                '        "return false;")
                'btnVarsHist.Attributes.Add("onclick", "CallTheServer('Panel|2',null);" + _
                '        "return false;")
            End If
            Inicio("en")
        Catch ex As Exception

        End Try
    End Sub

#Region "events"

    Sub populateFnsOLD()
        Try
            Dim i As Int32
            Dim vCnts(MathGlobal8.vCntsCaseSen.Length - 1) As String
            Array.Copy(MathGlobal8.vCntsCaseSen, vCnts, vCnts.Length)
            i = vCnts.Length
            ReDim Preserve vCnts(i + MathGlobal8.vCntsCaseNonSen.Length - 1)
            Array.Copy(MathGlobal8.vCntsCaseNonSen, 0,
                       vCnts, i, MathGlobal8.vCntsCaseNonSen.Length)
            Array.Sort(vCnts)
            cbFns.Items.Clear()
            cbFns.Items.Add("Functions:")
            'For i = 0 To vCnts.Length - 1
            '    cbFns.Items.Add(vCnts(i)) ' load constants into the combo 
            'Next
            'cbFns.Items.Add("------")

            Dim vFn(MathGlobal8.vFn.Length - 1) As String
            Array.Copy(MathGlobal8.vFn, vFn, vFn.Length)
            Array.Sort(vFn)
            For i = 0 To vFn.Length - 1
                cbFns.Items.Add(vFn(i)) ' load functions into the combobox
            Next
        Catch ex As Exception

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
                If LCase(vFn(i)) <> "diff" Then
                    i0(j) = CType(MathGlobal8.vFnType(i), Int32)
                    If MathGlobal8.vFnType(i) <> MathGlobal8.FnType.other Then
                        i1(j) = Chr(MathGlobal8.vFnType(i) + 48) + vFn(i)
                    Else
                        i1(j) = Chr(100) + vFn(i)
                    End If
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
                If e1 = "---" Then Exit Sub
                If e1.Chars(0) = "-" OrElse e1.Chars(0) = "(" OrElse Len(e1) = 0 Then
                    Exit Sub
                End If
                If e1 <> "pi" AndAlso e1 <> "e" Then
                    e1 += "()"
                End If
                tbQuery.Text += e1
                'Me.setSession(False, sFile) ' override avoiding duplicated entries in sTbQry() and sTbVar()
                'saveHistory()
            End If
        Catch ex As Exception

        End Try
    End Sub
    Private Sub rbIJ_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) _
        Handles rbI.CheckedChanged, rbJ.CheckedChanged
        Static bEnUso As Boolean = False
        If sender.text = "" Then
            Exit Sub
        End If
        If bEnUso Then Exit Sub
        bEnUso = True
        Try
            If InStr(LCase(sender.text), "i") Then
                If sender.checked Then
                    rbJ.Checked = False
                Else
                    rbJ.Checked = True
                End If
            Else
                If sender.checked Then
                    rbI.Checked = False
                Else
                    rbI.Checked = True
                End If
            End If
            'errClr()
            If rbI.Checked Then
                cfg.sImg = "i" ' set 'i' for imaginary symbol
            Else
                cfg.sImg = "j" ' set 'j' for imaginary symbol
            End If
        Catch ex As Exception
        Finally
            bEnUso = False
        End Try
    End Sub

    Protected Sub btnCalc_Click(ByVal sender As Object, ByVal e As EventArgs) Handles btnCalc.Click
        'If num.Value = 0 Then
        '    If Mid(tbQuery.Text, 1, 3) <> "Xc=" Then
        '        num.Value += 1
        '        chkVar1Char.Checked = True
        '        tbVarsWithName.Text = ""
        '    End If
        'End If
        If sender IsNot Nothing Then
            Me.CALCULATE(True)
        Else
            Me.CALCULATE(False)
        End If
        If num.Value = 1 Then
            chkVar1Char.Checked = True
        End If
    End Sub
    Sub errMsg(ByVal sErr As String)
        If Response.IsClientConnected = False Then
            Exit Sub
        End If
        litResponse.Text = "Message: " + sErr
    End Sub
    Sub errMsgClear()
        If Response.IsClientConnected = False Then
            Exit Sub
        End If
        litResponse.Text = ""
    End Sub

    Protected Sub cbExamples_SelectedIndexChanged(ByVal sender As Object, ByVal e As EventArgs) Handles cbExamples.SelectedIndexChanged
        Try
            errMsgClear()
            If cbExamples.SelectedIndex > -1 Then
                'chkIgnoreCase.Checked = False
                'Dim e1 As String = ""

                'e1 = cbExamples.Items(cbExamples.SelectedIndex).Text
                'If cbExamples.SelectedIndex >= 11 Then
                '    e1 = Replace(e1, " ", ";")
                'End If
                'e1 = Replace(e1, "|", vbCrLf)
                'Dim e2() As String = Split(e1, "@")
                'Me.tbQuery.Text = e2(0)
                'tbVarsWithName.Text = ""
                'If e2.Length > 1 Then
                '    For i As Int32 = 1 To e2.Length - 1
                '        tbVarsWithName.Text += e2(i) + vbCrLf
                '    Next
                'End If

                If cbExamples.SelectedIndex > 0 Then

                    'Dim sFile As String = readHistory()
                    'Me.setSession(False, sFile)
                    Dim cfg1 As New Config(cfg)

                    Dim e1 As String = ""
                    e1 = cbExamples.Items(cbExamples.SelectedIndex).Text
                    If Len(e1) > 3 AndAlso
                    Mid(e1, 1, 3) = "---" Then
                        Exit Sub
                    End If
                    e1 = Replace(e1, "&amp;", "&")
                    Dim e3() As String = Split(e1, "?")
                    tbQuery.Text = e3(0)
                    tbVarsWithName.Text = ""
                    If e3.Length > 1 Then
                        For i As Int32 = 1 To e3.Length - 1
                            tbVarsWithName.Text += e3(i)
                        Next
                        tbVarsWithName.Text = Replace(tbVarsWithName.Text, "|", vbCrLf)
                    End If
                    'e1 = Replace(e1, "'", "//")
                    'sComments = Split(e1, "//")
                    'e1 = sComments(0)

                    chkIgnoreSpaces.Checked = False
                    chkCaseSensitive.Checked = True
                    chkNumInVar.Checked = False
                    Dim bCharLen1 As Boolean = chkVar1Char.Checked
                    If InStr(e1, "Xc") Then
                        chkVar1Char.Checked = False
                    Else
                        chkVar1Char.Checked = True
                    End If
                    If InStr(e1, "&h") Then
                        chkOctal.Checked = True
                        chkBinary.Checked = True
                        chkHexa.Checked = True
                        chkIgnoreSpaces.Checked = True
                    Else
                        chkOctal.Checked = False
                        chkBinary.Checked = False
                        chkHexa.Checked = False
                        e1 = Replace(e1, ",", ";")
                    End If
                    C_sessionWeb.nCurSolution(nID) = 0
                    btnCalc_Click(Nothing, Nothing)
                    C_SessionWeb.nCurSolution(nID) = 0
                    cfg = New Config(cfg1)
                    Me.chkVar1Char.Checked = bCharLen1
                    'setSession(False, sFile)
                    'saveHistory()
                End If

            End If
        Catch ex As Exception
            'updateStatusBar(ex.Message)
            errMsg(ex.ToString)
        End Try
    End Sub



    Protected Sub DropHist_SelectedIndexChanged(ByVal sender As Object, ByVal e As EventArgs) Handles DropHist.SelectedIndexChanged
        Try
            Dim i As Int32 = DropHist.SelectedIndex
            If i > 0 Then
                readHistory()
                Me.tbQuery.Text = Me.sTbQry(i - 1)
                Me.tbVarsWithName.Text = Me.sTbVar(i - 1)
                parseOptions(vOptions(i - 1), False)
                CALCULATE(False)
                'Me.setSession(False, sFile) ' override avoiding duplicated entries in sTbQry() and sTbVar()
                'saveHistory()
            End If
        Catch ex As Exception

        End Try
    End Sub

    Private Sub tbQueryValidator_ServerValidate(ByVal source As Object, ByVal args As System.Web.UI.WebControls.ServerValidateEventArgs) Handles tbQueryValidator.ServerValidate
        Try
            If Not Me.bDoCalculate Then
                Exit Try
            End If
            args.IsValid = True
            If Not Me.chkIgnoreSpaces.Checked Then
                Dim e1 As String = tbQuery.Text
                removeComments(e1)
                e1 = Replace(e1, "|", vbCrLf)
                If Len(e1) Then
                    ' supress leading and trailing white spaces:
                    e1 = Regex.Replace(e1, "(^(\+|\s)+)|((" +
                             MathGlobal8.sCol + "|" + MathGlobal8.sRow + "|\s+)$)", "")
                    If Len(e1) = 0 Then
                        Exit Try
                    End If
                    sComments = Split(e1, "//")
                    e1 = sComments(0)
                    Me.spacesMatch = Regex.Match(e1,
                        "f\(x\)=|P\(x\)=|y=|Q\(x\)=|g\(x\)=|h\(x\)=",
                        RegexOptions.IgnoreCase Or RegexOptions.IgnorePatternWhitespace)
                    Dim posAt As Int32 = InStr(e1, "@") - 1
                    If Me.spacesMatch.Success AndAlso
                    Me.spacesMatch.Index = 0 AndAlso
                    (posAt = -1 OrElse posAt > Me.spacesMatch.Index) Then
                        With tbQueryValidator
                            .ErrorMessage =
                                "Please, don't define functions (as '" +
                                Replace(Me.spacesMatch.ToString, "=", "") +
                                "') in left box: the result is unpredictable; use right side box, under 'Vars. & Fns'."
                            args.IsValid = False
                            Me.bDoCalculate = False
                            Exit Try
                        End With
                    End If
                End If
            End If
            If chkVar1Char.Checked Then
                If chkNumInVar.Checked Then
                    With tbQueryValidator
                        chkNumInVar.Checked = False
                        .ErrorMessage =
                            "A variable's name of 1 character can't be a number."
                        args.IsValid = False
                    End With
                End If
            End If
        Catch ex As Exception

        End Try
    End Sub


    'Private Sub btnIntegral_Click(ByVal sender As Object, ByVal e As System.EventArgs) _
    '    Handles btnIntegral.Click, btnHexa.Click, btnOctal.Click, btnBinary.Click, _
    '    btnx.Click, btnx2.Click, btnx3.Click, btnx4.Click, btnSqrt.Click
    '    Try
    '        'Me.tbQuery.Text += sender.text
    '        'Me.tbQuery.Focus()
    '        Me.bDoCalculate = False
    '        '    Dim e1 As String = _
    '        '"function SetCursorToTextEnd(ctrl) " + vbCrLf + _
    '        '"{" + vbCrLf + _
    '        '" if(ctrl.setSelectionRange) {" + vbCrLf + _
    '        '" ctrl.setSelectionRange(ctrl.value.length, ctrl.value.length);" + vbCrLf + _
    '        '" }" + vbCrLf + _
    '        '" else if (ctrl.createTextRange) {" + vbCrLf + _
    '        '" var range = ctrl.createTextRange();" + vbCrLf + _
    '        '" range.moveStart('character', ctrl.value.length);" + vbCrLf + _
    '        '" range.select();" + vbCrLf + _
    '        '"// ctrl.focus();" + vbCrLf + _
    '        '" }        " + vbCrLf + _
    '        '"}" + vbCrLf
    '        '    ClientScript.RegisterStartupScript(GetType(String), "caret", e1, True)
    '        'Me.tbQuery.Attributes.Add("onfocus", "SetCursorToTextEnd(this)")
    '    Catch ex As Exception

    '    End Try
    'End Sub

    Private Sub chkVarAndNums_CheckedChanged(ByVal sender As Object, ByVal e As System.EventArgs) _
    Handles chkVar1Char.CheckedChanged, chkNumInVar.CheckedChanged
        If chkVar1Char.Checked Then
            chkNumInVar.Checked = False
        End If
    End Sub


#End Region

#Region "Calculate"
    Private Sub CALCULATE(bSaveHistoria As Boolean)
        Try
            If Response.IsClientConnected = False Then
                Exit Sub
            End If
            btnCalc.Enabled = False

            CALCULATE1(bSaveHistoria)
        Catch ex As Exception
        Finally
            If Response.IsClientConnected Then
                btnCalc.Enabled = True
            End If
            Try
                'Session.Abandon()
                'Response.Cookies.Add(New HttpCookie("ASP.NET_SessionId", ""))
            Catch ex As Exception

            End Try
        End Try
    End Sub
    Private Sub CALCULATE1(bSaveHistoria As Boolean)

        Dim e1 As String = ""
        Dim sResult As String = ""
        Try
            Try
                'Page.Session.Timeout = 30
                HttpContext.Current.Server.ScriptTimeout = 30
                Page.Server.ScriptTimeout = 30

            Catch ex As Exception

            End Try

            If Not Me.bDoCalculate Then
                Exit Sub
            End If
            'Dim i As Int32
            bLoaded = False
            'updateStatusBar("")
            bLoaded = True


            'rtfU1.getFontNames(rtbQuery.Rtf)
            'rtfU2.getFontNames(rtbVars.Rtf)

            e1 = Trim(tbQuery.Text)
            If Len(e1) = 0 Then
                If Response.IsClientConnected = False Then
                    Exit Sub
                End If
                litResponse.Text = msg8.num(67)
                Exit Sub
            End If
            e1 = Replace(e1, vbLf, vbCrLf)
            e1 = Replace(e1, vbCrLf + vbCrLf, vbCrLf)
            Dim vC() As String = {Chr(&HBC), Chr(&HBD), Chr(&HBE), Chr(&HB2), Chr(&HB3),
                        ChrW(&H2212), ChrW(&H3C0), "〖", "〗", "⅞", "**", "",
                        "П", "⁹", "⁸", "⁷", "⁶", "⁵", "⁴", "³", "²",
                        "−", "×", "√"}
            Dim e1c As New StringBuilder(Len(e1))
            For i1 As Int32 = 0 To Len(e1) - 1
                Dim c As String = e1.Chars(i1)
                If "!" <= e1.Chars(i1) AndAlso e1.Chars(i1) <= "}" Then
                    e1c.Append(e1.Chars(i1))
                ElseIf Array.IndexOf(vC, c) > -1 Then
                    e1c.Append(e1.Chars(i1))
                ElseIf InStr(MathGlobal8.sGreek, c) Then
                    e1c.Append(e1.Chars(i1))
                ElseIf e1.Chars(i1) = Chr(13) OrElse e1.Chars(i1) = Chr(10) Then
                    e1c.Append(e1.Chars(i1))
                End If
            Next
            e1 = e1c.ToString
            'Dim mc As MatchCollection = Regex.Matches(e1, MathGlobal8.sComment)
            Dim e1b As String = e1 ' Regex.Replace(e1, MathGlobal8.sComment, "")

            'sComments = Split(e1b, "'")
            'e1b = sComments(0)
            Dim strQuery As String = e1b
            'Dim e2 As String = Replace(rtbVars.Text, "//", "'")
            'e2 = removeComments(rtbVars.Text)
            Dim e2 As String = tbVarsWithName.Text
            'If Len(e2) Then
            '    e2 = Replace(e2, "//", "'")
            '    e2 = Regex.Replace(e2, _
            '    MathGlobal8.sComment, "")
            'End If
            e1c = New StringBuilder(Len(e2))
            For i1 As Int32 = 0 To Len(e2) - 1
                If "!" <= e2.Chars(i1) AndAlso e2.Chars(i1) <= "}" Then
                    e1c.Append(e2.Chars(i1))
                ElseIf Array.IndexOf(vC, e2.Chars(i1)) > -1 Then
                    e1c.Append(e2.Chars(i1))
                ElseIf InStr(MathGlobal8.sGreek, e2.Chars(i1)) Then
                    e1c.Append(e2.Chars(i1))
                ElseIf e2.Chars(i1) = Chr(13) OrElse e2.Chars(i1) = Chr(10) Then
                    e1c.Append(e2.Chars(i1))
                End If
            Next
            e2 = e1c.ToString

            Dim strVarsAndFns As String = e2



            ' Set the configuration:

            ' The following settings have to do
            ' with how the input data will be parsed:

            cfg.bCaseSensitive = chkCaseSensitive.Checked
            cfg.bIgnoreSpaces = chkIgnoreSpaces.Checked

            ' The next configuration settings affect
            ' on how the output data will be retrieved:
            cfg.bRounding = chkRound.Checked
            cfg.bEngNotation = chkENG.Checked
            cfg.bFractions = chkFractions.Checked
            cfg.bDetail = chkDetail.Checked
            cfg.Base = MathGlobal8.outputBase.decimal
            If chkVar1Char.Checked Then
                chkNumInVar.Checked = False
            End If
            cfg.bVarName1Char = chkVar1Char.Checked
            cfg.bNumsInVarName = chkNumInVar.Checked
            cfg.sesID = nID
            cfg.timeOutms = 30 * 1000
            cfg.doTimeOut = timeout.whenTimeIsOver
            'cfg.bUseUnits = chkUnits.Checked

            Dim oVars As VarsAndFns = Nothing

            ' INPUT for matrixParser.Parse()
            ' ==============================
            ' strQuery = the math expression to parse,
            ' for example: strQuery="2*2", "2*x+3*x", "∫(cos(x))dx", "roots(x^16-1)"
            '              or a matrix expression with columns delimited by
            '              semicolons and rows by vbCrLf as "A^-1"
            ' strVarsAndFns = "" or eventualy variables values or functions
            '                for ex. "x=-1" or  "A=2;3" + vbCrLf + "-1;2"
            ' Dim oVars As VarsAndFns = Nothing

            ' OUTPUT:
            ' =======
            ' 1) mP.toString returns the result as a string.
            ' 2) mP.retCjo() returns a complex or, eventually, an array of complex.
            ' 3) When the result is a matrix
            '   xmP.ret.exprMtx.getExpr(row, column) returns the expression
            '   contained at a row and column ((0,0) is the first row and columns)
            '   
            '   mP.ret.exprMtx.getExpr(row, column).IsReal will tell
            '   if the element's content is a real number and 
            '   mP.ret.exprMtx.getExpr(row, column).toDouble its value.
            '   mP.ret.exprMtx.rows gives the number of rows in the matrix
            '   mP.ret.exprMtx.cols gives the # of columns

            ' Example 1. We want the roots of x^16-1 and for that purpose
            ' we equal strQuery="roots(x^16-1)", execute
            ' mP = matrixParser.parse(strQuery,"", nothing)
            ' and, at the output, the roots will be in mP.retCjo(); first, the real
            ' roots (if any) and then the complex (if any):
            ' 
            ' root1:  mp.retCjo(0)  ' = -1 (real)
            ' root2:  mp.retCjo(1)  ' =  1 (real)
            ' root2:  mp.retCjo(2)  ' = -i (complex)
            ' ...
            ' root16: mp.retCjo(15) ' = (0.923879532511287 -i*0.38268343236509) (complex)

            ' Real roots, in mP.retCjo(), are ordered from most negative to most positive.
            ' If a root is real and not complex, i.e. the imaginary value is zero, 
            ' mP.retCjo(0).IsReal will be True.


            'Dim e2 As String = removeComments(tbVarsWithName.Text)
            'If Not chkCaseSensitive.Checked Then
            '    e2 = LCase(e2)
            'End If


            Dim ts As New TimeSpan(Now.Ticks)
            Dim ts2 As TimeSpan
            cfg.ticksStart = ts.Ticks


            'Dim strQuery As String = e1
            'Dim strVarsAndFns As String = e2

            sResult = ""
            For ioutput = 1 To 4
                If ioutput = 1 Then
                ElseIf ioutput = 2 Then
                    If chkHexa.Checked Then
                        cfg.Base = MathGlobal8.outputBase.hexadecimal
                    Else
                        GoTo sig_i_output
                    End If
                ElseIf ioutput = 3 Then
                    If chkOctal.Checked Then
                        cfg.Base = MathGlobal8.outputBase.octal
                    Else
                        GoTo sig_i_output
                    End If
                ElseIf ioutput = 4 Then
                    If chkBinary.Checked Then
                        cfg.Base = MathGlobal8.outputBase.binary
                    Else
                        GoTo sig_i_output
                    End If
                End If

                mP.cfg = cfg
                mP.cfg.doTimeOut = timeout.whenTimeIsOver

                Dim bSkipErr As Boolean = False
                If True Then ' set to 'False' for debugging purposes 
                    Dim bDone As Boolean = False
                    Dim oParse As New matrixParser.parseObject(
                        mP, strQuery, strVarsAndFns, oVars, cfg)
                    Dim th As New System.Threading.Thread(
                      New Threading.ParameterizedThreadStart(AddressOf oParse.parse))
                    Dim ts3 As New TimeSpan(Now.Ticks + cfg.timeOutms * 10 ^ 4)
                    ts2 = New TimeSpan(Now.Ticks - ts.Ticks)
                    th.Start(mP)
                    Do While Not th.Join(New TimeSpan(50))
                        ts2 = New TimeSpan(Now.Ticks - ts.Ticks)
                        Dim ts4 As New TimeSpan(ts3.Ticks - Now.Ticks)
                        If ts4.TotalSeconds < 0 OrElse Response.IsClientConnected = False Then
                            Config.cancelID(mP.cfg.ID)
                            'cfg.doTimeOut = timeout.never
                            System.Threading.Thread.Sleep(0)
                            System.Threading.Thread.Sleep(2000)
                            System.Threading.Thread.Sleep(0)
                            If mP.ret Is Nothing AndAlso
                            (mP.retCjo Is Nothing OrElse mP.retCjo.Length = 0) Then
                                sResult = msg8.num(44) + vbCrLf
                                th.Abort()
                                Exit Try
                            End If
                            th.Abort()
                            GoTo sigue ' porque mp.ret no es nothing
                            'Exit Do
                        End If
                        System.Threading.Thread.Sleep(0)
                    Loop
                    If Not th.Join(cfg.timeOutms * 2.2) Then
                        System.Threading.Thread.Sleep(200)
                        th.Abort()
                        System.Threading.Thread.Sleep(200)

                        sResult = msg8.num(44) + vbCrLf
                        DspCalc(strQuery, sResult, Me.n)
                        Exit Try
                    End If
                    ts2 = New TimeSpan(Now.Ticks - ts.Ticks)
                Else
                    For k As Int32 = 1 To 1
                        mP.parse(strQuery, strVarsAndFns, oVars, cfg)
                    Next
                    ts2 = New TimeSpan(Now.Ticks - ts.Ticks)
                End If

                If mP.errMsg IsNot Nothing AndAlso
                mP.errMsg.Length Then
                    sResult += mP.errMsg + vbCrLf
                    If (mP.msgHTML.Length AndAlso
                    mP.msgHTML <> mP.errMsg) OrElse
                    (mP.errMsg.Length = 0 AndAlso
                     mP.msgHTML.Length) Then
                        sResult += mP.msgHTML
                    End If
                    'updateStatusBar(mP.errMsg)
                    DspCalc(e1, sResult, Me.n)
                    Exit Try
                End If

                If mP.msgHTML.Length Then
                    sResult += mP.errMsg + vbCrLf
                    sResult += mP.msgHTML
                    'updateStatusBar(mP.errMsg)
                    DspCalc(e1, sResult, Me.n)
                    Exit Try
                End If
sigue:
                cfg.doTimeOut = timeout.never
                System.Threading.Thread.Sleep(0)

                If ioutput = 1 Then
                ElseIf ioutput = 2 Then
                    If chkHexa.Checked Then
                        cfg.Base = MathGlobal8.outputBase.hexadecimal
                        sResult += "<br /><h4>Hexadecimal:</h4>"
                    Else
                        GoTo sig_i_output
                    End If
                ElseIf ioutput = 3 Then
                    If chkOctal.Checked Then
                        cfg.Base = MathGlobal8.outputBase.octal
                        sResult += "<br /><h4>Octal:</h4>"
                    Else
                        GoTo sig_i_output
                    End If
                ElseIf ioutput = 4 Then
                    If chkBinary.Checked Then
                        cfg.Base = MathGlobal8.outputBase.binary
                        sResult += "<br /><h4>Binary:</h4>"
                    Else
                        GoTo sig_i_output
                    End If
                End If
                Dim seRet As String = ""
                Dim bIsEq As Boolean = False
                If mP.ret IsNot Nothing AndAlso
                mP.ret.getParser IsNot Nothing Then
                    bIsEq = mP.ret.getParser.cur.bIsEquation
                End If
                Dim bIsPoly As Boolean = False
                Dim bIsReal As Boolean = False
                If mP.ret.curExpr IsNot Nothing Then
                    bIsPoly = mP.ret.curExpr.IsPolynomial
                    bIsReal = mP.ret.curExpr.IsReal
                End If
                Dim bIsMtx As Boolean = False
                If mP.ret.exprMtx.Cols + mP.ret.exprMtx.Rows <> 2 Then
                    bIsMtx = True
                    If bIsEq AndAlso mP.ret.exprMtx.Cols <> 1 Then
                        sResult += msg8.num(50) ' found columns, expected a equation 
                        'updateStatusBar(sResult)
                        DspCalc(e1, sResult, Me.n)
                        Exit Try
                    End If
                End If
                bIsReal = mP.ret.curExpr.IsReal
                sResult += mP.ToString(cfg)
                Dim bOneOrMoreVars As Boolean = (mP.ret.exprMtx.getAllVars.Length >= 1)
                Dim bOneVar As Boolean = (mP.ret.exprMtx.getAllVars.Length = 1)
                If bIsEq AndAlso bOneVar Then
                    seRet += " = 0"
                End If
                Dim sRows() As String = Regex.Split(
                    seRet, MathGlobal8.sRow)
                Dim sCols() As String = Split(
                    Replace(sRows(0), ";", vbTab), vbTab)
                If mP.ret.curExpr.IsPolynomial AndAlso bOneVar AndAlso
                mP.ret.curExpr.getPolynomial.PolyResto Is Nothing Then
                    Dim Pa As Polynomial = mP.ret.curExpr.getPolynomial
                    Dim vsPa() As String = Split(Pa.ToStringFactors(cfg), "=")
                    C_sessionWeb.sfn(nID) = Pa.toStringPoly(cfg)
                    C_sessionWeb.sf2(nID) = vsPa(0)
                    C_sessionWeb.sf3(nID) = "0"
                    C_sessionWeb.yAuto(nID) = 1
                    Dim min As Double = 1000
                    Dim max As Double = -1000
                    Dim cjo() As Complex = Nothing ' mP.soe.resultValues ' Complex.sortRoots( reOpRoots.cjo)
                    If Pa.roots IsNot Nothing Then
                        cjo = Pa.roots.cjo
                    Else
                        cjo = mP.retCjo
                    End If
                    If cjo.Length > 1 Then
                        For i = 0 To cjo.Length - 1
                            If cjo(i).IsReal Then
                                If min > cjo(i).pRe.ToDouble Then
                                    min = cjo(i).pRe.ToDouble
                                End If
                                If max < cjo(i).pRe.ToDouble Then
                                    max = cjo(i).pRe.ToDouble
                                End If
                            End If
                        Next
                        If min < max Then
                            C_sessionWeb.xi(nID) = min
                            C_sessionWeb.xf(nID) = max
                        Else
                            C_sessionWeb.xi(nID) = min - 10
                            C_sessionWeb.xf(nID) = min + 10
                        End If
                    End If
                End If
                If bIsEq AndAlso bOneOrMoreVars Then
                    If bOneVar AndAlso
                    cfg.bDetail Then
                        'seRet = cfg.oDetail.ToStringHTML("navy")
                        'seRet = cfg.cur.toStringDetailStack
                        'seRet = cfg.oDetail.ToStringHTML("navy")

                        Dim Pa As Polynomial = mP.ret.curExpr.getPolynomial
                        If Pa IsNot Nothing AndAlso Pa.PolyResto IsNot Nothing Then
                            seRet = "Detail:" + vbCrLf + Pa.toStringPoly(cfg) + " = 0  =>" + vbCrLf
                            Dim div As Polynomial = New Polynomial(Pa.PolyDivisor)
                            Dim resto As New Polynomial(Pa.PolyResto)
                            Pa.PolyResto = Nothing
                            Pa.PolyDivisor = Nothing
                            Pa *= div
                            Pa += resto
                            mP.ret.curExpr = New Expression(Pa)
                            seRet += Pa.toStringPoly(cfg) + " = 0" + vbCrLf
                            sResult += MathGlobal8.getTable(seRet, "navy")
                        ElseIf mP.soe IsNot Nothing Then
                            seRet = mP.soe.sErrMsg
                        End If
                    Else
                        seRet = cfg.oDetail.ToStringDivisionHTML("navy")
                    End If
                ElseIf bIsEq Then
                    If Not bIsReal OrElse mP.ret.curExpr.toDouble <> 0.0 Then
                        sResult += String.Format("<br /><h3>" + msg8.num(50) + "</h3>",
                                        mP.ret.curExpr.toDouble)
                    Else
                        sResult += MathGlobal8.getTable(seRet, "navy")
                        sResult += "<br /><h3>0 = 0</h3>"
                    End If
                Else
                    Dim common As Expression = Nothing
                    Dim expr As Expression = Nothing
                    Dim commonStr As String = ""
                    Dim Expr1 As Expression = Nothing
                    Dim CommExpr As Expression = Nothing
                    Dim commStr As String = ""
                    If Len(seRet) Then
                        sResult += MathGlobal8.getTable(seRet, "navy")
                    End If
                    'If mP.ret.curExpr.getCommonFactor(cfg, CommExpr, Expr1, commStr) Then
                    '    seRet = " = " + commStr + vbCrLf
                    '    sResult += MathGlobal8.getTable(seRet, "navy")
                    'End If
                    If cfg.bDetail AndAlso mP.ret.isMatrix Then
                        sResult += "<br />" + cfg.oDetail.ToStringDivisionHTML("navy")
                    ElseIf cfg.doTimeOut AndAlso Not mP.ret.curExpr.IsPolynomial Then
                        sResult += cfg.cur.toStringDetailStack
                    ElseIf cfg.bDetail Then

                        Dim vDetail() As String = Detall.ToStringDetail(
                            mP.getParser.cur, mP, cfg.oDetail)
                        Dim sDetail As String = Join(vDetail, vbCrLf)
                        sResult += "<br />Detail: <br />"
                        'sResult += MathGlobal8.getTable(sDetail, "")
                        sResult += mP.cur.toStringDetailStack
                        If InStr(strQuery, "/") Then
                            sResult += "<br />" + cfg.oDetail.ToStringDivisionHTML("navy")
                        End If

                    End If
                End If

sig_i_output:
            Next
            'sResult += "<br>Time: " + Math.Floor(ts2.TotalMilliseconds).ToString + " ms"
            sResult += "<br>"


            DspCalc(e1, sResult, Me.n)
            'ReDim Preserve sTbQry(lstGo2), sTbVar(lstGo2), vsResultGo2(lstGo2), vN(lstGo2)
            'vN(lstGo2) = Me.n
            'sTbQry(lstGo2) = e1
            'sTbVar(lstGo2) = tbVarsWithName.Text
            'vsResultGo2(lstGo2) = sResult
            'iCurGo2 = lstGo2
            'lstGo2 += 1
        Catch ex As Exception
            If Response.IsClientConnected = False Then
                Exit Sub
            End If
            If InStr(ex.ToString, "#") Then
                Dim sMsg As String = ex.ToString
                Dim pos As Int32 = InStr(sMsg, "#")
                pos = InStr(pos, sMsg, ")")
                sMsg = Mid(sMsg, 1, pos)
                sMsg = Replace(sMsg, "System.Exception:", "")
                errMsg(Mid(sMsg, 1, pos))
            Else
                errMsg("n/a")
            End If
        End Try

        Try
            SyncLock "save"
                If Response.IsClientConnected = False Then
                    Exit Sub
                End If
                litResponse.Text = "<div style=""color:navy; margin-left:10px;"">" + DspCalc(e1, sResult, C_sessionWeb.nCurSolution(nID) + 1) + "</div>"
                If Len(Trim(e1)) = 0 OrElse Len(e1) > 1000 OrElse
                Len(tbVarsWithName.Text) > 1000 Then
                    Exit Sub
                End If
                If bSaveHistoria Then
                    readHistory() ' para obtener lstGo2
                    Me.n += 1
                    ReDim Preserve sTbQry(lstGo2), sTbVar(lstGo2), vsResultGo2(lstGo2), vN(lstGo2), vOptions(lstGo2)
                    vN(lstGo2) = nID = C_sessionWeb.getID(Page) + 1
                    sTbQry(lstGo2) = e1
                    sTbVar(lstGo2) = tbVarsWithName.Text
                    vOptions(lstGo2) = toStrOptions()
                    If sTbVar(lstGo2) = "" Then
                        sTbVar(lstGo2) = " "
                    End If
                    vsResultGo2(lstGo2) = sResult
                    iCurGo2 = lstGo2
                    lstGo2 += 1
                    saveHistory()
                    readHistory()
                    C_sessionWeb.nCurSolution(nID) += 1
                End If
            End SyncLock

            Try
                SyncLock "log"
                    If Response.IsClientConnected = False Then
                        Exit Sub
                    End If
                    If Len(tbVarsWithName.Text) Then
                        e1 += "<br />Vars=<br />" + tbVarsWithName.Text
                    End If
                    Dim sLog As String = String.Format(
                    "Rnd={0} Eng={1} Units={2} Detall={3} Case sens.={4} " +
                    "Ignore spaces={5} Fractions={6} 1 letter var.={7} allow #s in var.={8} units={9}" + vbCrLf,
                    cfg.bRounding, cfg.bEngNotation, "",
                    cfg.bDetail, cfg.bCaseSensitive,
                   cfg.bIgnoreSpaces, cfg.bFractions, cfg.bVarName1Char,
                   cfg.bNumsInVarName, "")
                    C_sessionWeb.logFile(Nothing,
                        sLog + e1 _
                        , litResponse.Text, Page, nID, "polycalc.txt")
                End SyncLock
            Catch ex As Exception

            End Try
            For i = 1 To 3
                If Response.IsClientConnected = False Then
                    Exit Sub
                End If
                litResponse.Text += "<br />"
            Next
        Catch ex As Exception
            'If InStr(ex.ToString, "#") Then
            '    Dim sMsg As String = ex.ToString
            '    Dim pos As Int32 = InStr(sMsg, "#")
            '    pos = InStr(pos, sMsg, ")")
            '    sMsg = Mid(sMsg, 1, pos)
            '    sMsg = Replace(sMsg, "System.Exception:", "")
            '    errMsg(Mid(sMsg, 1, pos))
            'Else
            '    errMsg("n/a")
            'End If
            ''updateStatusBar(ex.Message)
        End Try
    End Sub
    Function removeComments(ByVal txt As String) As String
        Dim e1() As String = Nothing
        Try
            If txt = "" Then Return ""
            e1 = Split(txt, vbLf)
            For i = 0 To e1.Length - 1
                If Len(e1(i)) Then
                    Dim pos As Int32 = InStr(e1(i), "//")
                    If pos = 1 Then
                        e1(i) = ""
                    ElseIf pos > 1 Then
                        e1(i) = Trim(Mid(e1(i), 1, pos - 1))
                    End If
                End If
            Next
        Catch ex As Exception
            'updateStatusBar(ex.Message)
            errMsg(ex.ToString)
        End Try
        Return Join(e1, vbLf)
    End Function
    Function DspCalc(ByVal sQuery As String, ByVal sResult As String, ByVal n As Int32) As String
        Dim sRet As String = ""
        Try
            'wb.Dsp(n.ToString + ") Question:", "black", True)
            n = Me.DropHist.Items.Count
            'sRet = n.ToString + ") Question:" + "<br />"
            sRet = "Question:" + "<br />"
            If Len(sQuery) > 50 Then
                sQuery = Replace(sQuery, "+", " +")
                sQuery = Replace(sQuery, "-", " -")
            End If
            sQuery = Replace(sQuery, vbLf, "<br>")
            Dim qTbl As String =
            MathGlobal8.getTable(sQuery + " = ?", "black")
            qTbl = Replace(qTbl, "</TD><TD>", ";")
            'wb.Dsp(qTbl, "black", False)
            sRet += qTbl + "<br />"

            ''wb.Dsp(" Answer: ", "blue", True)
            'sRet += " Answer:" + "<br />"
            'Dim respTbl As String = _
            'MathGlobal8.getTable( _
            'sResult, "blue")
            ''+ vbCrLf + _
            ''ts2.TotalMilliseconds.ToString + " ms", "blue")
            ''wb.Dsp("<table><tr><td>=</td><td>" + _
            ''respTbl + _
            ''"</td></tr></table>", "blue", False)
            'sRet += "<table><tr><td>=</td><td>" + _
            'respTbl + _
            '"</td></tr></table>" + "<br />"


            sRet += " Answer:" + "<br />"
            'Dim respTbl As String = _
            'MathGlobal8.getTable( _
            'sResult, "blue")
            Dim respTbl As String = Replace(sResult, "+", " +")
            '+ vbCrLf + _
            'ts2.TotalMilliseconds.ToString + " ms", "blue")
            'wb.Dsp("<table><tr><td>=</td><td>" + _
            'respTbl + _
            '"</td></tr></table>", "blue", False)
            sRet += respTbl
            n += 1
            ' record:

            sRet = Replace(sRet, "integrate", "&int;")
            sRet = Replace(sRet, "integrate", "&int;")
        Catch ex As Exception
            'updateStatusBar(ex.Message)
        End Try
        Return sRet
    End Function

#End Region

#Region "enlace"
    Sub enlazar(ByVal redirect As Boolean)
        If sfn Is Nothing Then sfn = ""
        If sf2 Is Nothing Then sf2 = ""
        If sf3 Is Nothing Then sf3 = ""
        'If Me.usaAproximacion Then sf2 = ""
        'save()
        'If xi = -4 AndAlso xf = 4 Then
        '    Try
        '        xi = Double.Parse(izq.Text)
        '    Catch ex As Exception

        '    End Try
        '    Try
        '        xf = Double.Parse(dch.Text)
        '    Catch ex As Exception

        '    End Try
        'End If
        If xi = xf Then
            xi = -4 : xf = 4
        End If
        If xf - xi > 10000000000.0 Then
            xi = 0.001
            xf = 4
        End If
        If xAxis < 0.5 Then xAxis = 1.0
        If xAxis > 2.0 Then xAxis = 2.0
        If yAxis < 0.5 Then yAxis = 1.0
        If yAxis > 2.0 Then yAxis = 2.0
        'If Not Me.haydsp Then sf2 = ""
        'If Len(sf2) > 2000 Then sf2 = ""
        Dim enlace As String
        enlace = "xi=" + xi.ToString(MathGlobal8.us)
        enlace += "&xf=" + xf.ToString(MathGlobal8.us)
        enlace += "&yi=" + yi.ToString(MathGlobal8.us)
        enlace += "&yf=" + yf.ToString(MathGlobal8.us)
        enlace += "&fn=" + sfn
        enlace += "&xAxis=" + xAxis.ToString(MathGlobal8.us)
        enlace += "&yAxis=" + yAxis.ToString(MathGlobal8.us)
        enlace += "&f2=" + sf2
        enlace += "&f3=" + sf3 + "&h1=0"
        enlace += "&yAuto=" + yAuto
        enlace += "&chk=0"
        enlace += "&haydsp=True" '+ Me.haydsp.ToString()
        'Session.Add("xi", xi.ToString(MathGlobal8.us))
        'Session.Add("xf", xf.ToString(MathGlobal8.us))
        'Session.Add("yi", yi.ToString(MathGlobal8.us))
        'Session.Add("yf", yf.ToString(MathGlobal8.us))
        'Session.Add("yAuto", yAuto)
        'Session.Add("xAxis", xAxis)
        'Session.Add("yAxis", yAxis)
        'Session.Add("fn", sfn)
        'Session.Add("f2", sf2)
        'Session.Add("f3", sf3)
        C_sessionWeb.enlace(nID) = enlace
        'If Me.haydsp Then
        C_SessionWeb.tb1(nID) = tbQuery.Text ' TextBox1.Text
        C_SessionWeb.tb3(nID) = tbVarsWithName.Text ' Textbox3.Text
        'End If
        C_SessionWeb.yAuto(nID) = yAuto
        If yAuto = 1 Then
            C_SessionWeb.yi(nID) = 0
            C_SessionWeb.yf(nID) = 0
        End If

        'If Not redirect Then Return
        Response.Redirect("gphframes.aspx", False)
        Context.ApplicationInstance.CompleteRequest()
        'If Me.haydsp Then
        'Response.Redirect("gphframes.aspx?" + enlace, False)
        'Else
        'Roots.Response.Redirect("gphframes.aspx", False)
        'Roots.Response.Redirect("gphframes.aspx?" + enlace, False)
        'End If
    End Sub

#End Region


#Region "idioma"

    Sub Inicio(ByVal sIdioma As String)


        chkVar1Char.Text = "Vars' Name 1 character long"
        chkVar1Char.ToolTip = "Name of variables 1 character long"
        chkNumInVar.Text = "Allow numbers in variables' name"
        chkNumInVar.ToolTip = "Allow numbers in variables' name"
        Table2.Attributes.Add("style", "font-size: small")

        Select Case sIdioma
            Case "it"
                'calcolatrice di polinomi
                btnCalc.Text = "Calculare" + vbCrLf + "(tasti F8)"
                litComment1.Text = "Benvenuti i rinnovati calcolatrice di polinomi, e scientifici, gratis." +
                        " Può eseguire operazioni con i polinomi (+, -, *, /), trovare le radici; " +
                        " operare di funzioni " +
                        " logaritmica, trigonometriche e esponenziali, o definire una funzione personalizzata, " +
                        " nel campo dei numeri reali o complessi. Incorpora inoltre costanti come 'pi' e 'e'. "
                litComment2.Text = "è possibile inserire binario, ottale o numeri esadecimali " +
                        " anteponendo  &b, &o i &h e ottenere l'output in ciascuna di queste basi, " +
                        " selezionando la casella relativa. " +
                        " Altre opzioni sono il calcolo di derivati, integrali " +
                        " o interpolazione di Lagrange, oltre al calcolo matriciale e risoluzione di " +
                        " sistema di equazioni." +
                        " Spesso, se necessario, è possibile visualizzare esempi o passi intermedi."

                'lblConstFn.Text = "Constanti/Funzioni: "
                'lblExamples.Text = "Esempi: "
                lblExpression.Text = "Expressió a operar: "
                lblUpdated.Text = "(ultima revisione: " + sToday + ")"
                lblVariables.Text = "Vars.& Fnz.(f(x)=...,_x1=...:"
                chkBinary.Text = "Binario"
                chkCaseSensitive.Text = "Sensib.alle.maiusc."
                'chkUnits.Text = "Use of units"
                chkDetail.Text = "Dettaglio"
                'chkENG.Text="ENG"
                chkHexa.Text = "Esadecimale"
                chkOctal.Text = "Ottale"
                chkRound.Text = "Arrotondamento"
                chkFractions.Text = "Frazione"
                lblTitulo.Text = "Calcolatrice di polinomi e scientifici"
                'chkDegrees.Text = "Gradi"
                lblResult.Text = "Risultato:"
                chkIgnoreSpaces.Text = "Ignorare 'CR'"
            Case "ca"
                btnCalc.Text = "Calcular" + vbCrLf + "(tecla F8)"
                litComment1.Text = "Benvingut a la pàgina renovada de la calculadora de polinomis, i científica, gratuïta." +
                        " Podreu realitzar operacions amb polinomis (+, -, *, /), trobar les arrels; " +
                        " operar funcions " +
                        " logarítmiques, trigonomètriques i exponencials, o definir una funció personalitzada, " +
                        " en el domini dels nombres reals o complexos. També incorpora constants tals com 'pi' i 'e'. "
                litComment2.Text = "Tanmateix, podreu introduir nombres en base binària, octal o hexadecimal avantposant &b, &o i &h, " +
                        " i obtenir els resultats en cadascuna de les bases marcant la corresponent casella. " +
                        " Altres possibilitats són les de càlcul de derivades, integrals " +
                        " o la d'interpolació de Lagrange, a més de càlcul matricial i resolució de " +
                        " sistemes d'equacions." +
                        " Sovint, si cal, podreu consultar els exemples o els passos intermitjos."

                'lblConstFn.Text = "Constants/Funcions: "
                'lblExamples.Text = "Exemples: "
                lblExpression.Text = "Expressió a operar: "
                lblUpdated.Text = "(Darrere actualització: " + sToday + ")"
                lblVariables.Text = "Vars.& Fns.(f(x)=...,_x1=...:"
                chkBinary.Text = "Binari"
                chkCaseSensitive.Text = "Sensible Maj./min."
                'chkUnits.Text = "Use of units"
                chkDetail.Text = "Detall"
                'chkENG.Text="ENG"
                chkHexa.Text = "Hexadecimal"
                chkOctal.Text = "Octal"
                chkRound.Text = "Arrodonir"
                chkFractions.Text = "Fraccions"
                lblTitulo.Text = "Calculadora de polinomis i científica"
                '                chkDegrees.Text = "Graus"
                lblResult.Text = "Resultat:"
                chkIgnoreSpaces.Text = "Ignorar 'CR'"
                chkVar1Char.Text = "Vars.d'un sol caràcter"
                chkNumInVar.Text = "Vars.: admetre #s"
            Case "es"
                Table2.Attributes.Add("style", "font-size: x-small")
                btnCalc.Text = "Calcular" + vbCrLf + "(tecla F8)"
                litComment1.Text = "Bienvenido a la página renovada de la calculadora de polinomios, y científica, gratis online. " +
                    "Podrá realizar operaciones con polinomios (+,-,*,/), hallar las raíces; " +
                    "operar funciones " +
                    "logarítmicas, trigonométricas y exponenciales, o definir una función personalizada, " +
                    "en el dominio de los números reales o complejos. También incorpora constantes tales como 'pi' y 'e'. "
                litComment2.Text = "Así mismo podrá introducir números en base binaria, octal o hexadecimal anteponiendo &b, &o y &h " +
                    " y obtener los resultados en cada una de dichas bases, marcando la respectiva casilla. " +
                    " Otras posibilidades son el cálculo de derivadas, integrales, " +
                    " interpolación de Lagrange, además de cálculo matricial y resolución de " +
                    " sistemas de ecuaciones." +
                    " A menudo, podrá consultar los ejemplos o los pasos intermedios."
                'lblConstFn.Text = "Constantes/Funciones: "
                'lblExamples.Text = "Ejemplos: "
                lblExpression.Text = "Expresión a operar: "
                lblUpdated.Text = "(Ultima actualización: " + sToday + ")"
                lblVariables.Text = "Variables (si hay): "
                chkBinary.Text = "Binario"
                chkCaseSensitive.Text = "Sensible May./min."
                'chkUnits.Text = "Uso de unidades"
                chkDetail.Text = "Detalle"
                'chkENG.Text="ENG"
                chkHexa.Text = "Hexadecimal"
                chkOctal.Text = "Octal"
                chkRound.Text = "Redondeo"
                chkFractions.Text = "Fracciones"
                lblTitulo.Text = "Calculadora de polinomios y científica"
                'chkDegrees.Text = "Grados"
                lblResult.Text = "Resultado:"
                chkIgnoreSpaces.Text = "Ignorar 'CR'"
                chkVar1Char.Text = "Vars. de un carácter"
                chkNumInVar.Text = "Vars.: admitir #s"
            Case "pt"
                btnCalc.Text = "Calcular" + vbCrLf + "(tecla F8)"
                litComment1.Text = "Bem-vindo à renovada calculadora do polinômios, e científica, online. " +
                    "Você pode executar operações com polinômios (+, -, *, /), encontrar as raízes; " +
                    "operar funções " +
                    "logarítmicas, trigonométricas e exponenciais, ou definir uma personalizada, " +
                    " no domínio dos números reais ou complexos. Também incorpora as constantes como 'pi' e 'e'. "
                litComment2.Text = "Além disso, você pode entrar números binário, octal ou hexadecimal " +
                    " com os prefixaçãos &b, &o e &h e obter os resultados em cada uma dessas bases, marcando " +
                    " a caixa apropriada. Outras opções são o cálculo de derivadas, integrais, " +
                    " interpolação de Lagrange, cálculo matricial e sistemas de resolução de equações. " +
                    " Em muitos casos, você pode ver exemplos ou as etapas intermédias."
                'lblConstFn.Text = "Constantes/Funções: "
                'lblExamples.Text = "Exemplos: "
                lblExpression.Text = "Expressão para operar: "
                lblUpdated.Text = "(Última atualização: " + sToday + ")"
                lblVariables.Text = "Variáveis ​​(se houver): "
                chkBinary.Text = "Binário"
                chkCaseSensitive.Text = "Case sensitive"
                'chkUnits.Text = "Use of units"
                chkDetail.Text = "Detalhe"
                'chkENG.Text="ENG"
                chkHexa.Text = "Hexadecimal"
                chkOctal.Text = "Octal"
                chkRound.Text = "Arredondar"
                chkFractions.Text = "Frações"
                lblTitulo.Text = "Calculadora do Polinómios e Científica"
                'chkDegrees.Text = "Graus"
                lblResult.Text = "Resultado:"
                chkIgnoreSpaces.Text = "Ignorar 'CR'"
            Case "fr"
                btnCalc.Text = "Calculer" + vbCrLf + "(F8)"
                litComment1.Text = "Bienvenue sur le renouvelée calculateur de polynôme, et scientifique, gratuit." _
                + " Vous pouvez effectuer des opérations avec des polynômes (+, -, *, /), trouver les racines; " _
                + " opérer fonctions " _
                + " logarithmiques, trigonométriques et exponentielle, ou définir une fonction personnalisée, " _
                + " dans le domaine des nombres réels ou complexes. Il intègre également des constantes comme «pi» et «e». "
                litComment2.Text = "Aussi, vous pouvez donner des chiffres binaires, octal ou hexadécimal en faisant précéder &b, &o et &h" _
                + " et obtenir les résultats en chacune de ces bases, en choisissant la case appropriée. " _
                + " Autres options sont le calcul des dérivées, intégrales " _
                + " ou interpolation de Lagrange, calcul matriciel et résolution de " _
                + " systèmes d'équations. " _
                + " Souvent, vous pouvez voir les exemples au les étapes intermédiaires. "
                'lblConstFn.Text = "Constantes/Fonctions: "
                'lblExamples.Text = "Exemples. "
                lblExpression.Text = "Expression d'opération: "
                lblUpdated.Text = "(dernière mise à jour: " + sToday + ")"
                lblVariables.Text = "Variables, si c'est le cas: "
                chkBinary.Text = "Binaire"
                chkCaseSensitive.Text = "Sensible a maj./min."
                'chkUnits.Text = "Use of units"
                chkDetail.Text = "Détail"
                'chkENG.Text="ENG"
                chkHexa.Text = "Hexadécimal"
                chkOctal.Text = "Octal"
                chkRound.Text = "Arrondir"
                chkFractions.Text = "Fractions"
                lblTitulo.Text = "Calculateur de polynôme et scientifique"
                'chkDegrees.Text = "Degrés"
                lblResult.Text = "Résultat:"
                chkIgnoreSpaces.Text = "Ignorer 'CR'"
            Case Else
                btnCalc.Text = "Calculate (F8 key)"
                litComment1.Text = "Welcome here to this Free Online Polynomial, and Scientific Calculator. " +
                            "You can perform operations with polynomials (+, -, *, /), find roots; " +
                            "operate logarithmic, trigonometric, matrix or exponential functions, or define custom functions, " +
                            "in the field of real or complex numbers. The calculator incorporates the constants 'pi' and 'e'. "
                litComment2.Text = "Also you can enter binary, octal or hexadecimal numbers by prefixing &b, &o and &h " +
                            " and get the output in each of these bases, checking the respective box. " +
                            " Other options are the calculus of derivatives, integrals, " +
                            " Lagrangian interpolation, matrices operations and resolution of " +
                            " systems of equations. " +
                            " In many cases, you may glance at the examples or to the intermediate steps."
                'lblConstFn.Text = "Constants/Functions: "
                'lblExamples.Text = "Examples: "
                tbVarsWithName.ToolTip = "Enter here your custom defined constants, variables and/or functions"
                tbQuery.ToolTip = "Enter here the math expression to operate/solve " + vbCrLf + "(e.g. x^2-1=0, P(-1), ...)" + vbCrLf + "and click on 'Calculate' button or press F8 key."
                lblExpression.Text = "Expression to operate: "
                lblUpdated.Text = "(Last update: " + sToday + ")"
                lblVariables.Text = "Variables/Custom Fn.: "
                chkBinary.Text = "Binary"
                chkCaseSensitive.Text = "Case sensitive"
                chkCaseSensitive.ToolTip = "Input data will be case sensitive (Yes/No)"
                'chkUnits.Text = "Use of units"
                chkDetail.Text = "Detail"
                chkDetail.ToolTip = "Try to show detailed info in the output"
                'chkENG.Text="ENG"
                chkHexa.Text = "Hexadecimal"
                chkOctal.Text = "Octal"
                chkRound.Text = "Rounding"
                chkFractions.Text = "Fractions"
                chkFractions.ToolTip = "Try to show fractions in the output (Yes/No)"
                lblTitulo.Text = "Polynomials &amp; Scientific Calculator"
                'chkDegrees.Text = "Degrees"
                lblResult.Text = "Result:"
                chkIgnoreSpaces.Text = "Ignore 'CR'"
                lblVariables.Text = "Definitions box (e.g. P(x)=x^2-1, x=3, ...):"
        End Select
        Dim s As String = litComment1.Text
        litComment1.Text = "<p style=""text-align:justify; width:90%;"">"
        litComment1.Text += s
        litComment1.Text += "</p>"
        litComment1.Visible = True
        s = litComment2.Text
        litComment2.Text = "<p style=""text-align:justify;  width:90%;"">"
        litComment2.Text += s
        litComment2.Text += "</p>"
        litComment2.Visible = True

        'litComment1.Text = ""
        'litComment2.Text = ""
        btnHelp.Text = "User's guide (.pdf) " +
            "v8.3.110"
        btnHelp.Font.Size = FontUnit.Small
    End Sub

#End Region


    Function UploadBtn_Click(ByVal sender As Object, ByVal e As EventArgs) As Boolean 'Handles btnUpload1.Click
        Try
            Dim fileExtension As String
            fileExtension = System.IO.Path.
                GetExtension(FileUpload1.FileName).ToLower()
            Dim allowedExtensions As String() =
                {".txt"}
            Dim fileOk As Boolean = False
            For i As Integer = 0 To allowedExtensions.Length - 1
                If fileExtension = allowedExtensions(i) Then
                    fileOk = True
                End If
            Next
            If fileOk AndAlso FileUpload1.PostedFile.ContentLength <= 10 ^ 6 Then
                FileUpload1.PostedFile.InputStream.Position = 0
                Dim sf As New Soap.SoapFormatter()
                sTbQry = sf.Deserialize(FileUpload1.PostedFile.InputStream)
                sTbVar = sf.Deserialize(FileUpload1.PostedFile.InputStream)
                vOptions = sf.Deserialize(FileUpload1.PostedFile.InputStream)
                Me.saveHistory()
                readHistory()
                Return True
            ElseIf fileOk Then
                litResponse.Text = "<h3>File too large. (max. 1MB)</h3>"
            End If
        Catch ex As Exception

        End Try
        Return False
    End Function

    Private Sub btnSave_Click(sender As Object, e As ImageClickEventArgs) Handles btnSave.Click
        Try
            'Dim sFile As String = readHistory()
            'setSession(False, sFile)
            readHistory()
            If lstGo2 = 0 Then
                Exit Sub
            End If
            ' save LBHistQuery items:
            Dim mem As New MemoryStream()
            Dim sf As New Soap.SoapFormatter
            sf.Serialize(mem, sTbQry)
            sf.Serialize(mem, sTbVar)
            sf.Serialize(mem, vOptions)
            mem.Flush()
            mem.Position = 0
            Dim sr As New StreamReader(mem)
            Dim sFile As String = sr.ReadToEnd
            mem.Close()


            Dim FileName As String = "m8"
            Dim attachment As String = "attachment; filename=" + FileName + ".txt"
            HttpContext.Current.Response.Clear()
            HttpContext.Current.Response.ClearHeaders()
            HttpContext.Current.Response.ClearContent()
            HttpContext.Current.Response.AddHeader("content-disposition", attachment)
            HttpContext.Current.Response.ContentType = "plain/text"
            HttpContext.Current.Response.AddHeader("Pragma", "public")
            'Dim buf() As Byte = System.Text.UnicodeEncoding.Unicode.GetBytes(sFile)
            HttpContext.Current.Response.Write(sFile)
            HttpContext.Current.Response.Flush()
            HttpContext.Current.Response.Close()
            Response.End()
        Catch ex As Exception

        End Try
    End Sub


#Region "read/save history"
    Dim sFilas As String = "#@@//@@" + vbCrLf
    Public Sub readHistory()
        Try
            ReDim sTbQry(-1), sTbVar(-1)
            Dim o = Me.histQ.Value
            If Len(o) Then
                o = System.Web.HttpUtility.UrlDecode(o)
                sTbQry = Split(o, sFilas)
            End If
            o = Me.histV.Value
            If Len(o) Then
                o = System.Web.HttpUtility.UrlDecode(o)
                sTbVar = Split(o, sFilas)
            End If
            o = Me.options.Value
            If Len(o) Then
                o = System.Web.HttpUtility.UrlDecode(o)
                vOptions = Split(o, sFilas)
            End If
            DropHist.Items.Clear()
            DropHist.Items.Add("Record:")
            For i As Int32 = 0 To sTbQry.Length - 1
                Dim e1 As String = Replace(sTbQry(i), ",", ";")
                e1 = Replace(e1, vbCrLf, "|")
                e1 = Replace(e1, vbCr, "|")
                e1 = Replace(e1, vbLf, "|")
                e1 = Replace(e1, "||", "|")
                Dim e2 As String = Replace(sTbVar(i), ",", ";")
                e2 = Replace(e2, vbCrLf, "|")
                e2 = Replace(e2, vbCr, "|")
                e2 = Replace(e2, vbLf, "|")
                e2 = Replace(e2, "||", "|")
                DropHist.Items.Add((1 + i).ToString + " " + e1 + "   " + e2)
            Next
            lstGo2 = sTbQry.Length
        Catch ex As Exception

        End Try
    End Sub
    Public Sub saveHistory()
        Try
            Me.histQ.Value = System.Web.HttpUtility.UrlEncode(Join(sTbQry, sFilas))
            Me.histV.Value = System.Web.HttpUtility.UrlEncode(Join(sTbVar, sFilas))
            Me.options.Value = System.Web.HttpUtility.UrlEncode(Join(vOptions, sFilas))
        Catch ex As Exception

        End Try
    End Sub
    Sub clearHistory()
        histQ.Value = ""
        histV.Value = ""
        DropHist.Items.Clear()
    End Sub
#End Region

    Private Sub btnClearDropQuery_Click(sender As Object, e As EventArgs) Handles btnClearDropQuery.Click
        clearHistory()
    End Sub


#Region "options"
    Sub parseOptions(ByVal txt As String, Optional bResize As Boolean = True)
        Try
            If txt = "" Then
                Exit Sub
            End If
            Dim e1() As String = Split(txt, "|")
            'chkClear.IsChecked = Boolean.Parse(e1(0))
            chkENG.Checked = Boolean.Parse(e1(1))
            chkCaseSensitive.Checked = Not Boolean.Parse(e1(2))
            chkIgnoreSpaces.Checked = Boolean.Parse(e1(3))
            chkRound.Checked = Boolean.Parse(e1(4))
            chkFractions.Checked = Boolean.Parse(e1(5))
            rbI.Checked = Boolean.Parse(e1(6))
            rbJ.Checked = Not rbI.Checked
            'chkDegrees.IsChecked = Boolean.Parse(e1(7))
            chkHexa.Checked = Boolean.Parse(e1(8))
            chkOctal.Checked = Boolean.Parse(e1(9))
            chkBinary.Checked = Boolean.Parse(e1(10))
            chkDetail.Checked = Boolean.Parse(e1(11))
            chkVar1Char.Checked = Boolean.Parse(e1(12))
            chkNumInVar.Checked = Boolean.Parse(e1(13))
            factor = Single.Parse(e1(14), Globalization.NumberStyles.Float, MathGlobal8.us)
            'cbTimeout.SelectedIndex = Int32.Parse(e1(15))
        Catch ex As Exception
        End Try
    End Sub
    Function toStrOptions() As String
        Dim e1() As String = Nothing
        Try
            e1 = New String() {
                "False",
                chkENG.Checked.ToString,
                (Not chkCaseSensitive.Checked).ToString,
                chkIgnoreSpaces.Checked.ToString,
                chkRound.Checked.ToString,
                chkFractions.Checked.ToString,
                rbI.Checked.ToString,
                "",
                chkHexa.Checked.ToString,
                chkOctal.Checked.ToString,
                chkBinary.Checked.ToString,
                chkDetail.Checked.ToString,
                chkVar1Char.Checked.ToString,
                chkNumInVar.Checked.ToString,
                "1.0",
                "10",
                "maximized",
                "0",
                "0",
                "300",
                "200",
                "false",
                "Arial",
                "12",
                "Normal",
                "Normal"
                }
        Catch ex As Exception
            'If bLoaded Then
            MsgBox(ex.ToString)
            'End If
        End Try
        Return Join(e1, "|")
    End Function

#End Region
End Class
