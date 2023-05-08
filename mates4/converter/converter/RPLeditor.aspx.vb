Imports System.IO


Public Class RPLeditor
    Inherits System.Web.UI.Page

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
    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load

        Dim o = Request.Params("eDiv")
        If o IsNot Nothing Then
            Initialize()
            UpdatePanel(CStr(o))
        ElseIf Not Page.IsPostBack Then
            populateTreeView()
        End If
    End Sub
    Private Sub UpdatePanel(param As String)
        Try
            Response.Clear()
            Response.ClearContent()
            If param.Length = 0 Then
                Response.Write("")
                Exit Try
            End If
            Dim e1 As String = ""
            e1 = Color(param)
            Response.Write(e1)
            Response.End()
        Catch ex As Exception
        End Try
    End Sub

    Public Function sAll2() As String
        Return Replace(sAll, "{}", "|" + "")
    End Function
    Public Sub Initialize()
        Try
            sFn = "(?<fn>(?i)" + Join(vFn, "|") + "(?-i))"
            sVar2 = "(?<var2>([a-zA-Z" + sGreek + "]){1}([a-zA-Z" + sGreek + "]){0,1})"
            sCnt = "(?<cnt>(?i)(?<![A-Za-z]+)(" + sCnts + "){1}(?![A-Za-z]+)(?-i))"
            Dim sImg As String = "i"
            Dim sImag As String
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
        Try
            Dim re As New Regex("(?<op>[-+*\/\^\!]+)|" + sNum +
                                "|" + vbLf +
                                "|" + sFn + "|" + sVar2 + "|(?<s>\s+)|" + sVar +
                                "|[\@\=\,\;\|\r\n]|(?<parenth>[\[\]\(\)\{\}])|(?<any>.+)", RegexOptions.IgnoreCase)
            Dim mc As MatchCollection = re.Matches(e1)
            sb.Append("<div>")
            For Each m As Match In mc
                If m.ToString.Length Then
                    If m.Groups("num").Success Then
                        sb.Append("<span style='color:orange;'>" + m.ToString + "</span>")
                    ElseIf m.Groups("op").Success Then
                        sb.Append("<span style='color:red;'>" + m.ToString + "</span>") ' dark red
                    ElseIf m.Groups("fn").Success Then
                        sb.Append("<span style='font-weight:bold;'>" + m.ToString + "</span>")
                    ElseIf m.Groups("var2").Success OrElse m.Groups("var").Success Then
                        sb.Append("<span style='color:#1B1BB1;'>" + m.ToString + "</span>") ' dark blue
                    ElseIf m.ToString = vbLf Then
                        sb.Append("</div><div>")
                        If m.Index = Len(e1) - 1 Then
                            sb.Append(" ")
                            'hiddencaretpos.Value += 1
                        End If
                    ElseIf m.Groups("s").Success Then
                        sb.Append(m.ToString)
                    Else
                        sb.Append("<span style='color:black;'>" + m.ToString + "</span>") ' dark blue
                    End If
                End If
            Next
            sb.Append("</div>")
        Catch ex As Exception

        End Try
        Return sb.ToString
    End Function
    Sub populateTreeView()
        Dim fs As FileStream = Nothing
        Dim sr As StreamReader = Nothing
        Try
            Dim vFn(-1) As String
            Dim iv As Int32 = vFn.Length
            Dim path As String = "C:\inetpub\wwwroot\txt\HPcmds.txt" 'Page.ResolveUrl(Server.MapPath("HPcmds.txt"))
            fs = New FileStream(path, FileMode.Open)
            sr = New StreamReader(fs)
            Dim root As New TreeNode
            root.Target = "_self"
            root.NavigateUrl = "javascript:void(0);"
            Dim parents() As TreeNode = New TreeNode() {root}
            TreeView1.Nodes.Add(root)
            Dim oldNivel As Int32 = 0
            sr.ReadLine()
            Dim cnt As Int32 = 1
            Dim sumar As Int32 = 0
            Dim noMas As Boolean = False
            Do While Not sr.EndOfStream
                Dim e1 As String = sr.ReadLine
                Dim nivel As Int32 = 1
                Do While e1.Chars(0) = " " OrElse e1.Chars(0) = vbTab
                    nivel += 1
                    e1 = Mid(e1, 2)
                Loop
                e1 = Replace(e1, ChrW(141), "&#8594;") ' rarr right arrow

                e1 = Replace(e1, ChrW(8250), "&#916;") ' DELTA
                e1 = Replace(e1, ChrW(8230), "&#931;") ' sigma
                e1 = Replace(e1, ChrW(339), "&#928;") ' PI
                e1 = Replace(e1, ChrW(8225), "&#960;") ' pi
                'e1 = Replace(e1, ChrW(45), "&#955;") ' lamda
                e1 = Replace(e1, ChrW(8222), "&#8747;") ' integral
                e1 = Replace(e1, ChrW(402), "&#8730;") ' square root
                e1 = Replace(e1, ChrW(8249), "&#8800;") ' not equal
                e1 = Replace(e1, ChrW(8240), "&#8804;") ' less or equal
                e1 = Replace(e1, ChrW(352), "&#8805;") ' greater or equal
                e1 = Replace(e1, ChrW(381), "&#8592;") ' arrow to left
                e1 = Replace(e1, ChrW(8364), "&#8736;") ' angle
                e1 = Replace(e1, ChrW(376), "&#8734;") ' infinity
                If e1 = "OTHERS" Then
                    'sumar = 1
                End If
                nivel += sumar
                oldNivel += sumar
                If nivel >= parents.Length Then
                    ReDim Preserve parents(nivel)
                End If
                Dim m As Match = Regex.Match(e1, "\[[^]]+\]")
                ReDim Preserve parents(nivel)
                If m.Success Then
                    e1 = Mid(m.Value, 2, m.Length - 2)
                    Dim currNode As New TreeNode(e1)
                    currNode.Target = "_self"
                    'currNode.NavigateUrl = "javascript:void(0);"
                    parents(nivel - 1).ChildNodes.Add(currNode)
                    parents(nivel) = currNode
                Else
                    If e1 = "1E" Then
                        noMas = True
                    End If
                    Dim currNode As New TreeNode(e1)
                    currNode.Target = "_self"
                    If Regex.IsMatch(e1, "[^a-zA-Z0-9]") OrElse noMas Then
                        'currNode.NavigateUrl = "javascript:void(0);"
                    Else
                        'currNode.NavigateUrl = "javascript:getSelectedNode(this);"
                        ReDim Preserve vFn(iv)
                        vFn(iv) = System.Web.HttpUtility.JavaScriptStringEncode(e1)
                        iv += 1
                    End If
                    parents(nivel - 1).ChildNodes.Add(currNode)
                    parents(nivel) = currNode
                End If
                oldNivel = nivel
                cnt += 1
            Loop
            Dim ln(vFn.Length - 1) As Int32
            For i As Int32 = 0 To vFn.Length - 1
                ln(i) = Len(vFn(i))
            Next
            Array.Sort(ln, vFn)
            Array.Reverse(vFn)
            'Dim e2 As String = "(" + Join(vFn, "|") + ")"
            Dim e2 As String = "(?:\s)(" + Join(Me.vFn, "|") + ")"
            HiddensFn.Value = e2
            TreeView1.ExpandDepth = 1
            populateTreeViewEvent(TreeView1.Nodes(0))
        Catch ex As Exception
            Dim s1() As String = Split(ex.ToString, vbCrLf)
            Dim s2 As String = ex.ToString
        Finally
            Try
                sr.Close()
                fs.Close()
            Catch ex As Exception

            End Try
        End Try
    End Sub
    Sub populateTreeViewEvent(n As TreeNode)
        If n.ChildNodes.Count Then
            For Each child In n.ChildNodes
                populateTreeViewEvent(child)
            Next
            n.NavigateUrl = "javascript:void(0);"
        Else
            Dim e1 As String = n.Text
            e1 = Replace(e1, "&#8594;", ChrW(141)) ' rarr right arrow
            e1 = Replace(e1, "&#916;", ChrW(8250)) ' DELTA
            e1 = Replace(e1, "&#931;", ChrW(8230)) ' sigma
            e1 = Replace(e1, "&#928;", ChrW(339)) ' PI
            e1 = Replace(e1, "&#960;", ChrW(8225)) ' pi
            'e1 = Replace(e1, ChrW(45), "&#955;") ' lamda
            e1 = Replace(e1, "&#8747;", ChrW(8222)) ' integral
            e1 = Replace(e1, "&#8730;", ChrW(402)) ' square root
            e1 = Replace(e1, "&#8800;", ChrW(8249)) ' not equal
            e1 = Replace(e1, "&#8804;", ChrW(8240)) ' less or equal
            e1 = Replace(e1, "&#8805;", ChrW(352)) ' greater or equal
            e1 = Replace(e1, "&#8592;", ChrW(381)) ' arrow to left
            e1 = Replace(e1, "&#8736;", ChrW(8364)) ' angle
            e1 = Replace(e1, "&#8734;", ChrW(376)) ' infinity

            n.NavigateUrl = "javascript:getSelectedNode('" + e1 + "');"
        End If
    End Sub

    Private Sub TreeView1_SelectedNodeChanged(sender As Object, e As EventArgs) Handles TreeView1.SelectedNodeChanged
        Try
            Dim pos As Int32 = hiddencaretpos.Value
            Dim sb As New StringBuilder(hiddeneDiv.Value)
            Dim pos1 As Int32 = pos
            For i As Int32 = 0 To pos
                If i < sb.Length AndAlso InStr(vbCrLf, sb.Chars(i)) Then
                    pos1 += 1
                End If
            Next
            sb.Insert(pos1, TreeView1.SelectedValue)
            eDiv.InnerHtml = sb.ToString
            hiddeneDiv.Value = sb.ToString
            'ClientScript.RegisterStartupScript(Page.GetType, "hwa", "Update1();")
        Catch ex As Exception

        End Try
    End Sub

    Private Sub btnSave_Click(sender As Object, e As EventArgs) Handles btnSave.Click
        Try
            Response.Clear()
            Response.AddHeader("content-disposition", "attachment; filename=RPLcode.txt")
            Response.ContentType = "application/octet-stream"
            Using sw As New StreamWriter(Response.OutputStream)
                sw.WriteLine(hiddeneDiv.Value)
                sw.Close()
            End Using
            Response.Flush()
            Response.End()
        Catch ex As Exception

        End Try
    End Sub

    Private Sub btnLoad_Click(sender As Object, e As EventArgs) Handles btnLoad.Click
        Try
            Dim fileExtension As String
            fileExtension = System.IO.Path.
                GetExtension(FileUpload1.FileName).ToLower()
            If fileExtension = ".txt" Then
                Using sr As New StreamReader(FileUpload1.PostedFile.InputStream)
                    eDiv.InnerText = sr.ReadToEnd
                End Using
                Exit Sub
            End If
        Catch ex As Exception

        End Try
    End Sub
End Class