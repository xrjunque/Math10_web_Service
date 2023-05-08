Imports System.IO
Imports System.Web
Imports System.Threading
Imports System.Net
Imports System.Web.HttpUtility

Public Class C_sessionWeb

    Public Shared sCurrVersion As String = "2023/05/06 -- v8.4.194"

    Const n As Integer = 9999
    Public Shared enlace1(n), tb11(n), tb31(n) As String, Idioma1(n) As String, round(n) As Boolean
    Public Shared yAuto1(n) As Int32, yi1(n), yf1(n), xtg1(n), xa1(n), ytg21(n), ya21(n), xi1(n), xf1(n), nPoints1(n) As Double
    Public Shared yiB1(n), yfB1(n), xAxis1(n), yAxis1(n) As Double
    Public Shared path1(n) As String, local1(n) As Boolean
    Public Shared numID As Int32, referer(n) As String
    Public Shared psfn(n), psf2(n), psf3(n), fp0(n) As String, lenVars(n) As Int32, usaGrafica(n), usaAproximacion(n) As Boolean
    Public Shared sProvieneDe(n), sTb1(n), sTb2(n) As String
    Public Shared numDspGrafica(n) As Int32
    Public Shared iH(n) As Int32, HT(n)() As String
    Public Shared HObj(n)()() As Object
    Public Shared strPtosLagr(n), strSolucPtosLagr(n) As String
    Public Shared maxNumUsers As Int32 = 200
    Public Shared ultimoID As Int32 = 0, strListaID(maxNumUsers) As String ' listaID(maxNumUsers) As Int32,
    Public Shared isLocalhost As Boolean = False
    Public Shared bPrecisFraccion(n) As Boolean
    Public Shared l1(n), l2(n) As Int32
    'Public Shared autoLog As New AutoResetEvent(True) 
    Public Shared mtx As New Mutex()
    Public Shared nCurSolution(n) As Int32
    Public Shared recaptcha(n) As Int32


    Public Shared Function init(ByVal id As Int32) As Int32
        If id = 0 Then
            numID += 1
            If numID > n Then
                numID = 0
            End If
            id = numID
        End If
        Return id
    End Function


    ' update  5/April/2018
    Shared MobileCheck As Regex = New Regex("(android|bb\d+|meego).+mobile|avantgo|bada\/|blackberry|blazer|compal|elaine|fennec|hiptop|iemobile|ip(hone|od)|iris|kindle|lge |maemo|midp|mmp|mobile.+firefox|netfront|opera m(ob|in)i|palm( os)?|phone|p(ixi|re)\/|plucker|pocket|psp|series(4|6)0|symbian|treo|up\.(browser|link)|vodafone|wap|windows ce|xda|xiino", RegexOptions.IgnoreCase Or RegexOptions.Multiline Or RegexOptions.Compiled)
    Shared MobileVersionCheck As Regex = New Regex("1207|6310|6590|3gso|4thp|50[1-6]i|770s|802s|a wa|abac|ac(er|oo|s\-)|ai(ko|rn)|al(av|ca|co)|amoi|an(ex|ny|yw)|aptu|ar(ch|go)|as(te|us)|attw|au(di|\-m|r |s )|avan|be(ck|ll|nq)|bi(lb|rd)|bl(ac|az)|br(e|v)w|bumb|bw\-(n|u)|c55\/|capi|ccwa|cdm\-|cell|chtm|cldc|cmd\-|co(mp|nd)|craw|da(it|ll|ng)|dbte|dc\-s|devi|dica|dmob|do(c|p)o|ds(12|\-d)|el(49|ai)|em(l2|ul)|er(ic|k0)|esl8|ez([4-7]0|os|wa|ze)|fetc|fly(\-|_)|g1 u|g560|gene|gf\-5|g\-mo|go(\.w|od)|gr(ad|un)|haie|hcit|hd\-(m|p|t)|hei\-|hi(pt|ta)|hp( i|ip)|hs\-c|ht(c(\-| |_|a|g|p|s|t)|tp)|hu(aw|tc)|i\-(20|go|ma)|i230|iac( |\-|\/)|ibro|idea|ig01|ikom|im1k|inno|ipaq|iris|ja(t|v)a|jbro|jemu|jigs|kddi|keji|kgt( |\/)|klon|kpt |kwc\-|kyo(c|k)|le(no|xi)|lg( g|\/(k|l|u)|50|54|\-[a-w])|libw|lynx|m1\-w|m3ga|m50\/|ma(te|ui|xo)|mc(01|21|ca)|m\-cr|me(rc|ri)|mi(o8|oa|ts)|mmef|mo(01|02|bi|de|do|t(\-| |o|v)|zz)|mt(50|p1|v )|mwbp|mywa|n10[0-2]|n20[2-3]|n30(0|2)|n50(0|2|5)|n7(0(0|1)|10)|ne((c|m)\-|on|tf|wf|wg|wt)|nok(6|i)|nzph|o2im|op(ti|wv)|oran|owg1|p800|pan(a|d|t)|pdxg|pg(13|\-([1-8]|c))|phil|pire|pl(ay|uc)|pn\-2|po(ck|rt|se)|prox|psio|pt\-g|qa\-a|qc(07|12|21|32|60|\-[2-7]|i\-)|qtek|r380|r600|raks|rim9|ro(ve|zo)|s55\/|sa(ge|ma|mm|ms|ny|va)|sc(01|h\-|oo|p\-)|sdk\/|se(c(\-|0|1)|47|mc|nd|ri)|sgh\-|shar|sie(\-|m)|sk\-0|sl(45|id)|sm(al|ar|b3|it|t5)|so(ft|ny)|sp(01|h\-|v\-|v )|sy(01|mb)|t2(18|50)|t6(00|10|18)|ta(gt|lk)|tcl\-|tdg\-|tel(i|m)|tim\-|t\-mo|to(pl|sh)|ts(70|m\-|m3|m5)|tx\-9|up(\.b|g1|si)|utst|v400|v750|veri|vi(rg|te)|vk(40|5[0-3]|\-v)|vm40|voda|vulc|vx(52|53|60|61|70|80|81|83|85|98)|w3c(\-| )|webc|whit|wi(g |nc|nw)|wmlb|wonu|x700|yas\-|your|zeto|zte\-", RegexOptions.IgnoreCase Or RegexOptions.Multiline Or RegexOptions.Compiled)

    Public Shared Function BrowserIsMobile() As Boolean
        Debug.Assert(HttpContext.Current IsNot Nothing)
        Try
            If HttpContext.Current.Request IsNot Nothing AndAlso
            HttpContext.Current.Request.ServerVariables("HTTP_USER_AGENT") IsNot Nothing Then

                Dim u As String = HttpContext.Current.Request.ServerVariables("HTTP_USER_AGENT").ToString()
                If (u.Length < 4) Then
                    Return False
                End If
                If MobileCheck.IsMatch(u) OrElse MobileVersionCheck.IsMatch(u.Substring(0, 4)) Then
                    Return True
                End If
            End If
        Catch ex As Exception
        End Try
        Return False
    End Function
    Public Shared Property sfn(id As Int32) As String
            Get
                Return psfn(id)
            End Get
            Set(value As String)
                psfn(id) = value
            End Set
        End Property
        Public Shared Property sf2(id As Int32) As String
            Get
                Return psf2(id)
            End Get
            Set(value As String)
                psf2(id) = value
            End Set
        End Property
        Public Shared Property sf3(id As Int32) As String
            Get
                Return psf3(id)
            End Get
            Set(value As String)
                psf3(id) = value
            End Set
        End Property
        Public Shared Property local(ByVal id As Int32) As String
            Get
                Return local1(id)
            End Get
            Set(ByVal value As String)
                local1(id) = value
            End Set
        End Property
        Public Shared Property path(ByVal id As Int32) As String
            Get
                Return path1(id)
            End Get
            Set(ByVal value As String)
                path1(id) = value
            End Set
        End Property
        Public Shared Property yAxis(ByVal id As Int32) As Double
            Get
                Return yAxis1(id)
            End Get
            Set(ByVal value As Double)
                yAxis1(id) = value
            End Set
        End Property
        Public Shared Property xAxis(ByVal id As Int32) As Double
            Get
                Return xAxis1(id)
            End Get
            Set(ByVal value As Double)
                If value < 0.5 Then value = 1.0
                xAxis1(id) = value
            End Set
        End Property
        Public Shared Property yfB(ByVal id As Int32) As Double
            Get
                Return yfB1(id)
            End Get
            Set(ByVal value As Double)
                yfB1(id) = value
            End Set
        End Property
        Public Shared Property yiB(ByVal id As Int32) As Double
            Get
                Return yiB1(id)
            End Get
            Set(ByVal value As Double)
                yiB1(id) = value
            End Set
        End Property
        Public Shared Property nPoints(ByVal id As Int32) As Double
            Get
                Return nPoints1(id)
            End Get
            Set(ByVal value As Double)
                nPoints1(id) = value
            End Set
        End Property
        Public Shared Property xf(ByVal id As Int32) As Double
            Get
                Return xf1(id)
            End Get
            Set(ByVal value As Double)
                xf1(id) = value
            End Set
        End Property
        Public Shared Property xi(ByVal id As Int32) As Double
            Get
                Return xi1(id)
            End Get
            Set(ByVal value As Double)
                xi1(id) = value
            End Set
        End Property
        Public Shared Property ya2(ByVal id As Int32) As Double
            Get
                Return ya21(id)
            End Get
            Set(ByVal value As Double)
                ya21(id) = value
            End Set
        End Property
        Public Shared Property ytg2(ByVal id As Int32) As Double
            Get
                Return ytg21(id)
            End Get
            Set(ByVal value As Double)
                ytg21(id) = value
            End Set
        End Property
        Public Shared Property xa(ByVal id As Int32) As Double
            Get
                Return xa1(id)
            End Get
            Set(ByVal value As Double)
                xa1(id) = value
            End Set
        End Property
        Public Shared Property xtg(ByVal id As Int32) As Double
            Get
                Return xtg1(id)
            End Get
            Set(ByVal value As Double)
                xtg1(id) = value
            End Set
        End Property
        Public Shared Property yf(ByVal id As Int32) As Double
            Get
                Return yf1(id)
            End Get
            Set(ByVal value As Double)
                yf1(id) = value
            End Set
        End Property
        Public Shared Property yi(ByVal id As Int32) As Double
            Get
                Return yi1(id)
            End Get
            Set(ByVal value As Double)
                yi1(id) = value
            End Set
        End Property
        Public Shared Property yAuto(ByVal id As Int32) As Double
            Get
                Return yAuto1(id)
            End Get
            Set(ByVal value As Double)
                yAuto1(id) = value
            End Set
        End Property
        Public Shared Property idioma(ByVal nID As Int32, page As System.Web.UI.Page) As String
            Get
            'page.Session.Timeout = 120
            HttpContext.Current.Server.ScriptTimeout = 120
                If Idioma1(nID) Is Nothing Then
                    Idioma1(nID) = "en"
                End If
                Return LCase(Idioma1(nID))
            End Get
            Set(ByVal value As String)
            'page.Session.Timeout = 120
            HttpContext.Current.Server.ScriptTimeout = 120
                If value <> Idioma1(nID) Then
                    logFile(Nothing, "idioma=" + value, "", page, nID, "")
                    Idioma1(nID) = Trim(LCase(value))
                End If
            End Set
        End Property
        Public Shared vImgNames() As String = {
        "btnHome.png",
        "btnRoots.png",
        "btnPolyCalc.png",
        "btnGraph.png",
        "btnDownloads.png",
        "btnLinks.png",
        "btnComments.png",
        "btnHome2.png",
        "btnRoots2.png",
        "btnPolyCalc2.png",
        "btnGraph2.png",
        "btnDownloads2.png",
        "btnLinks2.png",
        "btnComments2.png"
        }
        Shared Sub initMenu(page As System.Web.UI.Page)
        'Try
        '    Dim vAltTitle() As String = { _
        '            "Home page", _
        '            "Root finder", _
        '            "Polynomial & Scientific Calculator", _
        '            "Graph", _
        '            "Downloads", _
        '            "Links", _
        '            "Comments & suggestions" _
        '        }
        '    Dim t As String = "<script language=""javascript"" type=""text/javascript"">" + vbCrLf
        '    For i As Int32 = 0 To Math.Floor(vImgNames.Length / 2) - 1
        '        t += "mnImg" + i.ToString + "=new Image();" + vbCrLf
        '        t += "mnImg" + i.ToString + ".src = ""/images/" + vImgNames(i)
        '        t += """;" + vbCrLf
        '        t += "mnImg" + i.ToString + ".alt =""" + vAltTitle(i) + """;" + vbCrLf
        '        t += "mnImg" + i.ToString + ".title =""" + vAltTitle(i) + """;" + vbCrLf
        '    Next
        '    Dim j As Int32 = 0
        '    For i As Int32 = Math.Floor(vImgNames.Length / 2) To vImgNames.Length - 1
        '        t += "mnImg" + j.ToString + "B=new Image();" + vbCrLf
        '        t += "mnImg" + j.ToString + "B.src = ""/images/" + vImgNames(i)
        '        t += "mnImg" + j.ToString + ".alt =""" + vAltTitle(j) + """;" + vbCrLf
        '        t += "mnImg" + j.ToString + ".title =""" + vAltTitle(j) + """;" + vbCrLf
        '        't += """;" + vbCrLf
        '        j += 1
        '    Next
        '    t += "</script>"
        '    page.ClientScript.RegisterClientScriptBlock( _
        '        GetType(String), "mnImages", t)
        'Catch ex As Exception
        '    Throw ex
        'End Try
    End Sub
        Public Shared Property tb3(ByVal id As Int32) As String
            Get
                Return tb31(id)
            End Get
            Set(ByVal value As String)
                tb31(id) = value
            End Set
        End Property
        Public Shared Property tb1(ByVal id As Int32) As String
            Get
                Return tb11(id)
            End Get
            Set(ByVal value As String)
                tb11(id) = value
            End Set
        End Property
        Public Shared Property enlace(ByVal id As Int32) As String
            Get
                Return enlace1(id)
            End Get
            Set(ByVal value As String)
                enlace1(id) = value
            End Set
        End Property
        Shared Sub SesidiomaBAD(ByRef language As Object, ByRef request As Object, ByVal nID As Int32)
            'Sesidioma(True, language, request, nID)
        End Sub
        Shared Sub SesIdiomaBAD(ByVal primeraVez As Boolean, ByRef Page As System.Web.UI.Page, ByVal nID As Int32)
            'Dim sID As String
            'Dim i As Int32
            'sID = "en"
            'Dim ln As String = Page.Request.Params("ln")
            'If Page.Session.IsNewSession Then
            '    Dim n As Int32 = nID
            '    enlace1(n) = "" : tb11(n) = "" : tb31(n) = "" : Idioma1(n) = ""
            '    referer(n) = ""
            '    sfn(n) = "" : sf2(n) = "" : fp0(n) = "" ' As String, lenVars(n) As Int32, usaGrafica(n), usaAproximacion(n) As Boolean
            '    iH(n) = 0 ':' HT(n)() As String
            '    strPtosLagr(n) = "" : strSolucPtosLagr(n) = "" ' As String
            'End If
            'If ln Is Nothing OrElse Len(ln) = 0 Then
            '    If Not Page Is Nothing Then
            '        If Page.Session.IsNewSession Then
            '            idioma(nID, Page) = "en"
            '        End If
            '    End If
            '    ln = idioma(nID, Page)
            'Else
            'End If
            'sID = ln
            'Dim idiomas() As String = Page.Request.UserLanguages()
            'For i = 0 To language.Items.Count - 1
            '    If language.Items(i).Value = sID Then
            '        language.SelectedIndex = i
            '        Exit For
            '    End If
            'Next
            'If i = language.Items.Count Then sID = "en"
            'For i = 0 To language.Items.Count - 1
            '    If language.Items(i).Value = sID Then
            '        language.SelectedIndex = i
            '    End If
            'Next
            'C_SessionWeb.idioma(nID, Page) = sID
            'If Page.Session.IsNewSession Then
            '    If InStr(LCase(Page.Request.Url.AbsoluteUri), "localhost") > 0 Then
            '        isLocalhost = True
            '    Else
            '        isLocalhost = False
            '    End If
            '    iH(nID) = 0
            'End If
        End Sub
        Public Shared Function getID(ByRef Page As System.Web.UI.Page) As Int32
            Dim nuevo As Boolean = False
            Dim nID As Int32 = -1, i As Int32
            Dim Identificador As Int32 = 0

        Dim rnd As New Random
        nID = rnd.Next(0, 9999)
        Dim e1 As String = nID ' Page.Session.SessionID
        Try
            'If Page.Session.IsNewSession = False Then
            '    For i = ultimoID To 1 Step -1
            '    If strListaID(i) = Page.Session.SessionID Then
            '        nID = i : Exit For
            '    End If
            'Next
            '    If nID = -1 Then
            For i = strListaID.Length - 1 To ultimoID + 1 Step -1
                If strListaID(i) = nID Then
                    nID = i : Exit For
                End If
            Next
            'End If
            'End If
        Catch ex As Exception
            logFile(ex, "GETID", "i=" + i.ToString, Page, nID, "MatesErrors.txt")
        End Try
        'If Page.Session.IsNewSession OrElse nID = -1 Then
        '    ultimoID += 1
        '    If ultimoID >= strListaID.Length Then
        '        ultimoID = 1
        '    End If
        '    strListaID(ultimoID) = Page.Session.SessionID
        '    nID = ultimoID
        'End If
        Return nID
    End Function
    Public Shared Function getHostName(ByVal strIP As String) As String
            Dim myIP As System.Net.IPAddress = System.Net.IPAddress.Parse(strIP)
            Dim hostInfo As System.Net.IPHostEntry =
        System.Net.Dns.GetHostEntry(myIP)
            Return hostInfo.HostName
        End Function
        Shared Function delEtiquetas(ByVal e2 As String) As String
            Dim e1 As String = Replace(e2, "<font color=""Navy"" size=""+1"">", "")
            e1 = Replace(e1, "</font>", "")
            e1 = Replace(e1, Chr(10), "")
            e1 = Replace(e1, Chr(13), "")
            e1 = Replace(e1, "<font color=""#00aa00"">", "")
            e1 = Replace(e1, "<font color=""red"">", "")
            e1 = Replace(e1, "<font color=""black"">", "")
            e1 = Replace(e1, "</font>", "")
            e1 = Replace(e1, "<sup>", "^")
            e1 = Replace(e1, "</sup>", "")
            'e1 = Replace(e1, ",", ".")
            Return e1
        End Function
        Shared Sub completarTablaHTML(ByRef e1 As String)
            Try
                completarTablaHTML(e1, "td")
                completarTablaHTML(e1, "tr")
                completarTablaHTML(e1, "table")
            Catch ex As Exception

            End Try
        End Sub
        Shared Sub completarTablaHTML(ByRef e1 As String, ByVal tagACompletar As String)
            'Dim pos1 As Int32 = 0
            'Do While InStr(LCase(e1), "<" + tagACompletar)
            '    pos1 = InStr(pos1 + 1, LCase(e1), "<" + tagACompletar)
            '    If pos1 = -1 Then
            '        Exit Do
            '    End If
            '    Dim pos2 As Int32 = InStr(pos1 + 1, LCase(e1), "/" + tagACompletar)
            '    If pos2 = -1 Then
            '        e1 += "</" + tagACompletar + ">"
            '        Exit Do
            '    End If
            'Loop
        End Sub
        Shared Sub logFile(ByVal ex As Exception, ByVal e1 As String, ByVal e2 As String,
                ByRef Page As System.Web.UI.Page, ByVal nID As Int32, ByVal nomFile As String)
            Try
                'autoLog.WaitOne()
                mtx.WaitOne()
                If InStr(e1, "10*x^4 -270*x^2 -140*x +1200") Then
                    ' omitimos el ejemplo de rootfinder.aspx:
                    Exit Try
                End If
                'Dim maxLen As String = 10000
                'If e1.Length > maxLen Then
                '    e1 = Mid(e1, 1, maxLen)
                '    completarTablaHTML(e1)
                'End If
                'If e2.Length > maxLen Then
                '    e2 = Mid(e2, 1, maxLen)
                '    completarTablaHTML(e2)
                'End If
                'e1 = Replace(e1, Chr(10), "")
                'e2 = Replace(e2, Chr(13), "")
                'e1 = Replace(e1, Chr(10), "")
                'e2 = Replace(e2, Chr(13), "")
                If Len(e1) Then
                    e1 = Replace(e1, vbCrLf, "<br />")
                    e1 = Replace(e1, vbCr, "<br />")
                    e1 = Replace(e1, vbLf, "<br />")
                    e1 = Replace(e1, "<br /><br />", "<br />")
                    e1 = delEtiquetas(e1)
                End If
                If Len(e2) Then
                    e2 = Replace(e2, vbCrLf, "<br />")
                    e2 = Replace(e2, vbCr, "<br />")
                    e2 = Replace(e2, vbLf, "<br />")
                    e2 = Replace(e2, "<br /><br />", "<br />")
                    'e2 = delEtiquetas(e2)
                End If
                Dim f As FileStream
            If (Page.Request.UrlReferrer IsNot Nothing AndAlso
            InStr(Page.Request.UrlReferrer.AbsoluteUri, "//localhost:") > 0) Then
                f = New FileStream(Page.MapPath(".") + "/LOCAL.txt", FileMode.Append)
            Else
                f = New FileStream(
                "C:\inetpub\wwwroot\txt\" + nomFile, FileMode.Append,
             FileAccess.Write)
                End If
                Dim dt As DateTime = Now
                Dim host As String = ""
                Try
                    host = getHostName(Page.Request.UserHostAddress)
                Catch ex1 As Exception

                End Try
                Dim sFch As String = Now.ToString("yyyy/MM/dd   hh:mm:ss")
                Dim fs As New StreamWriter(f, System.Text.Encoding.Unicode)
                fs.WriteLine(Page.Request.UserHostAddress + Chr(9) _
            + host + Chr(9) + sFch +
            Chr(9) + dt.ToShortTimeString() + Chr(9) + e1 + Chr(9) + e2)
                If Not (ex Is Nothing) Then
                    'Dim e3 As String = ""
                    'If haydsp Then e3 = TextBox1.Text
                    fs.WriteLine(Page.Request.UserHostAddress + Chr(9) _
                + host + Chr(9) + Now().ToShortDateString() +
                Chr(9) + dt.ToShortTimeString() + Chr(9) +
                ex.Message)
                End If
                Try
                    fs.Close()
                    f.Close()
                Catch ex2 As Exception

                End Try
            Catch ex3 As Exception
            Finally
                'autoLog.Set()
                mtx.ReleaseMutex()
            End Try
        End Sub
        Public Shared Sub contador(ByRef Page As System.Web.UI.Page)
        Try
            If Not Page.IsPostBack Then
                Try
                    If Page.Request.UserHostAddress.ToString = "2.152.181.185" Then
                        Exit Sub
                    ElseIf Page.Request.Browser.Crawler OrElse
                    Regex.IsMatch(Page.Request.UserAgent, "bot|crawl|spider|scan|census", RegexOptions.IgnoreCase) Then
                        Exit Sub
                    End If
                Catch ex As Exception

                End Try
                Dim host As String = ""
                Try
                    host = getHostName(Page.Request.UserHostAddress)
                    If Regex.IsMatch(host, "bot|crawl|spider|scan|census", RegexOptions.IgnoreCase) Then
                        Exit Sub
                    End If
                Catch ex1 As Exception

                End Try
                Dim fs As New FileStream(
                        Page.Server.MapPath("~\txt\calculadora.txt"), FileMode.Append)
                Dim sw As New StreamWriter(fs, UnicodeEncoding.Unicode)
                'For i = 0 To Page.Request.ServerVariables.Count - 1
                '    sw.WriteLine(Page.Request.ServerVariables.Keys(i).ToString + _
                '        "=" + Page.Request.ServerVariables.Item(i).ToString)
                'Next

                sw.Write(Page.Request.UserHostAddress.ToString + vbTab)

                sw.Write(host + vbTab)

                Dim sFch As String = Now.ToString("yyyy/MM/dd   hh:mm:ss")
                sw.Write(sFch + vbTab)

                Dim dt As DateTime = Now
                sw.Write(dt.ToShortTimeString + vbTab)

                If Page.Request.UserLanguages Is Nothing Then
                    sw.Write(" " + vbTab)
                Else
                    sw.Write(Join(Page.Request.UserLanguages, " ") + vbTab)
                End If
                If Page.Request.UrlReferrer Is Nothing Then
                    sw.Write(" " + vbTab)
                Else
                    sw.Write(Page.Request.UrlReferrer.ToString + vbTab)
                End If

                If Page.Request.Url Is Nothing Then
                    sw.Write(" " + vbTab)
                Else
                    sw.Write(URLDecode(Page.Request.Url.OriginalString) + vbTab)
                End If

                If Page.Request.UserAgent Is Nothing Then
                    sw.Write(" " + vbTab)
                Else
                    sw.Write(Page.Request.UserAgent + vbTab)
                End If
                If Page.Request.Url Is Nothing Then
                    sw.Write(" " + vbTab)
                Else
                    sw.Write(Page.Request.Url.AbsolutePath)
                End If
                sw.WriteLine("")
                sw.Close()
                fs.Close()
            End If
        Catch ex1 As Exception

        End Try
    End Sub
        Public Shared t, t2 As String
        Public Delegate Sub delegContador(ByVal page As UI.Page,
                                ByVal Request As HttpRequest,
                               ByVal Response As HttpResponse)

        Public Shared Sub contador(ByVal page As UI.Page,
                                ByVal Request As HttpRequest,
                               ByVal Response As HttpResponse)
            Try
                ' 0)
                contador(page)
                ' 1) la siguiente llamada:
                'contador2(page, Request, Response)

                ' ó 2) el sig. código:
                'Dim d As New delegContador(AddressOf contador2)
                'Dim ar As IAsyncResult = d.BeginInvoke( _
                '    page, Request, Response, Nothing, Nothing)
                'Dim ts As New TimeSpan(Now.Ticks)
                'Dim ts2 As New TimeSpan(Now.Ticks - ts.Ticks)
                'Do While Not ar.IsCompleted AndAlso _
                'ts2.TotalMilliseconds < 30000
                '    System.Threading.Thread.Sleep(200)
                '    ts2 = New TimeSpan(Now.Ticks - ts.Ticks)
                'Loop
                'd.EndInvoke(ar)
            Catch ex As Exception

            End Try
        End Sub
        Public Shared Sub contador2(ByVal page As UI.Page,
                                ByVal Request As HttpRequest,
                               ByVal Response As HttpResponse)
            Try
                If page.IsPostBack = False Then
                    'Exit Sub
                End If
                Dim query, url, s, c, r, u, ip, ua, ln, r1, sb, v As String
                r = "" : ip = "" : ln = "" : ua = ""
                Try
                    r = Request.ServerVariables("HTTP_REFERER")
                Catch ex As Exception
                End Try
                'Try
                '    If Request.UrlReferrer IsNot Nothing AndAlso _
                '    Request.UrlReferrer.AbsoluteUri IsNot Nothing AndAlso _
                '    Request.UrlReferrer.AbsoluteUri > Len(r) Then
                '        r = Request.UrlReferrer.AbsoluteUri
                '    End If
                'Catch ex As Exception

                'End Try
                Try
                    ip = Request.ServerVariables("REMOTE_ADDR")
                Catch ex As Exception
                End Try
                If ip = "" Then
                    Try
                        ip = Request.UserHostAddress
                    Catch ex As Exception

                    End Try
                End If
                Try
                    ua = Request.ServerVariables("HTTP_USER_AGENT")
                Catch ex As Exception
                End Try
                Try
                    ln = Request.ServerVariables("HTTP_ACCEPT_LANGUAGE")
                Catch ex As Exception
                End Try
                Try
                    u = LCase(getPageName(Request)) ' nombre de la página
                Catch ex As Exception
                    u = ""
                End Try
                Try
                    If u IsNot Nothing AndAlso
                Len(u) = 0 Then
                        u = LCase(page.AppRelativeVirtualPath)
                        u = Replace(u, "\", "/")
                        If InStr(u, "/") Then
                            Dim vU() As String = Split(u, "/")
                            u = vU(vU.Length - 1)
                        End If
                    End If
                Catch ex As Exception

                End Try
                s = "0"
                c = "0"
                v = "0"
                r1 = r
                If t Is Nothing OrElse t = "" OrElse
            t2 Is Nothing OrElse t2 = "" Then
                    t = "0"
                    t2 = "0"
                End If
                sb = u ' subpágina

                query = "s=" & s & "&ip=" & ip & "&c=" & c & "&u=" & u & "&ln=" & ln &
             "&r=" & r1 & "&ua=" & ua & "&sb=" & sb & "&t=" & t & "&t2=" & t2 & "&v=" & v

                Dim sDom As String = "https://xrjunque.nom.es"
                If Request.UrlReferrer IsNot Nothing AndAlso
            Request.UrlReferrer.Host IsNot Nothing Then
                    query += "&refHost=" + Request.UrlReferrer.Host
                Else
                    query += "&refHost=" + sDom
                End If
                If Request.UrlReferrer IsNot Nothing AndAlso
            Request.UrlReferrer.AbsoluteUri IsNot Nothing Then
                    query += "&refAbsUri=" + Request.UrlReferrer.AbsoluteUri
                Else
                    'query += "&refAbsUri=" + sDom + Replace( _
                    'Request.FilePath, "\", "/")  '+ page.AppRelativeVirtualPath
                    query += "&refAbsUri=" + sDom + Replace(Replace(
                    page.AppRelativeVirtualPath, "\", "/"), "~", "")
                End If

                url = "https://xrjunque.nom.es/contador/conta1.aspx?" & query

                'url = "https://localhost:64842/conta1.aspx?" & query ' s=&ip=127.0.0.1&c=32&u=https://localhost/mates/RootFinder.aspx&ln=en-us&r=&ua=Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; InfoPath.1; .NET CLR 1.1.4322)&sb=default&t=633054938628130000&t2=633054938642973750&v=5071"


                'Response.Clear()
                Dim req As WebRequest = WebRequest.Create(url)
                Dim wr As WebResponse = req.GetResponse
                Dim sr As IO.Stream = wr.GetResponseStream
                Dim b(wr.ContentLength - 1) As Byte
                sr.Read(b, 0, b.Length)
                sr.Close()
                wr.Close()
                'Response.Write(System.Text.ASCIIEncoding.ASCII.GetString(b))
            Catch ex As Exception
                Dim s1 As String = ex.ToString
            End Try
        End Sub
        Public Shared Sub contador3(ByVal page As UI.Page,
                                ByVal Request As HttpRequest,
                               ByVal Response As HttpResponse, sb As String, u As String)
            Try
                If page.IsPostBack = False Then
                    'Exit Sub
                End If
                Dim query, url, s, c, r, ip, ua, ln, r1, v As String
                r = "" : ip = "" : ln = "" : ua = ""
                Try
                    r = Request.ServerVariables("HTTP_REFERER")
                Catch ex As Exception
                End Try
                'Try
                '    If Request.UrlReferrer IsNot Nothing AndAlso _
                '    Request.UrlReferrer.AbsoluteUri IsNot Nothing AndAlso _
                '    Request.UrlReferrer.AbsoluteUri > Len(r) Then
                '        r = Request.UrlReferrer.AbsoluteUri
                '    End If
                'Catch ex As Exception

                'End Try
                Try
                    ip = Request.ServerVariables("REMOTE_ADDR")
                Catch ex As Exception
                End Try
                If ip = "" Then
                    Try
                        ip = Request.UserHostAddress
                    Catch ex As Exception

                    End Try
                End If
                Try
                    ua = Request.ServerVariables("HTTP_USER_AGENT")
                Catch ex As Exception
                End Try
                Try
                    ln = Request.ServerVariables("HTTP_ACCEPT_LANGUAGE")
                Catch ex As Exception
                End Try
                'Try
                '    u = LCase(getPageName(Request)) ' nombre de la página
                'Catch ex As Exception
                '    u = ""
                'End Try
                'Try
                '    If u IsNot Nothing AndAlso _
                '    Len(u) = 0 Then
                '        u = LCase(page.AppRelativeVirtualPath)
                '        u = Replace(u, "\", "/")
                '        If InStr(u, "/") Then
                '            Dim vU() As String = Split(u, "/")
                '            u = vU(vU.Length - 1)
                '        End If
                '    End If
                'Catch ex As Exception

                'End Try
                s = "0"
                c = "0"
                v = "0"
                r1 = r
                If t Is Nothing OrElse t = "" OrElse
            t2 Is Nothing OrElse t2 = "" Then
                    t = "0"
                    t2 = "0"
                End If
                sb = u ' subpágina

                query = "s=" & s & "&ip=" & ip & "&c=" & c & "&u=" & u & "&ln=" & ln &
             "&r=" & r1 & "&ua=" & ua & "&sb=" & sb & "&t=" & t & "&t2=" & t2 & "&v=" & v

                Dim sDom As String = "https://xrjunque.nom.es"
                If Request.UrlReferrer IsNot Nothing AndAlso
            Request.UrlReferrer.Host IsNot Nothing Then
                    query += "&refHost=" + Request.UrlReferrer.Host
                Else
                    query += "&refHost=" + sDom
                End If
                If Request.UrlReferrer IsNot Nothing AndAlso
            Request.UrlReferrer.AbsoluteUri IsNot Nothing Then
                    query += "&refAbsUri=" + Request.UrlReferrer.AbsoluteUri
                Else
                    'query += "&refAbsUri=" + sDom + Replace( _
                    'Request.FilePath, "\", "/")  '+ page.AppRelativeVirtualPath
                    query += "&refAbsUri=" + sDom + Replace(Replace(
                    page.AppRelativeVirtualPath, "\", "/"), "~", "")
                End If

                url = "https://xrjunque.nom.es/contador/conta1.aspx?" & query

                'url = "https://localhost:64842/conta1.aspx?" & query ' s=&ip=127.0.0.1&c=32&u=https://localhost/mates/RootFinder.aspx&ln=en-us&r=&ua=Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; InfoPath.1; .NET CLR 1.1.4322)&sb=default&t=633054938628130000&t2=633054938642973750&v=5071"


                'Response.Clear()
                Dim req As WebRequest = WebRequest.Create(url)
                Dim wr As WebResponse = req.GetResponse
                Dim sr As IO.Stream = wr.GetResponseStream
                Dim b(wr.ContentLength - 1) As Byte
                sr.Read(b, 0, b.Length)
                sr.Close()
                wr.Close()
                'Response.Write(System.Text.ASCIIEncoding.ASCII.GetString(b))
            Catch ex As Exception
                Dim s1 As String = ex.ToString
            End Try
        End Sub


        Public Shared Function URLDecode(ByVal strToDecode As String) As String
            Dim strin, strOut, strLeft, strRight As String, intPos As Int32
            strin = strToDecode
            strOut = ""
            intPos = InStr(strin, "+") ' look for + and replace with space
            Do While intPos
                strLeft = ""
                strRight = ""
                If intPos > 1 Then strLeft = Left(strin, intPos - 1)
                If intPos < Len(strin) Then strRight = Mid(strin, intPos + 1)
                strin = strLeft & " " & strRight
                intPos = InStr(strin, "+") ' and then look for next onie
            Loop
            intPos = InStr(strin, "%") ' look for ASCII coded characters
            Do While intPos
                If intPos > 1 Then strOut = strOut & Left(strin, intPos - 1)
                strOut = strOut & Chr(CInt("&H" & Mid(strin, intPos + 1, 2)))
                If intPos > Len(strin) - 3 Then
                    strin = ""
                Else
                    strin = Mid(strin, intPos + 3)
                End If
                intPos = InStr(strin, "%") ' and then look for next one
            Loop
            Return strOut & strin
        End Function
        Public Shared Function getPageName(ByVal Request As HttpRequest) As String
            Dim e1 As String = ""
            Try
                Dim sPath As String = Request.Url.AbsolutePath
                Dim fi As New IO.FileInfo(sPath)
                e1 = fi.Name
            Catch ex As Exception
                e1 = "err"
            End Try
            Return e1
        End Function


    End Class
